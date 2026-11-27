
# Healdsburg SOH, Section 1.2: Median Rooms by Tenure ----------------------
#
# Pulls the median number of rooms for all occupied homes, owner-occupied
# homes and renter-occupied homes in the City of Healdsburg (incorporated
# city, Census place 0633056) from ACS Table B25021, and charts them.
# Rooms count bedrooms, living rooms, kitchens etc.; not bathrooms or halls.
#
# To update to the 2021-2025 release, change year_acs5 below to 2025.

# Settings ----------------------------------------------------------------

year_acs5 <- 2024                     # 2024 = ACS 2020-2024 5-year estimates

healdsburg_geoid <- '0633056'         # Healdsburg city, CA
dir_outputs      <- 'outputs/1.2_tenure-and-housing-type'

# loads Sofia Pro (fonts.R in the project folder); charts use the default font without it
path_fonts <- 'fonts.R'


# Packages ----------------------------------------------------------------

library(tidycensus)  # query census api (needs your Census API key)
library(tidyverse)
library(janitor)     # clean column names easily
library(scales)      # labels for ggplot

dir.create(dir_outputs, recursive = TRUE, showWarnings = FALSE)

acs_period <- paste0(year_acs5 - 4, '-', year_acs5)


# Chart Theme (from SOH 2023 set-up) --------------------------------------

font_family <- 'Sofia Pro'
if (file.exists(path_fonts)) source(path_fonts) else font_family <- 'sans'

theme_set(
  theme_bw() +
    theme(
      text = element_text(family = font_family, color = 'gray0', size = 9),
      plot.title = element_text(face = 'bold', hjust = 0.5, size = 11),
      plot.title.position = 'plot',
      plot.caption = element_text(size = 7, color = 'gray40', hjust = 0),
      plot.caption.position = 'plot',
      axis.title = element_text(size = 9, color = 'gray0'),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      axis.ticks.x = element_blank(),
      axis.ticks.y = element_line(linewidth = 0.4, color = 'gray70'),
      axis.text = element_text(color = 'gray0', size = 9),
      panel.border = element_blank(),
      panel.grid = element_blank(),
      legend.background = element_blank(),
      legend.position = 'bottom',
      legend.direction = 'horizontal',
      legend.margin = margin(),
      legend.text = element_text(size = 9, margin = margin(0, 10, 0, 5)),
      legend.spacing.x = unit(0, 'cm')
    )
)

colors_tenure <- c('Owner-Occupied' = '#0d3353', 'Renter-Occupied' = '#a1cc3a')

# save a chart as a 6 x 4 in PDF (like SOH 2023) and a PNG for quick review
save_chart <- function(name, width = 6, height = 4) {
  ggsave(file.path(dir_outputs, paste0(name, '.pdf')), width = width, height = height, units = 'in')
  ggsave(file.path(dir_outputs, paste0(name, '.png')), width = width, height = height, units = 'in', dpi = 300)
}


# Query -------------------------------------------------------------------

raw_rooms <- get_acs(
  geography = 'place',
  variables = c('Owner-Occupied'  = 'B25021_002',
                'Renter-Occupied' = 'B25021_003'),
  state = 'CA',
  year = year_acs5,
  survey = 'acs5'
)


# Clean -------------------------------------------------------------------

tidy_rooms <- raw_rooms %>%
  clean_names(case = 'snake') %>%
  filter(geoid == healdsburg_geoid) %>%
  rename(tenure = variable) %>%
  mutate(low  = estimate - moe,
         high = estimate + moe,
         tenure = factor(tenure, levels = c('Owner-Occupied', 'Renter-Occupied'))) %>%
  arrange(tenure) %>%
  select(tenure, estimate, moe, low, high)

tidy_rooms

write_csv(tidy_rooms, file.path(dir_outputs, paste0('1.2c_median-rooms_acs', year_acs5, '.csv')))


# Plot --------------------------------------------------------------------

# Pictogram: one bed = one room. Each row gets whole beds for the whole
# rooms plus a partly filled bed for the decimal (5.8 = 5 full beds + 80%).

# one bed icon, drawn from rectangles in a 1 x 1 box (x = 0 is the headboard)
bed_parts <- tribble(
  ~part,        ~xmin, ~xmax, ~ymin, ~ymax,
  'headboard',   0.00,  0.09,  0.00,  0.62,
  'frame',       0.09,  0.86,  0.14,  0.32,
  'pillow',      0.13,  0.33,  0.34,  0.46,
  'blanket',     0.36,  0.86,  0.34,  0.42,
  'footboard',   0.82,  0.90,  0.00,  0.40
)
bed_width <- max(bed_parts$xmax)
bed_gap   <- 1.15                               # spacing between bed icons
row_gap   <- 1.1                                # spacing between owner and renter rows

n_beds <- ceiling(max(tidy_rooms$estimate))

beds <- tidy_rooms %>%
  mutate(row = (n_distinct(tenure) - as.integer(tenure)) * row_gap) %>%   # owners on top
  select(tenure, estimate, row) %>%
  crossing(slot = seq_len(n_beds)) %>%
  mutate(x0   = (slot - 1) * bed_gap,
         fill_share = pmin(pmax(estimate - (slot - 1), 0), 1)) %>%        # 1 = full bed, 0.8 = 80%
  crossing(bed_parts) %>%
  mutate(xmin = xmin + x0, xmax = xmax + x0,
         ymin = ymin + row, ymax = ymax + row,
         cut  = x0 + fill_share * bed_width)

# gray outline beds only up to the partial bed in each row
beds_ghost  <- beds %>% filter(slot <= ceiling(estimate))
beds_filled <- beds %>%
  filter(fill_share > 0, xmin < cut) %>%
  mutate(xmax = pmin(xmax, cut))

labels_rooms <- tidy_rooms %>%
  mutate(row = (n_distinct(tenure) - as.integer(tenure)) * row_gap,
         tenure_label = if_else(tenure == 'Owner-Occupied', 'Owners', 'Renters'),
         value_label  = paste0(number(estimate, accuracy = 0.1), ' rooms'))

x_end <- (n_beds - 1) * bed_gap + bed_width

ggplot() +
  geom_rect(data = beds_ghost,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
            fill = 'gray90') +
  geom_rect(data = beds_filled,
            aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = tenure)) +
  geom_text(data = labels_rooms,
            aes(x = -0.25, y = row + 0.31, label = tenure_label),
            hjust = 1, size = 10/.pt, fontface = 'bold', color = 'gray0', family = font_family) +
  geom_text(data = labels_rooms,
            aes(x = x_end + 0.3, y = row + 0.31, label = value_label, color = tenure),
            hjust = 0, size = 10/.pt, fontface = 'bold', family = font_family) +
  labs(title = 'Median Number of Rooms by Tenure in Healdsburg',
       x = NULL, y = NULL,
       caption = str_wrap(paste0(
         'Each bed represents one room. Rooms include bedrooms, living rooms and kitchens, not ',
         'bathrooms or hallways. Source: U.S. Census Bureau, American Community Survey ', acs_period,
         ' 5-Year Estimates, Table B25021. Occupied homes only.'), width = 125)) +
  scale_fill_manual(values = colors_tenure, guide = 'none') +
  scale_color_manual(values = colors_tenure, guide = 'none') +
  coord_fixed(xlim = c(-1.6, x_end + 1.9), ylim = c(-0.3, row_gap + 0.9), expand = FALSE) +
  theme(panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.ticks.y = element_blank())

save_chart(paste0('1.2c_median-rooms_acs', year_acs5), height = 3)   # shorter: the beds don't need 4 in
