
# Healdsburg SOH, Section 1.2: Bedroom Mix by Tenure -----------------------
#
# Pulls owner- and renter-occupied homes by number of bedrooms for the City
# of Healdsburg (incorporated city, Census place 0633056) from ACS Table
# B25042 and charts them in the style of SOH 2023 Figure 7: owners to the
# left, renters to the right. Covers occupied homes only.
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

vars_acs5 <- load_variables(year = year_acs5, dataset = 'acs5', cache = TRUE)

raw_bedrooms <- get_acs(
  geography = 'place',
  table = 'B25042',
  state = 'CA',
  year = year_acs5,
  survey = 'acs5'
)


# Clean -------------------------------------------------------------------

tidy_bedrooms <- left_join(raw_bedrooms, vars_acs5, by = c('variable' = 'name')) %>%
  clean_names(case = 'snake') %>%
  filter(geoid == healdsburg_geoid) %>%
  mutate(level = str_count(label, '!!'),            # 3 = a bedroom row
         tenure = case_when(str_detect(label, 'Owner')  ~ 'Owner-Occupied',
                            str_detect(label, 'Renter') ~ 'Renter-Occupied'),
         bedrooms = str_extract(label, '[^!]+$')) %>%
  filter(level == 3) %>%
  mutate(bedrooms = case_when(bedrooms == 'No bedroom'         ~ 'Studio',
                              bedrooms == '1 bedroom'          ~ '1 Bedroom',
                              bedrooms == '5 or more bedrooms' ~ '5+ Bedrooms',
                              TRUE ~ str_to_title(bedrooms))) %>%
  group_by(tenure) %>%
  mutate(tenure_total = sum(estimate),
         prop = estimate / tenure_total) %>%            # share of owners / renters
  ungroup() %>%
  mutate(cv = (moe / 1.645) / estimate,
         bedrooms = factor(bedrooms, levels = c('Studio', '1 Bedroom', '2 Bedrooms',
                                                '3 Bedrooms', '4 Bedrooms', '5+ Bedrooms')),
         tenure = factor(tenure, levels = c('Owner-Occupied', 'Renter-Occupied'))) %>%
  arrange(tenure, bedrooms) %>%
  select(tenure, bedrooms, estimate, moe, cv, prop, tenure_total)

if (any(is.na(tidy_bedrooms$bedrooms))) stop('New B25042 bedroom category to map.')

tidy_bedrooms

write_csv(tidy_bedrooms, file.path(dir_outputs, paste0('1.2d_bedroom-mix_acs', year_acs5, '.csv')))


# Plot --------------------------------------------------------------------

# owners plotted as negative numbers so they extend left of zero
data_bed_plot <- tidy_bedrooms %>%
  mutate(x = if_else(tenure == 'Owner-Occupied', -estimate, estimate),
         label = comma(estimate))

max_bed <- max(tidy_bedrooms$estimate)
lim_bed <- max_bed * 1.25                       # room for labels on both sides
breaks_bed <- breaks_pretty(n = 3)(c(0, max_bed))
breaks_bed <- sort(unique(c(-breaks_bed, breaks_bed)))

ggplot(data_bed_plot, aes(x = x, y = fct_rev(bedrooms), fill = tenure)) +
  geom_col(width = 0.75) +
  geom_vline(xintercept = 0, linewidth = 0.4, color = 'gray40') +
  geom_text(aes(label = label,
                hjust = if_else(tenure == 'Owner-Occupied', 1, 0)),
            nudge_x = if_else(data_bed_plot$tenure == 'Owner-Occupied', -0.02, 0.02) * max_bed,
            size = 8/.pt,
            color = 'gray0',
            family = font_family) +
  labs(title = 'Bedrooms by Tenure Type in Healdsburg',
       x = 'Occupied Housing Units (owners left, renters right)',
       y = 'Bedrooms',
       fill = NULL,
       caption = str_wrap(paste0(
         'Source: U.S. Census Bureau, American Community Survey ', acs_period,
         ' 5-Year Estimates, Table B25042. Occupied homes only.'), width = 125)) +
  scale_x_continuous(breaks = breaks_bed,
                     labels = function(x) comma(abs(x))) +
  scale_fill_manual(values = colors_tenure) +
  coord_cartesian(xlim = c(-lim_bed, lim_bed)) +
  theme(axis.ticks.x = element_line(color = 'gray70'),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'gray85'))

save_chart(paste0('1.2d_bedroom-mix_acs', year_acs5))
