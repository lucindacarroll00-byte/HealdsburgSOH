
# Healdsburg SOH, Section 1.2: Tenure by Building Type ---------------------
#
# Pulls owner- and renter-occupied homes by building type for the City of
# Healdsburg (incorporated city, Census place 0633056) from ACS Table B25032
# and charts them. Building types are grouped as in 1.1 (2_units-by-type.R).
# Covers occupied homes only.
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

raw_tenure_type <- get_acs(
  geography = 'place',
  table = 'B25032',
  state = 'CA',
  year = year_acs5,
  survey = 'acs5'
)


# Clean -------------------------------------------------------------------

full_tenure_type <- left_join(raw_tenure_type, vars_acs5, by = c('variable' = 'name')) %>%
  clean_names(case = 'snake') %>%
  filter(geoid == healdsburg_geoid) %>%
  mutate(level = str_count(label, '!!'),            # 3 = a building-type row
         tenure = case_when(str_detect(label, 'Owner')  ~ 'Owner-Occupied',
                            str_detect(label, 'Renter') ~ 'Renter-Occupied'),
         units_raw = str_extract(label, '[^!]+$')) %>%
  filter(level == 3) %>%
  mutate(type = case_when(
    units_raw == '1, detached'                  ~ 'Single-Family Detached',
    units_raw == '1, attached'                  ~ 'Single-Family Attached',
    units_raw %in% c('2', '3 or 4')             ~ '2-4 Units',
    units_raw %in% c('5 to 9', '10 to 19')      ~ '5-19 Units',
    units_raw %in% c('20 to 49', '50 or more')  ~ '20+ Units',
    units_raw == 'Mobile home'                  ~ 'Mobile Home',
    units_raw == 'Boat, RV, van, etc.'          ~ 'Boat, RV, Van, etc.',
    TRUE                                        ~ NA_character_
  ))

# stop if the Census renames a category in a future release
if (any(is.na(full_tenure_type$type))) {
  stop('New B25032 category to map: ', paste(unique(full_tenure_type$units_raw[is.na(full_tenure_type$type)]), collapse = ', '))
}

tidy_tenure_type <- full_tenure_type %>%
  group_by(type, tenure) %>%
  summarize(moe = moe_sum(moe, estimate),
            estimate = sum(estimate),
            .groups = 'drop') %>%
  group_by(type) %>%
  mutate(type_total = sum(estimate),
         type_prop  = estimate / type_total) %>%        # owner/renter share of each building type
  group_by(tenure) %>%
  mutate(tenure_total = sum(estimate),
         tenure_prop  = estimate / tenure_total) %>%    # building-type share of all owners / renters
  ungroup() %>%
  mutate(cv = (moe / 1.645) / estimate,
         type = factor(type,
                       levels = c('Single-Family Detached',
                                  'Single-Family Attached',
                                  '2-4 Units',
                                  '5-19 Units',
                                  '20+ Units',
                                  'Mobile Home',
                                  'Boat, RV, Van, etc.')),
         tenure = factor(tenure, levels = c('Owner-Occupied', 'Renter-Occupied'))) %>%
  filter(!(type == 'Boat, RV, Van, etc.' & type_total == 0)) %>%   # drop if none
  arrange(type, tenure)

tidy_tenure_type

write_csv(tidy_tenure_type, file.path(dir_outputs, paste0('1.2b_tenure-by-building-type_acs', year_acs5, '.csv')))


# Plot --------------------------------------------------------------------

# Each bar is one building type split into owners and renters, as shares of
# that type, so small building types are as readable as single-family homes.
# The number of occupied homes of each type sits on top.
# ggplot stacks owners on top, so renters sit at the bottom of each bar.
data_type_plot <- tidy_tenure_type %>%
  filter(type_total > 0) %>%
  mutate(y_mid  = if_else(tenure == 'Renter-Occupied', type_prop / 2, 1 - type_prop / 2),
         inside = type_prop >= 0.08)

ggplot(data_type_plot, aes(x = type, y = type_prop, fill = tenure)) +
  geom_col(position = 'stack',
           width = 0.75) +
  geom_text(data = filter(data_type_plot, inside),
            aes(y = y_mid, label = percent(type_prop, accuracy = 1)),
            color = '#ffffff',
            size = 9/.pt,
            family = font_family) +
  geom_text(data = distinct(data_type_plot, type, type_total),
            aes(x = type, y = 1, label = paste0(comma(type_total), ' homes')),
            inherit.aes = FALSE,
            vjust = -0.7,
            size = 8/.pt,
            color = 'gray0',
            family = font_family) +
  labs(title = 'Tenure by Housing Type in Healdsburg',
       x = 'Building Type',
       y = 'Share of Occupied Homes',
       fill = NULL,
       caption = str_wrap(paste0(
         'Source: U.S. Census Bureau, American Community Survey ', acs_period,
         ' 5-Year Estimates, Table B25032. Occupied homes only. Each bar shows the owner/renter ',
         'split within that building type; the number above is how many occupied homes of that ',
         'type there are.'), width = 110)) +
  scale_x_discrete(labels = label_wrap(12)) +
  scale_y_continuous(labels = label_percent(),
                     breaks = seq(0, 1, 0.25),
                     expand = expansion(mult = c(0.0019, 0.0019))) +
  scale_fill_manual(values = colors_tenure) +
  coord_cartesian(ylim = c(0, 1.08), clip = 'off') +
  theme(axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.4, color = 'gray70'))

save_chart(paste0('1.2b_tenure-by-building-type_acs', year_acs5))
