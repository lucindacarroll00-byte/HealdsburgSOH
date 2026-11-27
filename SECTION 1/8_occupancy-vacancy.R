
# Healdsburg SOH, Section 1.2: Occupancy, Vacancy and Second Homes ---------
#
# For the City of Healdsburg (incorporated city, Census place 0633056):
#   B25002  occupied vs. vacant homes
#   B25004  vacant homes by reason, including second homes ("for seasonal,
#           recreational, or occasional use")
#   DP04    homeowner and rental vacancy rates, as calculated by the Census
# Prints the key numbers for the text and charts vacant homes by reason.
#
# Healdsburg's vacancy counts are small, so margins of error are large;
# they're in the CSV. Absorption isn't in the ACS (the report uses Census
# SOMA, outside this script).
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

raw_occupancy <- get_acs(
  geography = 'place',
  variables = c('total'    = 'B25002_001',
                'occupied' = 'B25002_002',
                'vacant'   = 'B25002_003'),
  state = 'CA',
  year = year_acs5,
  survey = 'acs5'
)

raw_vacancy <- get_acs(
  geography = 'place',
  variables = c('For Rent'                                  = 'B25004_002',
                'Rented, Not Occupied'                      = 'B25004_003',
                'For Sale Only'                             = 'B25004_004',
                'Sold, Not Occupied'                        = 'B25004_005',
                'Second Homes (Seasonal, Recreational, or Occasional Use)' = 'B25004_006',
                'For Migrant Workers'                       = 'B25004_007',
                'Other Vacant'                              = 'B25004_008'),
  state = 'CA',
  year = year_acs5,
  summary_var = 'B25004_001',
  survey = 'acs5'
)

# vacancy rates from the Data Profile (in percent)
raw_rates <- get_acs(
  geography = 'place',
  variables = c('homeowner_vacancy_rate' = 'DP04_0004',
                'rental_vacancy_rate'    = 'DP04_0005'),
  state = 'CA',
  year = year_acs5,
  survey = 'acs5'
)


# Clean -------------------------------------------------------------------

tidy_occupancy <- raw_occupancy %>%
  clean_names(case = 'snake') %>%
  filter(geoid == healdsburg_geoid) %>%
  select(variable, estimate, moe)

total_units <- tidy_occupancy$estimate[tidy_occupancy$variable == 'total']
total_moe   <- tidy_occupancy$moe[tidy_occupancy$variable == 'total']

tidy_vacancy <- raw_vacancy %>%
  clean_names(case = 'snake') %>%
  filter(geoid == healdsburg_geoid) %>%
  rename(reason = variable) %>%
  mutate(units_total     = total_units,          # all homes, for shares of all homes
         units_total_moe = total_moe,
         prop_of_vacant  = estimate / summary_est,
         prop_of_all     = estimate / units_total,
         prop_all_moe    = moe_prop(estimate, units_total, moe, units_total_moe),
         cv             = (moe / 1.645) / estimate,
         second_home    = str_detect(reason, 'Second Homes')) %>%
  select(reason, estimate, moe, cv, prop_of_vacant, prop_of_all, prop_all_moe,
         second_home, vacant_total = summary_est)

tidy_rates <- raw_rates %>%
  clean_names(case = 'snake') %>%
  filter(geoid == healdsburg_geoid) %>%
  select(variable, estimate, moe)

tidy_vacancy
tidy_rates

write_csv(bind_rows(tidy_occupancy, tidy_rates),
          file.path(dir_outputs, paste0('1.2e_occupancy-and-vacancy-rates_acs', year_acs5, '.csv')))
write_csv(tidy_vacancy,
          file.path(dir_outputs, paste0('1.2e_vacant-homes-by-reason_acs', year_acs5, '.csv')))


# Numbers for the text ----------------------------------------------------

occ     <- tidy_occupancy %>% select(variable, estimate) %>% deframe()   # lookup by name
occ_moe <- tidy_occupancy %>% select(variable, moe) %>% deframe()
second  <- filter(tidy_vacancy, second_home)
rate   <- function(v) tidy_rates$estimate[tidy_rates$variable == v]
rate_m <- function(v) tidy_rates$moe[tidy_rates$variable == v]

cat(paste0(
  '\nHealdsburg (city), ACS ', acs_period, ' 5-year estimates:\n',
  '  Total homes: ', comma(total_units), '\n',
  '  Occupied: ', comma(occ[['occupied']]), '  |  Vacant: ', comma(occ[['vacant']]),
  ' (+/- ', comma(occ_moe[['vacant']]), '), ', percent(occ[['vacant']] / total_units, accuracy = 0.1), ' of homes\n',
  '  Second homes: ', comma(second$estimate), ' (+/- ', comma(second$moe), '), ',
  percent(second$prop_of_vacant, accuracy = 1), ' of vacant homes and ',
  percent(second$prop_of_all, accuracy = 0.1), ' of all homes\n',
  '  Homeowner vacancy rate: ', rate('homeowner_vacancy_rate'), '% (+/- ', rate_m('homeowner_vacancy_rate'), ')\n',
  '  Rental vacancy rate: ', rate('rental_vacancy_rate'), '% (+/- ', rate_m('rental_vacancy_rate'), ')\n'))


# Plot --------------------------------------------------------------------

data_vac_plot <- tidy_vacancy %>%
  filter(estimate > 0) %>%                        # reasons with no homes are in the CSV
  mutate(reason = fct_reorder(str_wrap(reason, 30), estimate))

max_vac <- max(data_vac_plot$estimate)

ggplot(data_vac_plot, aes(x = estimate, y = reason, fill = second_home)) +
  geom_col(width = 0.7) +
  geom_text(aes(label = paste0(comma(estimate), ' (', percent(prop_of_vacant, accuracy = 1), ')')),
            hjust = 0,
            nudge_x = 0.015 * max_vac,
            size = 9/.pt,
            color = 'gray0',
            family = font_family) +
  labs(title = 'Vacant Homes by Reason in Healdsburg',
       x = 'Vacant Housing Units',
       y = 'Reason Vacant',
       caption = str_wrap(paste0(
         'Source: U.S. Census Bureau, American Community Survey ', acs_period,
         ' 5-Year Estimates, Table B25004. Percentages are shares of all ', comma(second$vacant_total),
         ' vacant homes. Reasons with no homes are not shown. Margins of error are large; see the CSV.'),
         width = 110)) +
  scale_x_continuous(labels = label_comma(),
                     breaks = breaks_pretty(n = 5),
                     expand = expansion(mult = c(0.0019, 0.02))) +
  scale_fill_manual(values = c('TRUE' = '#a1cc3a', 'FALSE' = '#0d3353'), guide = 'none') +
  coord_cartesian(xlim = c(0, max_vac * 1.3)) +
  theme(axis.ticks.x = element_line(color = 'gray70'),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'gray70'),
        panel.grid.minor.x = element_line(linewidth = 0.4, color = 'gray85'))

save_chart(paste0('1.2e_vacant-homes-by-reason_acs', year_acs5))
