
# Healdsburg SOH, Section 1: Age of Homes (Year Built) ---------------------
#
# Pulls housing units by decade built for the City of Healdsburg
# (incorporated city, Census place 0633056) from ACS Table B25034 and charts
# them in the style of SOH 2023 Figure 4: navy bars by decade with totals on
# top. B25034 counts all homes, occupied and vacant.
#
# To update to the 2021-2025 release, change year_acs5 below to 2025.


# Settings ----------------------------------------------------------------

year_acs5 <- 2024                     # 2024 = ACS 2020-2024 5-year estimates

healdsburg_geoid <- '0633056'         # Healdsburg city, CA
dir_outputs      <- 'outputs/1_housing-stock'

# point this at the fonts.R helper from the SOH repo to use Sofia Pro
path_fonts <- 'SOH-2023/scripts/fonts.R'


# Packages ----------------------------------------------------------------

library(tidycensus)  # query census api (needs your Census API key)
library(tidyverse)
library(janitor)     # clean column names easily
library(scales)      # labels for ggplot

dir.create(dir_outputs, recursive = TRUE, showWarnings = FALSE)


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


# Query -------------------------------------------------------------------

vars_acs5 <- load_variables(year = year_acs5, dataset = 'acs5', cache = TRUE)

raw_age <- get_acs(
  geography = 'place',
  table = 'B25034',
  state = 'CA',
  year = year_acs5,
  summary_var = 'B25034_001',
  survey = 'acs5'
)


# Clean -------------------------------------------------------------------

full_age <- left_join(raw_age, vars_acs5, by = c('variable' = 'name')) %>%
  clean_names(case = 'snake') %>%
  filter(geoid == healdsburg_geoid) %>%
  mutate(level = str_count(label, '!!'),            # 2 = a decade row
         period_raw = str_remove(str_extract(label, '[^!]+$'), '^Built ')) %>%
  filter(level == 2)

# Decade labels are built from the Census labels instead of typed in, so
# the newest bar reads '2020-24' now and '2020-25' with the next release.
tidy_age <- full_age %>%
  mutate(
    start = as.numeric(str_extract(period_raw, '\\d{4}')),
    end   = case_when(str_detect(period_raw, 'or later')   ~ year_acs5,
                      str_detect(period_raw, 'or earlier') ~ start,
                      TRUE ~ as.numeric(str_extract(period_raw, '\\d{4}$'))),
    period = case_when(
      str_detect(period_raw, 'or earlier') ~ paste0('Pre-', start + 1),
      end - start == 9                     ~ paste0(start, 's'),
      TRUE                                 ~ paste0(start, '-', str_sub(end, 3, 4))
    ),
    prop     = estimate / summary_est,                  # share of all homes
    prop_moe = moe_prop(estimate, summary_est, moe, summary_moe),
    cv       = (moe / 1.645) / estimate                 # coefficient of variation
  ) %>%
  arrange(start) %>%
  mutate(period = fct_inorder(period)) %>%
  select(period, estimate, moe, cv, prop, prop_moe, summary_est, start, end)

tidy_age

write_csv(tidy_age, file.path(dir_outputs, paste0('1c_age-of-homes_acs', year_acs5, '.csv')))


# Plot --------------------------------------------------------------------

acs_period <- paste0(year_acs5 - 4, '-', year_acs5)
max_age    <- max(tidy_age$estimate)

ggplot(tidy_age, aes(x = period, y = estimate)) +
  geom_col(fill = '#0d3353',
           width = 0.75) +
  geom_text(aes(label = comma(estimate)),
            vjust = -0.6,
            size = 9/.pt,
            color = 'gray0',
            family = font_family) +
  labs(title = paste0('Housing Units by Year Built in Healdsburg, ', acs_period),
       x = 'Year Built',
       y = 'Housing Units',
       caption = str_wrap(paste0(
         'Source: U.S. Census Bureau, American Community Survey ', acs_period,
         ' 5-Year Estimates, Table B25034. Includes occupied and vacant homes. ',
         tail(levels(tidy_age$period), 1), ' covers ',
         max(tidy_age$end) - max(tidy_age$start) + 1, ' years, not a full decade.'),
         width = 110)) +
  scale_x_discrete(labels = label_wrap(4)) +
  scale_y_continuous(breaks = breaks_pretty(n = 5),
                     labels = label_comma(),
                     expand = expansion(mult = c(0.0019, 0.0019))) +
  coord_cartesian(ylim = c(0, max_age * 1.12)) +
  theme(axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.4, color = 'gray70'),
        panel.grid.minor.y = element_line(linewidth = 0.4, color = 'gray85'))

ggsave(filename = file.path(dir_outputs, paste0('1c_age-of-homes_acs', year_acs5, '.pdf')),
       width = 6,
       height = 4,
       units = 'in')

ggsave(filename = file.path(dir_outputs, paste0('1c_age-of-homes_acs', year_acs5, '.png')),
       width = 6,
       height = 4,
       units = 'in',
       dpi = 300)
