
# Healdsburg SOH, Section 1: Housing Units by Type -------------------------
#
# Pulls housing units by building type for the City of Healdsburg
# (incorporated city, Census place 0633056) from ACS Table B25024 and
# charts them. ADUs aren't separated here: the ACS counts them inside the
# other building types.
#
# To update to the 2021-2025 release, change year_acs5 below to 2025.


# Settings ----------------------------------------------------------------

year_acs5 <- 2024                     # 2024 = ACS 2020-2024 5-year estimates

healdsburg_geoid <- '0633056'         # Healdsburg city, CA
dir_outputs      <- 'outputs/1_housing-stock'

# point this at the fonts.R helper from the SOH repo to use Sofia Pro
path_fonts <- 'SOH-2023/scripts/fonts.R'

# add 90% margin-of-error bars to the chart? (SOH 2023 charts didn't)
show_moe <- FALSE


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

raw_type <- get_acs(
  geography = 'place',
  table = 'B25024',
  state = 'CA',
  year = year_acs5,
  summary_var = 'B25024_001',
  survey = 'acs5'
)


# Clean -------------------------------------------------------------------

full_type <- left_join(raw_type, vars_acs5, by = c('variable' = 'name')) %>%
  clean_names(case = 'snake') %>%
  filter(geoid == healdsburg_geoid) %>%
  mutate(level = str_count(label, '!!'),             # 2 = a building-type row
         units_raw = str_extract(label, '[^!]+$')) %>%
  filter(level == 2) %>%
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
if (any(is.na(full_type$type))) {
  stop('New B25024 category to map: ', paste(full_type$units_raw[is.na(full_type$type)], collapse = ', '))
}

tidy_type <- full_type %>%
  group_by(type, summary_est, summary_moe) %>%
  summarize(moe = moe_sum(moe, estimate),
            estimate = sum(estimate),
            .groups = 'drop') %>%
  mutate(prop     = estimate / summary_est,
         prop_moe = moe_prop(estimate, summary_est, moe, summary_moe),
         cv       = (moe / 1.645) / estimate,
         type = factor(type,
                       levels = c('Single-Family Detached',
                                  'Single-Family Attached',
                                  '2-4 Units',
                                  '5-19 Units',
                                  '20+ Units',
                                  'Mobile Home',
                                  'Boat, RV, Van, etc.'))) %>%
  filter(!(type == 'Boat, RV, Van, etc.' & estimate == 0)) %>%   # drop if none
  arrange(type)

tidy_type

write_csv(tidy_type, file.path(dir_outputs, paste0('1b_units-by-type_acs', year_acs5, '.csv')))


# Plot --------------------------------------------------------------------

# '<1%' instead of '0%' for very small shares
label_share <- function(p) if_else(p > 0 & p < 0.005, '<1%', percent(p, accuracy = 1))

# labels go inside the bar when it's long enough, otherwise just past its end
max_type <- max(tidy_type$estimate + if (show_moe) tidy_type$moe else 0)

data_type_plot <- tidy_type %>%
  mutate(label  = paste0(comma(estimate), ' (', label_share(prop), ')'),
         inside = estimate > 0.25 * max_type & !show_moe)

ggplot(data_type_plot, aes(x = estimate, y = fct_rev(type))) +
  geom_col(fill = '#0d3353',
           width = 0.75) +
  {if (show_moe) geom_errorbar(aes(xmin = pmax(estimate - moe, 0), xmax = estimate + moe),
                               width = 0.2, linewidth = 0.4, color = 'gray50')} +
  geom_text(data = filter(data_type_plot, inside),
            aes(label = label),
            hjust = 1,
            nudge_x = -0.015 * max_type,
            size = 9/.pt,
            color = '#ffffff',
            family = font_family) +
  geom_text(data = filter(data_type_plot, !inside),
            aes(x = estimate + if (show_moe) moe else 0, label = label),
            hjust = 0,
            nudge_x = 0.015 * max_type,
            size = 9/.pt,
            color = 'gray0',
            family = font_family) +
  labs(title = paste0('Housing Units by Building Type in Healdsburg, ', year_acs5 - 4, '-', year_acs5),
       x = 'Housing Units',
       y = 'Building Type',
       caption = paste0('Source: U.S. Census Bureau, American Community Survey ',
                        year_acs5 - 4, '-', year_acs5, ' 5-Year Estimates, Table B25024.')) +
  scale_x_continuous(labels = label_comma(),
                     breaks = breaks_pretty(n = 5),
                     expand = expansion(mult = c(0.0019, 0.02))) +
  coord_cartesian(xlim = c(0, max_type * 1.2)) +
  theme(axis.ticks.x = element_line(color = 'gray70'),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.4, color = 'gray70'),
        panel.grid.minor.x = element_line(linewidth = 0.4, color = 'gray85'))

ggsave(filename = file.path(dir_outputs, paste0('1b_units-by-type_acs', year_acs5, '.pdf')),
       width = 6,
       height = 4,
       units = 'in')

ggsave(filename = file.path(dir_outputs, paste0('1b_units-by-type_acs', year_acs5, '.png')),
       width = 6,
       height = 4,
       units = 'in',
       dpi = 300)
