
# Healdsburg SOH, Section 1.2: Tenure (Renters vs. Owners) -----------------
#
# Pulls owner- and renter-occupied households for the City of Healdsburg
# (incorporated city, Census place 0633056) from ACS Table B25003 and charts
# them. Covers occupied homes only, so the total is lower than in 1.1.
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

raw_tenure <- get_acs(
  geography = 'place',
  variables = c('Owner-Occupied'  = 'B25003_002',
                'Renter-Occupied' = 'B25003_003'),
  state = 'CA',
  year = year_acs5,
  summary_var = 'B25003_001',
  survey = 'acs5'
)


# Clean -------------------------------------------------------------------

tidy_tenure <- raw_tenure %>%
  clean_names(case = 'snake') %>%
  filter(geoid == healdsburg_geoid) %>%
  rename(tenure = variable) %>%
  mutate(prop     = estimate / summary_est,
         prop_moe = moe_prop(estimate, summary_est, moe, summary_moe),
         cv       = (moe / 1.645) / estimate,
         tenure   = factor(tenure, levels = c('Owner-Occupied', 'Renter-Occupied'))) %>%
  select(tenure, estimate, moe, cv, prop, prop_moe, summary_est, summary_moe)

tidy_tenure

write_csv(tidy_tenure, file.path(dir_outputs, paste0('1.2a_tenure_acs', year_acs5, '.csv')))


# Plot --------------------------------------------------------------------

ggplot(tidy_tenure, aes(x = tenure, y = estimate, fill = tenure)) +
  geom_col(width = 0.45) +
  geom_text(aes(label = comma(estimate)),
            vjust = -0.9,
            size = 9/.pt,
            color = 'gray0',
            family = font_family) +
  labs(title = 'Owner- and Renter-Occupied Homes in Healdsburg',
       x = 'Tenure',
       y = 'Occupied Housing Units',
       caption = str_wrap(paste0('Source: U.S. Census Bureau, American Community Survey ', acs_period,
                                 ' 5-Year Estimates, Table B25003. Occupied homes only.'), width = 80)) +
  scale_y_continuous(labels = label_comma(),
                     breaks = breaks_pretty(n = 5),
                     expand = expansion(mult = c(0.0019, 0.0019))) +
  scale_x_discrete(expand = expansion(add = 0.5)) +
  scale_fill_manual(values = colors_tenure, guide = 'none') +
  coord_cartesian(ylim = c(0, max(tidy_tenure$estimate) * 1.1)) +
  theme(axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(linewidth = 0.4, color = 'gray70'),
        panel.grid.minor.y = element_line(linewidth = 0.4, color = 'gray85'))

save_chart(paste0('1.2a_tenure_acs', year_acs5), width = 4)   # narrower: only two bars
