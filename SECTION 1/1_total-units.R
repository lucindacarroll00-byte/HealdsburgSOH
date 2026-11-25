
# Healdsburg SOH, Section 1: Total Housing Units ---------------------------
#
# Pulls the total number of housing units in the City of Healdsburg
# (incorporated city, Census place 0633056) from the ACS 5-year estimates.
#
# To update to the 2021-2025 release, change year_acs5 below to 2025.


# Settings ----------------------------------------------------------------

year_acs5 <- 2024                     # 2024 = ACS 2020-2024 5-year estimates

healdsburg_geoid <- '0633056'         # Healdsburg city, CA
dir_outputs      <- 'outputs/1_housing-stock'


# Packages ----------------------------------------------------------------

library(tidycensus)  # query census api (needs your Census API key)
library(tidyverse)
library(janitor)     # clean column names easily
library(scales)      # number formatting

dir.create(dir_outputs, recursive = TRUE, showWarnings = FALSE)


# Query -------------------------------------------------------------------

raw_total <- get_acs(
  geography = 'place',
  variables = c('total_units' = 'B25001_001'),
  state = 'CA',
  year = year_acs5,
  survey = 'acs5'
)


# Clean -------------------------------------------------------------------

tidy_total <- raw_total %>%
  clean_names(case = 'snake') %>%
  filter(geoid == healdsburg_geoid) %>%
  mutate(acs_period = paste0(year_acs5 - 4, '-', year_acs5),
         low  = estimate - moe,                  # ACS MOEs are 90% intervals
         high = estimate + moe,
         cv   = (moe / 1.645) / estimate)        # coefficient of variation

tidy_total

# the number for the report
cat(paste0('Healdsburg (city) has an estimated ', comma(tidy_total$estimate),
           ' housing units (+/- ', comma(tidy_total$moe), '), ACS ',
           tidy_total$acs_period, ' 5-year estimates, Table B25001.\n'))

write_csv(tidy_total, file.path(dir_outputs, paste0('1a_total-units_acs', year_acs5, '.csv')))
