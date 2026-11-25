# HealdsburgSOH

R code for the Healdsburg State of Housing report.

## Structure

```
Healdsburg State of Housing.Rproj   open this to start R (sets the working directory)
SECTION 1/
  1_total-units.R                   total housing units (ACS B25001)
  2_units-by-type.R                 units by building type + chart (ACS B25024)
  3_age-of-homes.R                  units by decade built + chart (ACS B25034)
outputs/1_housing-stock/            CSVs, PDFs and PNGs the scripts save
```

All figures are for the City of Healdsburg (Census place 0633056) from the
ACS 5-year estimates.

## Running the scripts

1. Open `Healdsburg State of Housing.Rproj`, then run any script in `SECTION 1`.
2. Census API key: run once in the console, then restart R. The key is saved
   in `~/.Renviron`, outside this repo.

   ```r
   tidycensus::census_api_key("YOUR-KEY-HERE", install = TRUE)
   ```

3. Fonts: charts use Sofia Pro through `fonts.R` from the Gen H SOH repo.
   Set `path_fonts` at the top of scripts 2 and 3 to where that file is;
   without it, charts fall back to the default font.

## Updating to a new ACS release

Change `year_acs5` at the top of each script (2024 = 2020-2024 estimates,
2025 = 2021-2025). Titles, captions and the newest year-built bar update
automatically.
