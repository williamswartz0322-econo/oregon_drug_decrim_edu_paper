# 01_build_data.R
# Build the master analysis dataset (R port of the data steps in CollegeV2.do)
#
# Inputs:
#   data-raw/Data_12-19-2024---668.csv   IPEDS custom data file (wide, one col per var-year)
#   data-raw/demographic_census_data.dta ACS 1-year state-level estimates, 2012-2023 (2020 missing)
# Output:
#   data/master.rds                      institution-year panel, 2012-2023

library(dplyr)
library(tidyr)
library(readr)
library(stringr)
library(haven)

raw <- read_csv("data-raw/Data_12-19-2024---668.csv",
                show_col_types = FALSE,
                name_repair = "minimal")

# The CSV has a trailing empty column (Stata's `drop v240`)
raw <- raw[, names(raw) != ""]

# Columns look like "GRRTTOT (DRVGR2023)", "RET_PCF (EF2022D)", "FIPS (HD2020)".
# Pivot to an institution-year panel, keeping the variable stem as the column name.
long <- raw %>%
  rename(uniti = UnitID, institutionname = `Institution Name`) %>%
  pivot_longer(
    cols = matches("\\([A-Z]+\\d{4}D?\\)$"),
    names_to = c(".value", "year"),
    names_pattern = "^(.+?) \\([A-Z]+(\\d{4})D?\\)$"
  ) %>%
  mutate(year = as.integer(year)) %>%
  rename_with(tolower) %>%
  # match the variable names produced by the do-file's rename chain
  rename(ret_pcfef = ret_pcf, ret_pcpef = ret_pcp, enrtotdrvef = enrtot) %>%
  # drop unused race categories (grrtan grrtap grrtnh grrt2m grrtun)
  select(-grrtan, -grrtap, -grrtnh, -grrt2m, -grrtun)

# Keep only 2-year and 4-year institutions
college <- long %>% filter(iclevel %in% c(1, 2))

census <- read_dta("data-raw/demographic_census_data.dta") %>%
  mutate(fips = as.integer(fips), year = as.integer(year))

master <- college %>%
  left_join(select(census, -name), by = c("fips", "year")) %>%
  filter(fips != 72) %>%               # drop Puerto Rico
  # Stata's `keep if sector > 0` also keeps missing sector (missing == +inf)
  filter(sector > 0 | is.na(sector)) %>%
  mutate(
    public = as.integer(sector %in% c(1, 4)),
    urban_score = case_when(
      locale == 11 ~ 1.0,  # City: Large
      locale == 12 ~ 0.9,  # City: Midsize
      locale == 13 ~ 0.8,  # City: Small
      locale == 21 ~ 0.7,  # Suburb: Large
      locale == 22 ~ 0.6,  # Suburb: Midsize
      locale == 23 ~ 0.5,  # Suburb: Small
      locale == 31 ~ 0.4,  # Town: Fringe
      locale == 32 ~ 0.3,  # Town: Distant
      locale == 33 ~ 0.2,  # Town: Remote
      locale == 41 ~ 0.15, # Rural: Fringe
      locale == 42 ~ 0.1,  # Rural: Distant
      locale == 43 ~ 0.0   # Rural: Remote
    )
  )

dir.create("data", showWarnings = FALSE)
saveRDS(master, "data/master.rds")

cat("Master data:", nrow(master), "rows,", n_distinct(master$fips), "states,",
    "years", min(master$year), "-", max(master$year), "\n")
