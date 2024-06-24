
# Exploring Eviction Geographies 
# Background: https://docs.google.com/document/d/1YQzIEelpFLTj7D2t7xB0-F6GVqapQn4lv1I0bGxAUng/edit

# Setup ----
library(leaflet)
library(sf)
library(tidyverse)
library(tidycensus)
library(zctaCrosswalk)

# Census ----
# Initial variables to explore: 
vars <- c("B01003_001",    # total population
          "S1701_C03_001", # poverty rate
          "S1901_C01_012", # median household income
          "B25070_001",    # total renter-occupied housing units
          "B25070_007",    # N renters with 30-34.9% of income to rent 
          "B25070_008",    # N renters with 35-39.9% of income to rent 
          "B25070_009",    # N renters with 40-49.9% of income to rent 
          "B25070_010",    # N renters with 50+% of income to rent 
          "B25002_001",    # total housing units
          "B25064_001",    # median gross rent
          "B25033_008")    # total renter population  

# Rent burden = percent of renting households with gross rent 30% or more of household income

# ZCTAs ----
# Error : The Census Bureau has not yet released the CB ZCTA file for 2022. 
# Please use the argument `year = 2020` or `cb = FALSE` instead.
zcta_rent <- get_acs(geography = "zcta",
                     variable = vars,
                     geometry = TRUE, 
                     output = "wide",
                     cb = FALSE)

# Filter to VA zipcodes and rename variables 
zcta_rent <- zcta_rent %>%
  filter(GEOID %in% get_zctas_by_state("VA")) %>%
  select(-ends_with("M")) %>%
  mutate(GEOID = as.numeric(GEOID)) %>%
  rename(total_pop = "B01003_001E",
         pov_rate = "S1701_C03_001E",
         med_hh_income = "S1901_C01_012E",
         rental_units = "B25070_001E",
         rent30 = "B25070_007E",
         rent35 = "B25070_008E",
         rent40 = "B25070_009E",
         rent50 = "B25070_010E",
         housing_units = "B25002_001E",
         med_gross_rent = "B25064_001E",
         total_renters = "B25033_008E")

# Normalize by populations
zcta_rent <- zcta_rent %>%
  filter(housing_units > 0,
         total_pop > 0) %>%
  mutate(percent_rental_units = (rental_units/housing_units) * 100,
         percent_renters = (total_renters/total_pop) * 100,
         total_burdened = rent30 + rent35 + rent40 + rent50,
         percent_burdened = case_when(
           total_renters > 0 ~ (total_burdened/total_renters) * 100,
           TRUE ~ NA))

# Supplemental zip info ----
zips <- read_csv("data/zip_code_database.csv")

zips <- zips %>%
  filter(state == "VA",
         decommissioned == 0) %>%
  select(zip, type, primary_city, county) %>%
  mutate(zip = as.numeric(zip))

zcta_rent <- zcta_rent %>%
  left_join(zips, by = join_by(GEOID == zip)) %>%
  drop_na(type)

# Read clean eviction data----
evictions_zip <- read_csv("data/evictions_zip.csv")

# Join with ZCTA df
zcta_rent <- zcta_rent %>%
  mutate(GEOID = as.integer(GEOID)) %>%
  left_join(evictions_zip, by = join_by(GEOID == defendant_zip))

# Derive some variables ----
zcta_rent <- zcta_rent %>%
  mutate(
    # total filed per rental unit
    filed_unit = case_when( 
      rental_units > 0 ~ (total_filed/rental_units) * 100,  
      TRUE ~ NA),
    # total filed per renter
    filed_renter = case_when(
      total_renters > 0 ~ (total_filed/total_renters) * 100,
      TRUE ~ NA),
    # total filed per total pop
    filed_pop = (total_filed/total_pop) * 100,
    # percent default
    percent_default = case_when(
      total_filed > 0 ~ (total_default/total_filed) * 100,
      TRUE ~ NA),
    # percent plaintiff won 
    percent_plaintiff_won = case_when(
      total_filed > 0 ~ (total_plaintiff_won/total_filed) * 100,
      TRUE ~ NA),
    # percent where defendant had an attorney present
    percent_d_attorney = case_when(
      total_filed > 0 ~ (n_d_attorney / total_filed) * 100,
      TRUE ~ NA),
    # percent filed by non-person plaintiffs
    percent_plaintiff_business= case_when(
      total_filed > 0 ~ (cases_plaintiff_business / total_filed) * 100, 
      TRUE ~ NA),
    # percent of cases where possession was immediate 
    percent_immediate = case_when(
      total_filed > 0 ~ (total_immediate / total_filed) * 100,
      TRUE ~ NA)
    )

saveRDS(zcta_rent, "data/zcta_rent.RDS")
