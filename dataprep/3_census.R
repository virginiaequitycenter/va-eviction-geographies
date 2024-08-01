# Exploring Eviction Geographies 
# Background: https://docs.google.com/document/d/1YQzIEelpFLTj7D2t7xB0-F6GVqapQn4lv1I0bGxAUng/edit

# Setup ----
library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(zctaCrosswalk)

# Anonymous function to rename census variables:
rename_variables <- . %>%
  rename(total_pop = "B01003_001E", # total pop
         total_renters = "B25033_008E", # total renter population
         housing_units = "B25002_001E", # total housing units
         rental_units = "B25070_001E", # total renter-occupied housing units
         med_hh_income = "S1901_C01_012E", # median household income
         rent30 = "B25070_007E",  # N renters with 30-34.9% of income to rent 
         rent35 = "B25070_008E", # N renters with 35-39.9% of income to rent 
         rent40 = "B25070_009E", # N renters with 40-49.9% of income to rent 
         rent50 = "B25070_010E", # N renters with 50+% of income to rent 
         med_gross_rent = "B25064_001E", # median gross rent
         percent_white = "DP05_0079PE", # percent white alone 
         percent_black = "DP05_0080PE", # percent black or African American alone
         percent_aian = "DP05_0081PE", # percent American Indian and Alaska Native alone  
         percent_asian = "DP05_0082PE", # percent Asian alone
         percent_nhpi = "DP05_0083PE", # percent Native Hawaiian and Other Pacific Islander alone
         percent_other = "DP05_0084PE", # percent Some other race alone 
         percent_two = "DP05_0085PE", # percent Two or more races
         percent_hispanic = "DP05_0073PE") # Percent Hispanic or Latino

vars <- c("B01003_001", "B25033_008", "B25002_001", "B25070_001", "S1901_C01_012", 
          "B25070_007", "B25070_008", "B25070_009", "B25070_010", "B25064_001",    
          "DP05_0079P", "DP05_0080P", "DP05_0081P", "DP05_0082P", "DP05_0083P",    
          "DP05_0084P", "DP05_0085P", "DP05_0073P")    

# Anonymous function to derive population-level info:
derive_pops <- . %>%
  mutate(percent_rental_units = (rental_units/housing_units) * 100,
       percent_renters = (total_renters/total_pop) * 100,
       total_burdened = rent30 + rent35 + rent40 + rent50,
       percent_burdened = case_when(
         total_renters > 0 ~ (total_burdened/total_renters) * 100,
         TRUE ~ 0))

# Anonymous function to calculate eviction-level percentages 
calculate_percentages <- . %>%
  mutate(
  #Total filed per rental unit 
  filed_unit = case_when( 
    rental_units > 0 ~ (total_filed/rental_units) * 100,  
    TRUE ~ NA),
  #Percent filed by non-person plaintiffs
  percent_plaintiff_business = case_when(
    total_filed > 0 ~ (cases_plaintiff_business / total_filed) * 100, 
    TRUE ~ NA),
  #Percent of cases that went to judgment
  percent_judgment = case_when(
    total_filed > 0 ~ (total_judgment / total_filed) * 100,
    TRUE ~ NA),
  #Judgments per rental unit
  judgment_rate = case_when(
    rental_units > 0 ~ (total_judgment / rental_units) * 100,
    TRUE ~ NA)
  )

# Supplemental info ----
# Zip codes:
zips <- read_csv("data/zip_code_database.csv")

zips <- zips %>%
  filter(state == "VA",
         decommissioned == 0) %>%
  select(zip, type, primary_city, county) %>%
  mutate(zip = as.numeric(zip))

# Legal Aid Service areas:
legal_aid_service_areas <- read_csv("data/legal_aid_service_areas.csv") %>%
  select(-geometry)

# Zipcode level ----
# Get census info
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
  rename_variables()

# Derive population-level info
zcta_rent <- zcta_rent %>%
  filter(housing_units > 0,
         total_pop > 0) %>%
  derive_pops()

# Join with supplemental zip and service area info 
zcta_rent <- zcta_rent %>%
  left_join(zips, by = join_by(GEOID == zip)) %>%
  left_join(legal_aid_service_areas, by = join_by(county == locality)) %>%
  drop_na(type)

# Join with evictions
evictions_zip <- read_csv("data/evictions_zip.csv")

zcta_rent <- zcta_rent %>%
  mutate(GEOID = as.integer(GEOID)) %>%
  left_join(evictions_zip, by = join_by(GEOID == defendant_zip))

# Missing zips? Cities -- included in relevant county data

# Calculate eviction percentages
zcta_rent <- zcta_rent %>%
  calculate_percentages()
  
# Save 
saveRDS(zcta_rent, "data/zcta_rent.RDS")

# County level ----
# Get census info
county_rent <- get_acs(geography = "county",
                       state = "VA",
                       variable = vars,
                       geometry = TRUE, 
                       output = "wide")

# Rename variables 
county_rent <- county_rent %>%
  select(-ends_with("M")) %>%
  mutate(GEOID = as.numeric(GEOID),
         NAME = str_to_title(gsub("(.*),.*", "\\1", NAME))) %>%
  rename_variables()

# Derive population-level info
county_rent <- county_rent %>%
  filter(housing_units > 0,
         total_pop > 0) %>%
  derive_pops()

# Join with legal aid service area
county_rent <- county_rent %>%
  left_join(legal_aid_service_areas, by = join_by(NAME == locality))

# Save copy for creating legal aid service area dataframe
tmp_lasa <- county_rent

# Join with evictions
evictions_county <- read_csv("data/evictions_county.csv")

county_rent <- county_rent %>%
  left_join(evictions_county, by = join_by(NAME == locality))

# Calculate eviction percentages 
county_rent <- county_rent %>%
  calculate_percentages()

# Save county aggregation 
saveRDS(county_rent, "data/county_rent.RDS")

# Legal Aid Service Area level ----

# Use county census info to calculate population variables for service areas
lasa_rent <- tmp_lasa %>%
  group_by(legal_aid_service_area) %>%
  summarise(
    # add counts
    across(c(total_pop, total_renters, rental_units, housing_units,  total_burdened), sum),
    # get median percentages and dollar amounts 
    across(c(med_gross_rent, med_hh_income, percent_white, percent_black,
                     percent_aian, percent_asian, percent_nhpi, percent_other, 
                     percent_two, percent_hispanic), mean),
    # derive pop variables
    percent_rental_units = (rental_units/housing_units) * 100,
    percent_renters = (total_renters/total_pop) * 100,
    percent_burdened = case_when(
      total_renters > 0 ~ (total_burdened/total_renters) * 100,
      TRUE ~ 0),
    # create new geometry 
    geometry = st_union(geometry)) 

# Join with evictions
evictions_servicearea <- read_csv("data/evictions_servicearea.csv")

lasa_rent <- lasa_rent %>%
  left_join(evictions_servicearea)

# Calculate eviction percentages
lasa_rent <- lasa_rent %>%
  calculate_percentages()

# Save legal aid service area aggregation 
saveRDS(lasa_rent, "data/lasa_rent.RDS")
