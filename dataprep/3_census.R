# Exploring Eviction Geographies 
# Background: https://docs.google.com/document/d/1YQzIEelpFLTj7D2t7xB0-F6GVqapQn4lv1I0bGxAUng/edit
# ACS 5-year Survey 2019-2023

# Setup ----
library(janitor)
library(readxl)
library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)
options(tigris_use_cache = TRUE)
library(zctaCrosswalk)

# Anonymous function to rename census variables:
rename_variables <- . %>%
  rename(total_pop = "B01003_001E", # total population
         total_renters = "B25033_008E", # total renter population
         housing_units = "B25002_001E", # total housing units 
         rental_units = "B25003_003E", # total renter-occupied housing units
         percent_pov = "S1701_C03_001E", # poverty rate
         med_hh_income = "S1901_C01_012E", # median household income 
         rent30 = "B25070_007E", # N renters with 30-34.9% of income to rent 
         rent35 = "B25070_008E", # N renters with 35-39.9% of income to rent 
         rent40 = "B25070_009E", # N renters with 40-49.9% of income to rent 
         rent50 = "B25070_010E", # N renters with 50+% of income to rent 
         med_gross_rent = "B25064_001E", # median gross rent estimate
         med_gross_rent_MOE = "B25064_001M", # median gross rent MOE 
         percent_white = "DP05_0079PE", # percent white alone 
         percent_black = "DP05_0080PE", # percent black or African American alone
         percent_aian = "DP05_0081PE", # percent American Indian and Alaska Native alone  
         percent_asian = "DP05_0082PE", # percent Asian alone
         percent_nhpi = "DP05_0083PE", # percent Native Hawaiian and Other Pacific Islander alone
         percent_other = "DP05_0084PE", # percent another race not listed alone 
         percent_two = "DP05_0085PE", # percent Two or more races
         percent_hispanic = "DP05_0073PE", # percent Hispanic or Latino
         med_tax = "B25103_001E", # median real estate taxes paid for owner-occupied housing units
         med_tax_MOE = "B25103_001M") # median real estate taxes MOE
         

vars <- c("B01003_001",    # total pop
          "B25033_008",    # renter pop
          "B25002_001",    # total housing units
          "B25003_003",    # total renter-occupied units
          "S1701_C03_001", # pov rate
          "S1901_C01_012", # med hh income
          "B25070_007", "B25070_008", "B25070_009", "B25070_010", # rent burden
          "B25064_001",    # med rent 
          "DP05_0079P", "DP05_0080P", "DP05_0081P", "DP05_0082P", "DP05_0083P",
          "DP05_0084P", "DP05_0085P", "DP05_0073P", # race and ethnicity 
          "B25103_001")    # med tax

# Anonymous function to derive population-level info:
calculate_pops <- . %>%
  mutate(percent_rental_units = (rental_units/housing_units) * 100,
       percent_renters = (total_renters/total_pop) * 100,
       total_burdened = rent30 + rent35 + rent40 + rent50,
       percent_burdened = case_when(
         total_renters > 0 ~ (total_burdened/total_renters) * 100,
         TRUE ~ 0),
       exploit = med_gross_rent / med_tax,
       exploit_MOE = moe_ratio(med_gross_rent, med_tax, med_gross_rent_MOE, med_tax_MOE))

# Anonymous function to calculate eviction-level info: 
calculate_evictions <- . %>%
  mutate(
  #Total filed per rental unit 
  filed_unit = case_when( 
    rental_units > 0 ~ total_filed/rental_units,  
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
    rental_units > 0 ~ (total_judgment / rental_units),
    TRUE ~ NA)
  )

# Supplemental info ----
# HUD zip codes:
hud_zips <- read_excel("data/hud_zips.xlsx") %>%
  clean_names()

hud_zips <- hud_zips %>%
  filter(usps_zip_pref_state == "VA") %>%
  rename(state = usps_zip_pref_state,
         city = usps_zip_pref_city,
         county_fips = county) %>%
  mutate(city = str_to_title(city),
         county_fips = gsub("^.{0,2}", "", county_fips))

# Legal Aid Service areas:
legal_aid_service_areas <- read_csv("data/legal_aid_service_areas.csv") %>%
  select(-geometry)

# Zipcode level ----
# Get census info
zcta_rent <- get_acs(geography = "zcta",
                     year = 2023,
                     variable = vars,
                     geometry = TRUE, 
                     output = "wide",
                     cb = FALSE)

# Filter to VA zipcodes and rename variables 
zcta_rent <- zcta_rent %>%
  filter(GEOID %in% get_zctas_by_state("VA")) %>%
  rename_variables() %>%
  select(-ends_with("M"),
         -NAME)

# Derive population-level info
zcta_rent <- zcta_rent %>%
  filter(housing_units > 0,
         total_pop > 0) %>%
  calculate_pops()

# Join with supplemental zip and service area info 
zcta_rent <- zcta_rent %>%
  left_join(hud_zips, by = join_by(GEOID == zip)) %>%
  left_join(legal_aid_service_areas, by = join_by(county_fips == fips)) 

# Join with evictions
evictions_zip <- read_csv("data/evictions_zip.csv")

zcta_rent <- zcta_rent %>%
  mutate(GEOID = as.integer(GEOID)) %>%
  left_join(evictions_zip, by = join_by(GEOID == defendant_zip))

# Missing zips? Cities -- included in relevant county data

# Calculate eviction percentages
zcta_rent <- zcta_rent %>%
  calculate_evictions()
  
# Save 
saveRDS(zcta_rent, "data/zcta_rent.RDS")

# County level ----
# Get census info
county_rent <- get_acs(geography = "county",
                       year = 2023,
                       state = "VA",
                       variable = vars,
                       geometry = TRUE, 
                       output = "wide")

# Rename variables 
county_rent <- county_rent %>%
  mutate(NAME = str_to_title(gsub("(.*),.*", "\\1", NAME)),
         county_fips = gsub("^.{0,2}", "", GEOID)) %>%
  rename_variables() %>%
  select(-ends_with("M")) 

# Derive population-level info
county_rent <- county_rent %>%
  filter(housing_units > 0,
         total_pop > 0) %>%
  calculate_pops()

# Join with legal aid service area
county_rent <- county_rent %>%
  left_join(legal_aid_service_areas, by = join_by(county_fips == fips))

# Save copy for creating legal aid service area dataframe
tmp_lasa <- county_rent

# Join with evictions
evictions_county <- read_csv("data/evictions_county.csv")

county_rent <- county_rent %>%
  left_join(evictions_county, by = join_by(county_fips == fips))

# Calculate eviction percentages 
county_rent <- county_rent %>%
  calculate_evictions()

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
    across(c(med_gross_rent, med_hh_income, med_tax, percent_white, percent_black,
                     percent_aian, percent_asian, percent_nhpi, percent_other, 
                     percent_two, percent_hispanic), mean),
    # derive pop variables
    percent_rental_units = (rental_units/housing_units) * 100,
    percent_renters = (total_renters/total_pop) * 100,
    percent_burdened = case_when(
      total_renters > 0 ~ (total_burdened/total_renters) * 100,
      TRUE ~ 0),
    exploit = (med_gross_rent / med_tax) * 100,
    exploit_MOE = moe_ratio(med_gross_rent, med_tax, med_gross_rent_MOE, med_tax_MOE),
    # create new geometry 
    geometry = st_union(geometry)) 

# Join with evictions
evictions_servicearea <- read_csv("data/evictions_servicearea.csv")

lasa_rent <- lasa_rent %>%
  left_join(evictions_servicearea)

# Calculate eviction percentages
lasa_rent <- lasa_rent %>%
  calculate_evictions()

# Save legal aid service area aggregation 
saveRDS(lasa_rent, "data/lasa_rent.RDS")
