# Pull Census data and prepare for analysis
# Background: https://docs.google.com/document/d/1YQzIEelpFLTj7D2t7xB0-F6GVqapQn4lv1I0bGxAUng/edit
# ACS 5-year Surveys

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
         undergrad_pop = "B14001_008E", # total undergrad population
         grad_pop = "B14001_009E", # total grad school population
         housing_units = "B25002_001E", # total occupied housing units 
         rental_units = "B25003_003E", # total renter-occupied housing units 
         pct_pov = "S1701_C03_001E", # poverty rate
         med_hh_income = "S1901_C01_012E", # median household income 
         rent30 = "B25070_007E", # N renters with 30-34.9% of income to rent 
         rent35 = "B25070_008E", # N renters with 35-39.9% of income to rent 
         rent40 = "B25070_009E", # N renters with 40-49.9% of income to rent 
         rent50 = "B25070_010E", # N renters with 50+% of income to rent 
         med_gross_rent = "B25064_001E", # median gross rent estimate
         med_gross_rent_MOE = "B25064_001M", # median gross rent MOE 
         pct_white = "DP05_0079PE", # percent white alone 
         pct_black = "DP05_0080PE", # percent black or African American alone
         pct_hispanic = "DP05_0073PE", # percent Hispanic or Latino
         med_tax = "B25103_001E", # median real estate taxes paid for owner-occupied housing units
         med_tax_MOE = "B25103_001M", # median real estate taxes MOE
         moved_county = "B07013_009E", # renter-occupied units where householder moved within county
         moved_va = "B07013_012E", # renter-occupied units where householder moved from a diff county within state 
         moved_otherstate = "B07013_015E", # renter-occupied units where householder that moved from a diff state
         moved_abroad = "B07013_018E") # renter-occupied units where householder moved from abroad 
         
vars <- c("B01003_001",    # total pop
          "B14001_008", "B14001_009",   # total college student pop
          "B25002_001",    # total housing units
          "B25003_003",    # total renter-occupied units
          "S1701_C03_001", # pov rate
          "S1901_C01_012", # med hh income
          "B25070_007", "B25070_008", "B25070_009", "B25070_010", # rent burden
          "B25064_001",    # med rent 
          "DP05_0079P", "DP05_0080P", "DP05_0073P", # race and ethnicity 
          "B25103_001",    # med tax
          "B07013_009", "B07013_012", "B07013_015", "B07013_018") # residential mobility

# Residential mobility = # of renters that moved / total renter-occupied households 
# Population density = total population / land area in square miles 
# Housing density = total occupied housing units / land area in square miles 

# Anonymous function to derive population-level info:
derive_pops <- . %>%
  mutate(
    pct_rental_units = (rental_units/housing_units) * 100,
    total_burdened = rent30 + rent35 + rent40 + rent50,
    pct_burdened = case_when(
      total_pop > 0 ~ (total_burdened/rental_units) * 100,
      TRUE ~ 0),
    exploit = med_gross_rent / med_tax,
    exploit_MOE = moe_ratio(med_gross_rent, med_tax, med_gross_rent_MOE, med_tax_MOE),
    mobility_rate = ((moved_county + moved_va + moved_otherstate + moved_abroad)/rental_units),
    pct_students = ((undergrad_pop + grad_pop)/total_pop) * 100,
    pct_nonwhite = 100 - pct_white)

# Anonymous function to calculate eviction and judgment rates: 
calculate_evictions <- . %>%
  mutate(
    #Total filed per rental unit
    eviction_rate = case_when(
      rental_units > 0 ~ total_filed/rental_units,
      TRUE ~ NA),
    #Judgments per rental unit
    judgment_rate = case_when(
      rental_units > 0 ~ (n_judgment/rental_units),
      TRUE ~ NA))

# Read geographies ----
# HUD zip codes:
hud_zips <- read_excel("data/hud_zips.xlsx") %>%
  clean_names() %>%
  filter(usps_zip_pref_state == "VA") %>%
  rename(state = usps_zip_pref_state,
         approx_city = usps_zip_pref_city,
         county_fips = county) %>%
  mutate(approx_city = str_to_title(approx_city),
         county_fips = gsub("^.{0,2}", "", county_fips)) %>%
  select(zip, approx_city, county_fips)

# Remove duplicates 
hud_zips <- hud_zips[!duplicated(hud_zips$zip),]

# Legal Aid Service areas:
legal_aid_service_areas <- read_csv("data/service_areas.csv")
 
# Zipcode level ----
# Get census info
zip_raw <- get_acs(geography = "zcta",
                    year = 2022,
                    variable = vars,
                    geometry = TRUE,
                    output = "wide",
                    cb = FALSE)

# Filter to VA zipcodes and rename variables 
zip_data <- zip_raw %>%
  filter(GEOID %in% get_zctas_by_state("VA")) %>%
  rename_variables() %>%
  mutate(zip = GEOID) %>%
  select(-ends_with("M"),
         -NAME)

# Derive population-level info
zip_data <- zip_data %>%
  filter(housing_units > 0,
         total_pop > 0) %>%
  derive_pops()

# Get land area (for population density)
va_zips_aland <- zctas(state = "VA", year = 2010) %>% # 2010 -- too far back?
  mutate(landarea_sqmi = ALAND10 / 2589988.10) %>%
  select(zip = ZCTA5CE10, landarea_sqmi) %>%
  st_drop_geometry()

# Join population data with land area by zip and calculate population and housing densities
zip_data <- zip_data %>%
  left_join(va_zips_aland) %>%
  mutate(pop_density = total_pop / landarea_sqmi,
         housing_density = housing_units / landarea_sqmi)

# Join with HUD zip dataframe to get approximate city/region:
# Since some zips cross multiple city, county, and state lines, there's not an easy way to perfectly associate 
# each zip with a single fips code, so instead we use the "approximate city" jurisdiction from HUD to provide 
# some geographic context in the app popups. Using the distinct() function earlier, we manually assign each 
# zip to an approximate fips so that we can associate it with a legal aid service area in the app. 

zip_data <- zip_data %>%
  left_join(hud_zips)

zip_data <- zip_data %>%
  left_join(legal_aid_service_areas, by = join_by(county_fips == fips)) %>%
  rename(county_name = locality)

# *Join Census data with Eviction data ----
# Join pop data with eviction data and calculate eviction rates

# 2018-2019:
esum_zip_1819 <- read_csv("data/app_data/esum_zip_1819.csv") %>%
  mutate(defendant_zip = as.character(defendant_zip))

zip_1819 <- zip_data %>%
  left_join(esum_zip_1819, by = join_by(zip == defendant_zip)) %>%
  calculate_evictions() #870 obs

# 2020-2021:
esum_zip_2021 <- read_csv("data/app_data/esum_zip_2021.csv") %>%
  mutate(defendant_zip = as.character(defendant_zip))

zip_2021 <- zip_data %>%
  left_join(esum_zip_2021, by = join_by(zip == defendant_zip)) %>%
  calculate_evictions() #870 obs

# 2022-2023:
esum_zip_2223 <- read_csv("data/app_data/esum_zip_2223.csv") %>%
  mutate(defendant_zip = as.character(defendant_zip))

zip_2223 <- zip_data %>%
  left_join(esum_zip_2223, by = join_by(zip == defendant_zip)) %>%
  calculate_evictions() #870 obs

# *Explore thresholds ----
# NA zip codes, total pop minimums, pct rental unit minimums, etc. 
# Example 2018-2019:
zip_1819 %>%
  ggplot() +
  geom_histogram(aes(pct_rental_units)) 

# If we reduce to only known regions where the percent rental units is >=5% we adjust for 
# most of the regions where the population or housing & rental units is 0 or NA (why, however
# are there evictions being filed in these regions if there are no people or rental units though?)

# Anonymous function to keep only regions where the percent rental units is >=5% and drop regions
# where there were no evictions filed:
reduce_data <- . %>%
  filter(pct_rental_units >= 5,
         !is.na(eviction_rate))

zip_1819 <- zip_1819 %>%
  reduce_data() #870 -> 722 obs

zip_2021 <- zip_2021 %>%
  reduce_data() #870 -> 696 obs

zip_2223 <- zip_2223 %>%
  reduce_data() #879 -> 721 obs

# *Save ----
# Individual
saveRDS(zip_1819, "data/app_data/zip_1819.RDS")
saveRDS(zip_2021, "data/app_data/zip_2021.RDS")
saveRDS(zip_2223, "data/app_data/zip_2223.RDS")

# Stacked (perhaps easier for app?)
zip_1819 <- zip_1819 %>%
  mutate(yrs = "2018-2019")

zip_2021 <- zip_2021 %>%
  mutate(yrs = "2020-2021")

zip_2223 <- zip_2223 %>%
  mutate(yrs = "2022-2023")

zip <- bind_rows(zip_1819, zip_2021, zip_2223)
saveRDS(zip, "data/app_data/zip.RDS")

# County level ----
# Get census info
county_raw <- get_acs(geography = "county",
                       year = 2022,
                       state = "VA",
                       variable = vars,
                       geometry = TRUE, 
                       output = "wide")

# Rename variables 
county_data <- county_raw %>%
  mutate(NAME = str_to_title(gsub("(.*),.*", "\\1", NAME)),
         county_fips = gsub("^.{0,2}", "", GEOID)) %>%
  rename_variables() %>%
  select(-ends_with("M"))

# Derive population-level info
county_data <- county_data %>%
  filter(housing_units > 0,
         total_pop > 0) %>%
  derive_pops()

# Calculate population and housing densities 
va_counties_aland <- counties(state = "VA", year = 2023) %>%
  mutate(landarea_sqmi = ALAND / 2589988.10) %>%
  select(GEOID, landarea_sqmi) %>%
  st_drop_geometry()

county_data <- county_data %>%
  left_join(va_counties_aland, by = join_by(GEOID == GEOID)) %>%
  mutate(pop_density = total_pop / landarea_sqmi,
         housing_density = housing_units / landarea_sqmi) 

# Join with Legal Aid Service Areas by county fips 
county_data <- county_data %>%
  left_join(legal_aid_service_areas, by = join_by(county_fips == fips)) %>%
  select(-locality)

# Save copy for creating Legal Aid Service Area dataframe
tmp_lasa <- county_data

# *Join Census data with Eviction data ----
# Join pop data with eviction data and calculate eviction rates

# 2018-2019:
esum_county_1819 <- read_csv("data/app_data/esum_county_1819.csv")

county_1819 <- county_data %>%
  left_join(esum_county_1819, by = join_by(county_fips == fips)) %>%
  calculate_evictions() #133 obs

# 2020-2021: 
esum_county_2021 <- read_csv("data/app_data/esum_county_2021.csv")

county_2021 <- county_data %>%
  left_join(esum_county_2021, by = join_by(county_fips == fips)) %>%
  calculate_evictions() #133 obs

# 2022-2023:
esum_county_2223 <- read_csv("data/app_data/esum_county_2223.csv")

county_2223 <- county_data %>%
  left_join(esum_county_2223, by = join_by(county_fips == fips)) %>%
  calculate_evictions() #133 obs

# *Explore thresholds  ----
# The minimum percent rental units is ~6%, so we currently don't reduce anything
county_2021 %>%
ggplot() +
  geom_histogram(aes(pct_rental_units)) 

# *Save ----
# Individual
saveRDS(county_1819, "data/app_data/county_1819.RDS")
saveRDS(county_2021, "data/app_data/county_2021.RDS")
saveRDS(county_2223, "data/app_data/county_2223.RDS")

# Stacked (perhaps easier for app?)
county_1819 <- county_1819 %>%
  mutate(yrs = "2018-2019")

county_2021 <- county_2021 %>%
  mutate(yrs = "2020-2021")

county_2223 <- county_2223 %>%
  mutate(yrs = "2022-2023")

county <- bind_rows(county_1819, county_2021, county_2223)
saveRDS(county, "data/app_data/county.RDS")

# Legal Aid Service Area level ----
# Use county-level census info to calculate population variables for service areas
lasa_data <- tmp_lasa %>%
  group_by(legal_aid_service_area) %>%
  summarise(
    # Add count variables:
    across(c(total_pop, undergrad_pop, grad_pop, housing_units, rental_units, moved_county,
             moved_va, moved_otherstate, moved_abroad, rent30, rent35, rent40, rent50, 
             landarea_sqmi), sum),
    # Get median percentages and dollar amounts for census-provided info:
    across(c(med_gross_rent, med_gross_rent_MOE, med_tax, med_tax_MOE, med_hh_income,
             pct_pov, pct_white, pct_black, pct_hispanic), median),
    # Derive pop variables:
    pct_rental_units = (rental_units/housing_units) * 100,
    total_burdened = rent30 + rent35 + rent40 + rent50,
    pct_burdened = case_when(
      total_pop > 0 ~ (total_burdened/total_pop) * 100,
      TRUE ~ 0),
    exploit = med_gross_rent / med_tax,
    exploit_MOE = moe_ratio(med_gross_rent, med_tax, med_gross_rent_MOE, med_tax_MOE),
    mobility_rate = ((moved_county + moved_va + moved_otherstate + moved_abroad)/rental_units),
    pct_students = ((undergrad_pop + grad_pop)/total_pop) * 100,
    pct_nonwhite = 100 - pct_white,
    pop_density = total_pop / landarea_sqmi,
    housing_density = housing_units / landarea_sqmi, 
    # Create new geometries: 
    geometry = st_union(geometry)) 

# *Join Census data with Eviction data ----
# Join pop data with eviction data and calculate eviction rates

# 2018-2019:
esum_lasa_1819 <- read_csv("data/app_data/esum_lasa_1819.csv")

lasa_1819 <- lasa_data %>%
  left_join(esum_lasa_1819) %>%
  calculate_evictions()

# 2020-2021:
esum_lasa_2021 <- read_csv("data/app_data/esum_lasa_2021.csv")

lasa_2021 <- lasa_data %>%
  left_join(esum_lasa_2021) %>%
  calculate_evictions()

# 2022-2023:
esum_lasa_2223 <- read_csv("data/app_data/esum_lasa_2223.csv")

lasa_2223 <- lasa_data %>%
  left_join(esum_lasa_2223) %>%
  calculate_evictions()

# *Save ----
# Individual
saveRDS(lasa_1819, "data/app_data/lasa_1819.RDS")
saveRDS(lasa_2021, "data/app_data/lasa_2021.RDS")
saveRDS(lasa_2223, "data/app_data/lasa_2223.RDS")

# Stacked (perhaps easier for app?)
lasa_1819 <- lasa_1819 %>%
  mutate(yrs = "2018-2019")

lasa_2021 <- lasa_2021 %>%
  mutate(yrs = "2020-2021")

lasa_2223 <- lasa_2223 %>%
  mutate(yrs = "2022-2023")

lasa <- bind_rows(lasa_1819, lasa_2021, lasa_2223)
saveRDS(lasa, "data/app_data/lasa.RDS")
