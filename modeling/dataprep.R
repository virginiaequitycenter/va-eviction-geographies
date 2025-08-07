# Prepare eviction and census data for modeling 

# Libraries ----
library(sf)
library(tidyverse)
library(tidycensus)
library(tigris)

# 2024 (post-COVID)----

## 2024 Evictions (before collapsing court jurisdictions) ---- 
# From dataprep/2_evictions.R:

evictions24 <- read_csv("data/evictions_clean.csv") %>%
  filter(filed_year == "2024") %>%
  select(case_number, plaintiff_name, serial_filing, plaintiff_non_residential, defendant_zip, court, locality, fips)

eviction_counts24 <- evictions24 %>%
  group_by(court, locality, fips) %>%
  count(name = "total_filed24")

## Census measures (2019-2023) before collapsing and deriving percents: ----

rename_variables <- . %>%
  rename(total_pop = "B01003_001E", # total population
         undergrad_pop = "B14001_008E", # total undergrad population
         grad_pop = "B14001_009E", # total grad school population
         housing_units = "B25002_001E", # total occupied housing units 
         rental_units = "B25003_003E", # (Tenure: total renter-occupied housing units)
         rental_pop = "B25008_003E", # (Pop: total pop of renter-occupied housing units)
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
          "B25008_003",    # total pop of renter-occupied units 
          "S1701_C03_001", # pov rate
          "S1901_C01_012", # med hh income
          "B25070_007", "B25070_008", "B25070_009", "B25070_010", # rent burden
          "B25064_001",    # med rent 
          "DP05_0079P", "DP05_0080P", "DP05_0073P", # race and ethnicity 
          "B25103_001",    # med tax
          "B07013_009", "B07013_012", "B07013_015", "B07013_018") # residential mobility

county_raw <- get_acs(geography = "county",
                      year = 2023,
                      state = "VA",
                      variable = vars,
                      geometry = TRUE, 
                      output = "wide")

county23 <- county_raw %>%
  mutate(NAME = str_to_title(gsub("(.*),.*", "\\1", NAME)),
         county_fips = gsub("^.{0,2}", "", GEOID)) %>%
  rename_variables() %>%
  select(-ends_with("M"))

# Add totals
county23 <- county23 %>%
  mutate(
    total_burdened = rent30 + rent35 + rent40 + rent50,
    total_moved = moved_county + moved_va + moved_otherstate + moved_abroad,
    total_students = undergrad_pop + grad_pop)

# Keep only variables of interest
county23 <- county23 %>%
  select(-contains(c("grad", "rent3", "rent4", "rent5", "moved_", "income"))) %>%
  st_drop_geometry()

# Land areas
va_counties_aland <- counties(state = "VA", year = 2023) %>%
  mutate(landarea_sqmi = ALAND / 2589988.10) %>%
  select(GEOID, landarea_sqmi) %>%
  st_drop_geometry()
  
## Landlord summaries (2022-2024) before collapsing ----
evictions2224 <- read_csv("data/evictions_clean.csv") %>%
  filter(filed_year >= "2022",
         filed_year <= "2024") %>%
  select(case_number, plaintiff_name, serial_filing, plaintiff_non_residential, defendant_zip, court, locality, fips)

# Total filed:
total_filed <- evictions2224 %>%
  group_by(locality, court, fips) %>%
  count(name = "total_filed2224")

# Number of serial filings:
n_serial <- evictions2224 %>%
  filter(serial_filing == TRUE) %>%
  group_by(locality, court, fips) %>%
  count(name = "n_serial")

# Number of filings by business landlords:
n_cases_filed_by_businesses <- evictions2224 %>%
  filter(plaintiff_non_residential == TRUE) %>%
  group_by(locality, court, fips) %>%
  count(name = "n_cases_filed_by_businesses")

# Total landlords:
total_plaintiffs <- evictions2224 %>%
  group_by(locality, court, fips) %>%
  summarize(total_plaintiffs = n_distinct(plaintiff_name, na.rm = TRUE))

# Total business landlords:
n_business_plaintiffs <- evictions2224 %>%
  filter(plaintiff_non_residential == TRUE) %>%
  group_by(locality, court, fips) %>%
  summarize(n_business_plaintiffs = n_distinct(plaintiff_name, na.rm = TRUE))

# Number of landlords that engage in serial filing behavior: 
n_plaintiffs_serial <- evictions2224 %>%
  filter(serial_filing == TRUE) %>%
  group_by(locality, court, fips) %>%
  summarize(n_plaintiffs_serial = n_distinct(plaintiff_name, na.rm = TRUE))

# Join:
landlord_summary2224 <- total_filed %>%
  left_join(n_serial) %>%
  left_join(n_cases_filed_by_businesses) %>%
  left_join(total_plaintiffs) %>%
  left_join(n_business_plaintiffs) %>%
  left_join(n_plaintiffs_serial)
  
## Collapse ----

# Join
joined24 <- full_join(eviction_counts24, county23, by = join_by(fips == county_fips)) %>%
  inner_join(landlord_summary2224) %>%
  left_join(va_counties_aland) %>%
  mutate_if(is.integer, as.numeric)

to_collapse <- joined24 %>%
  mutate(NAME = case_when(
    fips %in% c("095", "830") ~ "Williamsburg and James City County",
    fips %in% c("195", "720") ~ "Norton and Wise County",
    fips %in% c("163", "678") ~ "Lexington and Rockbridge County",
    fips %in% c("165", "660") ~ "Harrisonburg and Rockingham County",
    fips %in% c("059", "600") ~ "Fairfax City and County",
    fips %in% c("703", "700") ~ "Newport News County and City",
    TRUE ~ NAME
  )) %>% 
  filter(NAME %in% c("Williamsburg and James City County", "Norton and Wise County",
                     "Lexington and Rockbridge County", "Harrisonburg and Rockingham County",
                     "Fairfax City and County", "Newport News County and City")) 

ok_already <- joined24 %>% 
  filter(!fips %in% to_collapse$fips)

# Add values
collapsed24 <- to_collapse %>% 
  group_by(NAME) %>% 
  summarise(total_filed24 = sum(total_filed24, na.rm = T), 
            total_filed2224 = sum(total_filed2224, na.rm = T),
            total_pop = sum(total_pop, na.rm = T),
            housing_units = sum(housing_units, na.rm = T),
            rental_units = sum(rental_units, na.rm = T),
            rental_pop = sum(rental_pop, na.rm = T),
            med_gross_rent = median(med_gross_rent, na.rm = T),
            med_gross_rent_MOE = median(med_gross_rent_MOE, na.rm = T),
            med_tax = median(med_tax, na.rm = T),
            med_tax_MOE = median(med_tax_MOE, na.rm = T),
            pct_pov = median(pct_pov, na.rm = T),
            pct_white = median(pct_white, na.rm = T),
            pct_black = median(pct_black, na.rm = T),
            pct_hispanic = median(pct_hispanic, na.rm = T),
            total_burdened = sum(total_burdened, na.rm = T),
            total_moved = sum(total_moved, na.rm = T),
            total_students = sum(total_students, na.rm = T),
            n_serial = sum(n_serial, na.rm = T),
            n_cases_filed_by_businesses = sum(n_cases_filed_by_businesses, na.rm = T),
            total_plaintiffs = sum(total_plaintiffs, na.rm = T),
            n_business_plaintiffs = sum(n_business_plaintiffs, na.rm = T),
            n_plaintiffs_serial = sum(n_plaintiffs_serial, na.rm = T),
            landarea_sqmi = sum(landarea_sqmi, na.rm = T)) %>% 
  bind_rows(ok_already)

# Calculate some variables using new collapsed values
collapsed24 <- collapsed24 %>%
  mutate(
    eviction_rate24 = total_filed24 / rental_units,
    pct_rental_units = (rental_units/housing_units) * 100,
    pct_burdened = case_when(
      total_pop > 0 ~ (total_burdened/rental_units) * 100,
      TRUE ~ 0),
    exploit = med_gross_rent / med_tax,
    exploit_MOE = moe_ratio(med_gross_rent, med_tax, med_gross_rent_MOE, med_tax_MOE),
    pct_moved = (total_moved / rental_pop) * 100,
    pct_students = (total_students / total_pop) * 100,
    pct_serial = ((n_serial / total_filed2224) * 100),
    pct_business = ((n_business_plaintiffs / total_plaintiffs) * 100),
    prop_landlord_serial = n_plaintiffs_serial / total_plaintiffs,
    pop_density = total_pop / landarea_sqmi)

## Separation indices ----
# From dataprep/4_separation_indices.R:
dissim23_collapsed <- read_csv("modeling/data/dissim23_collapsed.csv")

# Join with dissimilarity indices
collapsed24 <- collapsed24 %>% 
  mutate(NAME = tolower(NAME)) %>% 
  inner_join(mutate(dissim23_collapsed, NAME = tolower(county_collapsed))) %>%
  mutate(percent_white = (cowhite/total_pop) * 100)

# Keep only variables of interest for modeling
collapsed24 <- collapsed24 %>%
  select(-contains(c("gross", "tax", "MOE", "pct_white", "pct_black", 
                   "pct_hispanic", "locality", "county_collapsed")))

# Save
write_csv(collapsed24, "modeling/data/collapsed24.csv")


# 2019 (pre-COVID) ----

## 2019 Evictions (before collapsing court jurisdictions) ---- 
# From dataprep/2_evictions.R:

evictions19 <- read_csv("data/evictions_clean.csv") %>%
  filter(filed_year == "2019") %>%
  select(case_number, plaintiff_name, serial_filing, plaintiff_non_residential, defendant_zip, court, locality, fips)

eviction_counts19 <- evictions19 %>%
  group_by(court, locality, fips) %>%
  count(name = "total_filed19")

## Census measures (2015-2019) before collapsing and deriving percents ----

county_raw <- get_acs(geography = "county",
                      year = 2019,
                      state = "VA",
                      variable = vars,
                      geometry = TRUE, 
                      output = "wide")

county19 <- county_raw %>%
  mutate(NAME = str_to_title(gsub("(.*),.*", "\\1", NAME)),
         county_fips = gsub("^.{0,2}", "", GEOID)) %>%
  rename_variables() %>%
  select(-ends_with("M"))

# Add totals
county19 <- county19 %>%
  mutate(
    total_burdened = rent30 + rent35 + rent40 + rent50,
    total_moved = moved_county + moved_va + moved_otherstate + moved_abroad,
    total_students = undergrad_pop + grad_pop)

# Keep only variables of interest
county19 <- county19 %>%
  select(-contains(c("grad", "rent3", "rent4", "rent5", "moved_", "income"))) %>%
  st_drop_geometry()

# Land areas
va_counties_aland <- counties(state = "VA", year = 2019) %>%
  mutate(landarea_sqmi = ALAND / 2589988.10) %>%
  select(GEOID, landarea_sqmi) %>%
  st_drop_geometry()

## Landlord summaries (2018-2019) before collapsing ----
evictions1819 <- read_csv("data/evictions_clean.csv") %>%
  filter(filed_year >= "2018",
         filed_year <= "2019") %>%
  select(case_number, plaintiff_name, serial_filing, plaintiff_non_residential, defendant_zip, court, locality, fips)

# Total filed:
total_filed <- evictions1819 %>%
  group_by(locality, court, fips) %>%
  count(name = "total_filed1819")

# Number of serial filings:
n_serial <- evictions1819 %>%
  filter(serial_filing == TRUE) %>%
  group_by(locality, court, fips) %>%
  count(name = "n_serial")

# Number of filings by business landlords:
n_cases_filed_by_businesses <- evictions1819 %>%
  filter(plaintiff_non_residential == TRUE) %>%
  group_by(locality, court, fips) %>%
  count(name = "n_cases_filed_by_businesses")

# Total landlords:
total_plaintiffs <- evictions1819 %>%
  group_by(locality, court, fips) %>%
  summarize(total_plaintiffs = n_distinct(plaintiff_name, na.rm = TRUE))

# Total business landlords:
n_business_plaintiffs <- evictions1819 %>%
  filter(plaintiff_non_residential == TRUE) %>%
  group_by(locality, court, fips) %>%
  summarize(n_business_plaintiffs = n_distinct(plaintiff_name, na.rm = TRUE))

# Number of landlords that engage in serial filing behavior: 
n_plaintiffs_serial <- evictions1819 %>%
  filter(serial_filing == TRUE) %>%
  group_by(locality, court, fips) %>%
  summarize(n_plaintiffs_serial = n_distinct(plaintiff_name, na.rm = TRUE))

# Join:
landlord_summary1819 <- total_filed %>%
  left_join(n_serial) %>%
  left_join(n_cases_filed_by_businesses) %>%
  left_join(total_plaintiffs) %>%
  left_join(n_business_plaintiffs) %>%
  left_join(n_plaintiffs_serial)

## Collapse ----

# Join
joined19 <- full_join(eviction_counts19, county19, by = join_by(fips == county_fips)) %>%
  inner_join(landlord_summary1819) %>%
  left_join(va_counties_aland) %>%
  mutate_if(is.integer, as.numeric)

to_collapse <- joined19 %>%
  mutate(NAME = case_when(
    fips %in% c("095", "830") ~ "Williamsburg and James City County",
    fips %in% c("195", "720") ~ "Norton and Wise County",
    fips %in% c("163", "678") ~ "Lexington and Rockbridge County",
    fips %in% c("165", "660") ~ "Harrisonburg and Rockingham County",
    fips %in% c("059", "600") ~ "Fairfax City and County",
    fips %in% c("703", "700") ~ "Newport News County and City",
    TRUE ~ NAME
  )) %>% 
  filter(NAME %in% c("Williamsburg and James City County", "Norton and Wise County",
                     "Lexington and Rockbridge County", "Harrisonburg and Rockingham County",
                     "Fairfax City and County", "Newport News County and City")) 

ok_already <- joined19 %>% 
  filter(!fips %in% to_collapse$fips)

# Add values
collapsed19 <- to_collapse %>% 
  group_by(NAME) %>% 
  summarise(total_filed19 = sum(total_filed19, na.rm = T), 
            total_filed1819 = sum(total_filed1819, na.rm = T),
            total_pop = sum(total_pop, na.rm = T),
            housing_units = sum(housing_units, na.rm = T),
            rental_units = sum(rental_units, na.rm = T),
            rental_pop = sum(rental_pop, na.rm = T),
            med_gross_rent = median(med_gross_rent, na.rm = T),
            med_gross_rent_MOE = median(med_gross_rent_MOE, na.rm = T),
            med_tax = median(med_tax, na.rm = T),
            med_tax_MOE = median(med_tax_MOE, na.rm = T),
            pct_pov = median(pct_pov, na.rm = T),
            pct_white = median(pct_white, na.rm = T),
            pct_black = median(pct_black, na.rm = T),
            pct_hispanic = median(pct_hispanic, na.rm = T),
            total_burdened = sum(total_burdened, na.rm = T),
            total_moved = sum(total_moved, na.rm = T),
            total_students = sum(total_students, na.rm = T),
            n_serial = sum(n_serial, na.rm = T),
            n_cases_filed_by_businesses = sum(n_cases_filed_by_businesses, na.rm = T),
            total_plaintiffs = sum(total_plaintiffs, na.rm = T),
            n_business_plaintiffs = sum(n_business_plaintiffs, na.rm = T),
            n_plaintiffs_serial = sum(n_plaintiffs_serial, na.rm = T),
            landarea_sqmi = sum(landarea_sqmi, na.rm = T)) %>% 
  bind_rows(ok_already)

# Calculate some variables using new collapsed values:
collapsed19 <- collapsed19 %>%
  mutate(
    eviction_rate19 = total_filed19 / rental_units,
    pct_rental_units = (rental_units/housing_units) * 100,
    pct_burdened = case_when(
      total_pop > 0 ~ (total_burdened/rental_units) * 100,
      TRUE ~ 0),
    exploit = med_gross_rent / med_tax,
    exploit_MOE = moe_ratio(med_gross_rent, med_tax, med_gross_rent_MOE, med_tax_MOE),
    pct_moved = (total_moved / rental_pop) * 100,
    pct_students = (total_students / total_pop) * 100,
    pct_serial = ((n_serial / total_filed1819) * 100),
    pct_business = ((n_business_plaintiffs / total_plaintiffs) * 100),
    prop_landlord_serial = n_plaintiffs_serial / total_plaintiffs,
    pop_density = total_pop / landarea_sqmi)

## Separation indices ----
# From dataprep/4_separation_indices.R:
dissim19_collapsed <- read_csv("modeling/data/dissim19_collapsed.csv")

# Join with dissimilarity indices
collapsed19 <- collapsed19 %>% 
  mutate(NAME = tolower(NAME)) %>% 
  inner_join(mutate(dissim19_collapsed, NAME = tolower(county_collapsed))) %>%
  mutate(percent_white = (cowhite/total_pop) * 100)

# Keep only variables of interest for modeling
collapsed19 <- collapsed19 %>%
  select(-contains(c("gross", "tax", "MOE", "pct_white", "pct_black", 
                     "pct_hispanic", "locality", "county_collapsed")))

# Save
write_csv(collapsed19, "modeling/data/collapsed19.csv")
