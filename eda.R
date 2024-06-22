
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

# *ZCTAs ----
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

#write_csv(zcta_rent, "data/zcta_rent.csv")
#zcta_rent <- read_csv("data/zcta_rent.csv")
           
# Supplemental zip info
zips <- read_csv("data/zip_code_database.csv")

zips <- zips %>%
  filter(state == "VA",
         decommissioned == 0) %>%
  select(zip, type, primary_city, county) %>%
  mutate(zip = as.numeric(zip))

zcta_rent <- zcta_rent %>%
  left_join(zips, by = join_by(GEOID == zip)) %>%
  drop_na(type)

# Visualize----

# *Renters ----
pal <- colorNumeric(palette = "viridis", domain = NULL, reverse = TRUE)

zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(percent_renters),
              fillOpacity = 0.5,
              popup = paste0("Total Renters: ", zcta_rent$total_renters, "<br>",
                             "Total Population ", zcta_rent$total_pop, "<br>",
                             "Percent Renters: ", round(zcta_rent$percent_renters), "%", "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ percent_renters,
            title = paste0("Percent of Population", "<br>",
                           "Living in Rental Units"),
            labFormat = labelFormat(suffix = "%"), 
            opacity = 1)

# *Rent burden ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(percent_burdened),
              fillOpacity = 0.5,
              popup = paste0("Total Renters: ", zcta_rent$total_renters, "<br>",
                             "Cost-Burdened Renters: ", zcta_rent$total_burdened, "<br>",
                             "Percent of Renters: ", round(zcta_rent$percent_burdened), "%", "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ percent_burdened,
            title = paste0("Percent of Renters", "<br>",
                           "Identified as Cost-Burdened"),
            labFormat = labelFormat(suffix = "%"), 
            opacity = 1)

# *Median gross rent ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(med_gross_rent),
              fillOpacity = 0.5,
              popup = paste0("Median Gross Rent: ", scales::dollar(zcta_rent$med_gross_rent), "<br>",
                             "Median Household Income: ", scales::dollar(zcta_rent$med_hh_income), "<br>",
                             "Number of Cost-Burdened Renters: ", zcta_rent$total_burdened, "<br>",
                             "Total Rental Population: ", zcta_rent$total_renters, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ med_gross_rent,
            title = "Median Gross Rent",
            labFormat = labelFormat(prefix = "$"), 
            opacity = 1)

# *Median hh income ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(med_hh_income),
              fillOpacity = 0.5,
              popup = paste0("Median Household Income: ", scales::dollar(zcta_rent$med_hh_income), "<br>",
                             "Median Gross Rent: ", scales::dollar(zcta_rent$med_gross_rent), "<br>",
                             "Number of Cost-Burdened Renters: ", zcta_rent$total_burdened, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ med_hh_income,
            title = "Median Household Income",
            labFormat = labelFormat(prefix = "$"), 
            opacity = 1)

# # Evictions ----

evictions <- read.delim("data/cases_residential_only.txt", sep = ",")

# Identify if the defendant had an attorney present 
evictions$defendant_attorney <- na_if(evictions$defendant_attorney, "")

evictions <- evictions %>%
  mutate(
    d_attorney_present = case_when(
      defendant_attorney == "NONE" | defendant_attorney == "SELF-REPRESENTED" | is.na(defendant_attorney) ~ FALSE,
      TRUE ~ TRUE),
    principal_amount = parse_number(principal_amount))

# Identify non-residential plaintiffs 
#remotes::install_github("virginiaequitycenter/ECtools")
library(ECtools)
evictions$plaintiff_non_residential <- identify_non_residential(evictions$plaintiff_name)

# Explore judgments:
# # A tibble: 11 × 2
# # Groups:   judgment [11]
# judgment                                n
# <chr>                               <int>
#   1 ""                                   5773
# 2 "Case Dismissed"                   227577
# 3 "Case Dismissed with prejudice"       801
# 4 "Case Dismissed without prejudice"   2408
# 5 "Defendant"                           848
# 6 "Non-suit"                          69405
# 7 "Not Found/Unserved"                 4505
# 8 "Other"                              5948
# 9 "Plaintiff"                        349681
# 10 "Transfer/Change of Venue"             89
# 11 "null without prejudice"                3

# Questions:
# What should we do when there is no judgement? 
# What about when the judgement is other? 
# Do we care if a case was dismissed w/ or w/o prejudice? 

# Temp solution:
evictions <- evictions %>%
  mutate(
    default = case_when(
      grepl("Default", latest_hearing_result) ~ TRUE,
      TRUE ~ FALSE),
    judgment_clean = case_when(
      grepl("Case|Non-suit|Unserved|null", judgment) ~ "Case Dismissed",
      grepl("Defendant", judgment) ~ "Defendant",
      grepl("Plaintiff", judgment) ~ "Plaintiff",
      grepl("Other|Transfer", judgment) ~ "Other",
      TRUE ~ "None"))

# Get totals and medians by geography
total_filed <- evictions %>% group_by(defendant_zip) %>%
  count() %>%
  rename(total_filed = n)

total_default <- evictions %>% 
  filter(default == TRUE) %>%
  group_by(defendant_zip) %>%
  count() %>%
  rename(total_default = n)

total_plaintiff_won <- evictions %>% 
  filter(judgment_clean == "Plaintiff") %>%
  group_by(defendant_zip) %>%
  count() %>%
  rename(total_plaintiff_won = n)

median_principal <- evictions %>%
  group_by(defendant_zip) %>%
  summarise(median_principal = median(principal_amount, na.rm = TRUE))

#Number of cases where the defendant had an attorney 
n_d_attorney <- evictions %>%
  filter(d_attorney_present == TRUE) %>%
  group_by(defendant_zip) %>%
  count() %>%
  rename(n_d_attorney = n)

# Rate of non-person evictors 
cases_plaintiff_business <- evictions %>%
  filter(plaintiff_non_residential == TRUE) %>%
  group_by(defendant_zip) %>%
  count() %>%
  rename(cases_plaintiff_business = n)

# Join back to df
zcta_rent <- zcta_rent %>%
  mutate(GEOID = as.integer(GEOID)) %>%
  left_join(total_filed, by = join_by(GEOID == defendant_zip)) %>%
  left_join(total_default, by = join_by(GEOID == defendant_zip)) %>%
  left_join(total_plaintiff_won, by = join_by(GEOID == defendant_zip)) %>%
  left_join(median_principal, by = join_by(GEOID == defendant_zip)) %>%
  left_join(n_d_attorney, by = join_by(GEOID == defendant_zip)) %>%
  left_join(cases_plaintiff_business, by = join_by(GEOID == defendant_zip))

# TODO: convert NAs to 0?

# Derive some variables
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
      TRUE ~ NA)
    )
         
# *Evictions per rental unit----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(filed_unit),
              fillOpacity = 0.5,
              popup = paste0("Eviction Rate per Rental Unit: ", round(zcta_rent$filed_unit), "%", "<br>",
                             "Total Rental Households: ", zcta_rent$rental_units, "<br>",
                             "Evictions Filed: ", zcta_rent$total_filed, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ filed_unit,
            title = paste0("Eviction Rate", "<br>",
                           "Per Rental Unit"),
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

# *Evictions per renter ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(filed_renter),
              fillOpacity = 0.5,
              popup = paste0("Eviction Rate per Renter: ", round(zcta_rent$filed_renter), "%", "<br>",
                             "Total Renters: ", zcta_rent$total_renters, "<br>",
                             "Evictions Filed: ", zcta_rent$total_filed, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ filed_renter,
            title = paste0("Eviction Rate", "<br>",
                           "Per Renter"),
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

# *Evictions per total pop ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(filed_pop),
              fillOpacity = 0.5,
              popup = paste0("Eviction Rate per Total Population: ", round(zcta_rent$filed_pop), "%", "<br>",
                             "Evictions Filed: ", zcta_rent$total_filed, "<br>",
                             "Total Population: ", zcta_rent$total_pop, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ filed_pop,
            title = paste0("Eviction Rate Per", "<br>",
                           "Total Population"),
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

# *Percent default ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(percent_default),
              fillOpacity = 0.5,
              popup = paste0("Default Rate: ", round(zcta_rent$percent_default), "%", "<br>",
                             "Evictions Filed: ", zcta_rent$total_filed, "<br>",
                             "Total Population: ", zcta_rent$total_pop, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ percent_default,
            title = paste0("Default Rate"),
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

# *Percent plaintiff won ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(percent_plaintiff_won),
              fillOpacity = 0.5,
              popup = paste0("Plaintiff Won: ", round(zcta_rent$percent_plaintiff_won), "%", "<br>",
                             "Default Rate: ", round(zcta_rent$percent_default), "%", "<br>",
                             "Evictions Filed: ", zcta_rent$total_filed, "<br>",
                             "Total Population: ", zcta_rent$total_pop, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ percent_plaintiff_won,
            title = paste0("Percent Plaintiff Won"),
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

# *Median Principal Amount ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(median_principal),
              fillOpacity = 0.5,
              popup = paste0("Median Principal Amount: ", scales::dollar(zcta_rent$median_principal), "<br>",
                             "Evictions Filed: ", zcta_rent$total_filed, "<br>",
                             "Total Population: ", zcta_rent$total_pop, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ median_principal,
            title = paste0("Median Principal Amount"),
            labFormat = labelFormat(prefix = "$"),
            opacity = 1)

# *Percent defendant attorney present ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(percent_d_attorney),
              fillOpacity = 0.5,
              popup = paste0("Defendant Attorney Present: ", round(zcta_rent$percent_d_attorney), "%", "<br>",
                             "Evictions Filed: ", zcta_rent$total_filed, "<br>",
                             "Total Population: ", zcta_rent$total_pop, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ percent_d_attorney,
            title = paste0("Percent of Cases", "<br>",
                           "with a Defendant Attorney"),
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

# *Percent of cases filed by non-person plaintiffs ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(percent_plaintiff_business),
              fillOpacity = 0.5,
              popup = paste0("Non-Person Plaintiffs: ", round(zcta_rent$percent_plaintiff_business), "%", "<br>",
                             "Evictions Filed: ", zcta_rent$total_filed, "<br>",
                             "Total Population: ", zcta_rent$total_pop, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ percent_plaintiff_business,
            title = paste0("Percent of Cases Filed", "<br>",
                           "by Non-Person Plaintiffs"),
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)
  