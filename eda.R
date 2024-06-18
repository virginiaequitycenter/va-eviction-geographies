
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
          "B25064_001")    # median gross rent

# Rent burden = percent of renting households with gross rent 30% or more of household income

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
  rename(total_pop = "B01003_001E",
         pov_rate = "S1701_C03_001E",
         med_hh_income = "S1901_C01_012E",
         rental_units = "B25070_001E",
         rent30 = "B25070_007E",
         rent35 = "B25070_008E",
         rent40 = "B25070_009E",
         rent50 = "B25070_010E",
         housing_units = "B25002_001E",
         med_gross_rent = "B25064_001E")

# Calculate rent burden variables
zcta_rent <- zcta_rent %>%
  filter(housing_units > 0,
         total_pop > 0) %>%
  mutate(percent_rentals = (rental_units/housing_units) * 100,
         total_burdened = rent30 + rent35 + rent40 + rent50,
         percent_burdened = (total_burdened/total_pop) * 100)

# Supplemental zip info
zips <- read_csv("data/raw/zip_code_database.csv")

zips <- zips %>%
  filter(state == "VA",
         decommissioned == 0) %>%
  select(zip, type, primary_city, county)

zcta_rent <- zcta_rent %>%
  left_join(zips, by = join_by(GEOID == zip)) %>%
  drop_na(type)

# Visualize ----

# *Rent burden ----
pal <- colorNumeric(palette = "viridis", domain = NULL, reverse = TRUE)

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
              popup = paste0("Percent of Population: ", round(zcta_rent$percent_burdened), "%", "<br>",
                             "Number of Cost-Burdened Renters: ", zcta_rent$total_burdened, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ percent_burdened,
            title = paste0("Percent of Population", "<br>",
                           "Identified as Rent-Burdened"),
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
                             "Total Population: ", zcta_rent$total_pop, "<br>",
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

# *Median gross income ----
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
              popup = paste0("Median Gross Rent: ", scales::dollar(zcta_rent$med_gross_rent), "<br>",
                             "Median Household Income: ", scales::dollar(zcta_rent$med_hh_income), "<br>",
                             "Number of Cost-Burdened Renters: ", zcta_rent$total_burdened, "<br>",
                             "Total Population: ", zcta_rent$total_pop, "<br>",
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

evictions <- read.delim("data/raw/cases_residential_only.txt", sep = ",")

total_filed <- evictions %>% group_by(defendant_zip) %>%
  count()

evictions %>%
  group_by(judgment) %>%
  count() 

# Questions:
# What should we do when there is no judgement? 
# What about when the judgement is other? 
# Do we care if a case was dismissed w/ or w/o prejudice? 

# Quick viz of number of eviction filings per zip:

zcta_rent <- zcta_rent %>%
  mutate(GEOID = as.integer(GEOID)) %>%
  left_join(total_filed, by = join_by(GEOID == defendant_zip))

zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(n),
              fillOpacity = 0.5,
              popup = paste0("Number of evictions filed: ", zcta_rent$n, "<br>",
                             "Total Population: ", zcta_rent$total_pop, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ n,
            title = "Number of Evictions Filed",
            opacity = 1)

# TODO: number of evictions per total households? 



