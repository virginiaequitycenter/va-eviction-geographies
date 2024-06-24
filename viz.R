
# Visualize eviction trends 

# Setup ----
library(leaflet)
library(sf)
library(tidyverse)

zcta_rent <- readRDS("data/zcta_rent.RDS")

# Question: best practices for reading and writing shapefiles? 

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

# *Percent where possession was immediate ----
zcta_rent %>%
  st_transform(crs = 4326) %>%
  leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(stroke = TRUE, 
              weight = 0.5,
              opacity = 1,
              color = "black",
              fillColor = ~pal(percent_immediate),
              fillOpacity = 0.5,
              popup = paste0("Immediate Possession: ", round(zcta_rent$percent_immediate), "%", "<br>",
                             "Evictions Filed: ", zcta_rent$total_filed, "<br>",
                             "Total Population: ", zcta_rent$total_pop, "<br>",
                             "Zipcode: ", zcta_rent$GEOID, "<br>",
                             "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
              highlightOptions = highlightOptions(
                fillOpacity = 1,
                bringToFront = FALSE)) %>%
  addLegend("bottomright",
            pal = pal, 
            values = ~ percent_immediate,
            title = paste0("Percent of Cases", "<br>",
                           "with Immediate Possession"),
            labFormat = labelFormat(suffix = "%"),
            opacity = 1)

