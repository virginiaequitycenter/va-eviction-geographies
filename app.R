# Global ----

library(leaflet)
library(shiny)
library(sf)
library(tidyverse)

my_choices = c("Rental Population" = "percent_renters",
               "Cost-Burdened Renters" = "percent_burdened",
               "Poverty Rate" = "pov_rate",
               "Eviction Rate" = "filed_pop",
               "Non-Person Filing Rate" = "percent_plaintiff_business",
               "Rate of Default Judgments" = "percent_default",
               "Rate of Immediate Possession" = "percent_immediate",
               "Defendant Attorney Present" = "percent_d_attorney")

zcta_rent <- readRDS("data/zcta_rent.RDS")

# UI ----

ui <- fluidPage(
  titlePanel("Exploring Eviction Geographies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "Select Variable", choices = my_choices)),
    mainPanel(leafletOutput("map") 
    )))

# Server ----

server <- function(input, output, session) {
  
  pal <- colorNumeric(palette = "viridis", domain = NULL, reverse = TRUE)
  
  output$map <- renderLeaflet({
    zcta_rent %>%
      st_transform(crs = 4326) %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  
      addPolygons(stroke = TRUE,
                  weight = 0.5,
                  opacity = 1,
                  color = "black",
                  fillColor = ~pal(zcta_rent[[input$var1]]),
                  fillOpacity = 0.5,
                  popup = paste0("Total Renters: ", zcta_rent$total_renters, "<br>",
                                 "Total Population ", zcta_rent$total_pop, "<br>",
                                 "Percent Renters: ", round(zcta_rent$percent_renters), "%", "<br>",
                                 "Zipcode: ", zcta_rent$GEOID, "<br>",
                                 "Region: ", zcta_rent$primary_city, ", ", zcta_rent$county),
                  highlightOptions = highlightOptions(
                    fillOpacity = 1,
                    bringToFront = FALSE)) %>%
      addLegend("topright",
                pal = pal,
                values = ~ zcta_rent[[input$var1]],
                title = names(my_choices[my_choices == input$var1]),
                labFormat = labelFormat(suffix = "%"),
                opacity = 1)
  })
}

shinyApp(ui, server)
