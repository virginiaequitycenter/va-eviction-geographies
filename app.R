# Global ----

library(leaflet)
library(plotly)
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

my_colors <- c("Southwest Virginia Legal Aid Society" = "#E31A1C",
               "Legal Aid Society of Roanoke Valley" = "#FDBF6F",
               "Blue Ridge Legal Services" = "#1F78B4",
               "Virginia Legal Aid Society" = "#743089",
               "Central Virginia Legal Aid Society" = "#B2DF8A",
               "Legal Services of Northern Virginia" = "#FF7F00",
               "Legal Aid Works" = "#FB9A99",
               "Legal Aid Society of Eastern Virginia" = "#A6CEE3")

zcta_rent <- readRDS("data/zcta_rent.RDS")

# UI ----

ui <- fluidPage(
  titlePanel("Exploring Eviction Geographies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("var1", "Exploratory Variable:", choices = my_choices),
      selectInput("var2", "Comparison Variable:", choices = my_choices),
      imageOutput("img")),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel(
          title = "Explore", 
          h4(textOutput("var1", inline = TRUE)),
          leafletOutput("map")),
        tabPanel(
          title = "Compare", 
          h4(textOutput("var2", inline = TRUE)),
          plotlyOutput("plt")),
        tabPanel("Table", "download button")
    ))))

# Server ----

server <- function(input, output, session) {
  
  output$img <- renderImage({
    list(
      src = file.path("service_areas.png"),
      contentType = "image/png",
      width = 500
    )
  }, deleteFile = FALSE)
  
  
  pal <- colorNumeric(palette = "viridis", domain = NULL, reverse = TRUE)
  
  output$var1 <- renderText({
    as.character(names(my_choices[my_choices == input$var1]))
  })
  
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
                  popup = paste0(as.character(names(my_choices[my_choices == input$var1])), ": ", round(zcta_rent[[input$var1]]), "%", "<br>", 
                                 "Total Population ", zcta_rent$total_pop, "<br>",
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
  
  output$var2 <- renderText({
     
    paste0(as.character(names(my_choices[my_choices == input$var1])), " compared to ", 
           as.character(names(my_choices[my_choices == input$var2])))
    
  })
  
  output$plt <- renderPlotly({
    
    plt_median_x <- as_tibble(zcta_rent) %>%
      filter(!is.na(zcta_rent[[input$var1]])) %>%
      select(percent_renters) %>%
      summarise(plt_median_x = round(median(zcta_rent[[input$var1]])))
    
    plt_median_y <- as_tibble(zcta_rent) %>%
      filter(!is.na(zcta_rent[[input$var2]])) %>%
      select(percent_burdened) %>%
      summarise(plt_median_y = round(median(zcta_rent[[input$var2]])))
    
    plt_dat <- zcta_rent %>%
      filter(zcta_rent[[input$var1]] > 0,
             zcta_rent[[input$var2]] > 0,
             !is.na(legal_aid_service_area))
    
    plt <- ggplot(plt_dat, aes(x = .data[[input$var1]], y = .data[[input$var2]], 
                               color = legal_aid_service_area, size = total_pop)) +
      geom_point(alpha = 0.5) +
      geom_vline(aes(xintercept = as.numeric(plt_median_x[1])), linetype = "dashed", size = 0.25) +
      annotate("text", x = as.numeric(plt_median_x[1]) + 3, y = 75, label = "VA Median", angle = -90, size = 3, color = "#808080") +
      geom_hline(aes(yintercept = as.numeric(plt_median_y[1])), linetype = "dashed", size = 0.25) +
      scale_color_manual(values = my_colors) +
      guides(size = "none")
      
      ggplotly(plt)

    
  })
  
  
  
  
}

shinyApp(ui, server)
