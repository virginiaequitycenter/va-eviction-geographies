# Global ----

library(leaflet)
library(plotly)
library(reactable)
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

areas_sf <- readRDS("data/areas_sf.RDS")
zcta_rent <- readRDS("data/zcta_rent.RDS")
county_rent <- readRDS("data/county_rent.RDS")
lasa_rent <- readRDS("data/lasa_rent.RDS")

# UI ----

ui <- fluidPage(
  titlePanel("Exploring Eviction Geographies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("geo", "Geographic Level", choices = c("Zipcode", "County", "Legal Aid Service Area"), selected = "County"),
      selectInput("var1", "Exploratory Variable:", choices = my_choices, selected = my_choices[4]),
      selectInput("var2", "Comparison Variable:", choices = my_choices, selected = my_choices[2]),
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
        tabPanel(
          title = "Table",
          downloadButton("downloadData", "Download"),
          reactableOutput("tbl"))
    ))))

# Server ----

server <- function(input, output, session) {
  
  rv <- reactiveValues()
  observeEvent(input$geo, {
    if (input$geo == "Zipcode") {
      rv$dat = zcta_rent
    } else if (input$geo == "County") {
      rv$dat = county_rent
    } else {
      rv$dat = lasa_rent
    }
  })
  
  output$img <- renderImage({
    list(
      src = file.path("service_areas_full.png"),
      contentType = "image/png",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  
  pal <- colorNumeric(palette = "viridis", domain = NULL, reverse = TRUE,
                      na.color = NA)
  
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
      addPolygons(data = areas_sf %>% st_transform(crs = 4326),
                  fillColor = "transparent",
                  color = "blue", 
                  fillOpacity = 0.8,
                  group="Legal Aid Service Area",
                  popup=NULL,
                  weight = 1) %>%
      addLegend("topright",
                pal = pal,
                values = ~ zcta_rent[[input$var1]],
                title = names(my_choices[my_choices == input$var1]),
                labFormat = labelFormat(suffix = "%"),
                opacity = 1) %>%
      addLayersControl(
        overlayGroups =c ("Legal Aid Service Area"),
        options = layersControlOptions(collapsed=FALSE)
      )
  })
  
  output$var2 <- renderText({
     
    paste0(as.character(names(my_choices[my_choices == input$var1])), " compared to ", 
           as.character(names(my_choices[my_choices == input$var2])))
    
  })
  
  output$plt <- renderPlotly({
    
    plt_median_x <- median(zcta_rent[[input$var1]], na.rm = TRUE)
    
    plt_median_y <- median(zcta_rent[[input$var2]], na.rm = TRUE)
      
    
    plt_dat <- zcta_rent %>%
      filter(zcta_rent[[input$var1]] > 0,
             zcta_rent[[input$var2]] > 0,
             !is.na(legal_aid_service_area))
    
    plt <- ggplot(plt_dat, aes(x = .data[[input$var1]], y = .data[[input$var2]], 
                               color = legal_aid_service_area, size = total_pop,
                               text = paste0("Zipcode: ", GEOID, "<br>", 
                                            "Region: ", primary_city, ", ", county, "<br>",
                                            "Estimated Population: ", total_pop, "<br>",
                                            names(my_choices[my_choices == input$var1]), ": ", round(.data[[input$var1]]), "%", "<br>",
                                            names(my_choices[my_choices == input$var2]), ": ", round(.data[[input$var2]]), "%", "<br>"))) +
      # annotate("text", x = 80, y = plt_median_y - 3,
      #          label = "State Median",  color = "#808080") +
      geom_hline(aes(yintercept = plt_median_y), linetype = "dashed", linewidth = 0.1) +
      geom_vline(aes(xintercept = plt_median_x), linetype = "dashed", linewidth = 0.1) +
      geom_point(alpha = 0.5) +
      scale_color_manual(values = my_colors) +
      scale_y_continuous(labels = function(x) paste0(x, "%")) +
      scale_x_continuous(labels = function(x) paste0(x, "%")) +
      guides(size = "none") +
      labs(x = names(my_choices[my_choices == input$var1]),
           y = names(my_choices[my_choices == input$var2]),
           color = "Legal Aid Service Area")
      
      ggplotly(plt, tooltip = c("text"))
    
  })
  
  data <- reactive({
    zcta_rent %>%
      as_tibble() %>%
      select(GEOID, primary_city, county, legal_aid_service_area,total_pop, total_renters, 
             total_burdened, med_gross_rent, med_hh_income,median_principal, total_filed, 
             total_default, total_immediate, n_d_attorney,
             cases_plaintiff_business) %>%
      rename("Zipcode" = GEOID,
             "City" = primary_city,
             "County" = county,
             "Service Area" = legal_aid_service_area,
             "Total Pop." = total_pop,
             "Rental Pop." = total_renters,
             "Cost-Burdened Renters" = total_burdened,
             "Median Rent" = med_gross_rent,
             "Median Household Income" = med_hh_income,
             "Median Case Amount" = median_principal,
             "Evictions Filed" = total_filed,
             "Default Judgments" = total_default,
             "Immediate Possession" = total_immediate,
             "Cases with Defendent Attorney" = n_d_attorney,
             "Cases Filed by Businesses" = cases_plaintiff_business)

      
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("evictions_zipcode_", Sys.Date(), ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(data(), file)
    }
  )
  
  output$tbl <- renderReactable(
    
    zip_tbl <- zcta_rent %>%
      as_tibble() %>%
      select(GEOID, primary_city, county, legal_aid_service_area,total_pop, total_renters, 
             total_burdened, med_gross_rent, med_hh_income,median_principal, total_filed, 
             total_default, total_immediate, n_d_attorney,
             cases_plaintiff_business) %>%
      rename("Zipcode" = GEOID,
             "City" = primary_city,
             "County" = county,
             "Service Area" = legal_aid_service_area,
             "Total Pop." = total_pop,
             "Rental Pop." = total_renters,
             "Cost-Burdened Renters" = total_burdened,
             "Median Rent" = med_gross_rent,
             "Median Household Income" = med_hh_income,
             "Median Case Amount" = median_principal,
             "Evictions Filed" = total_filed,
             "Default Judgments" = total_default,
             "Immediate Possession" = total_immediate,
             "Cases with Defendent Attorney" = n_d_attorney,
             "Cases Filed by Businesses" = cases_plaintiff_business) %>%
      reactable(
        defaultColDef = colDef(
          align = "center",
          defaultSortOrder = "desc",
          headerStyle = list(background = "#f7f7f8"),
          format = colFormat(separators = TRUE)),
        defaultSorted = list(`Rental Pop.` = "desc"),
        filterable = TRUE,
        searchable = TRUE,
        columns = list(
        `Zipcode` = colDef(format = colFormat(separators = FALSE)),
        `Median Rent` = colDef(format = colFormat(prefix = "$", digits = 2)),
        `Median Household Income` = colDef(format = colFormat(prefix = "$", digits = 2)),
        `Median Case Amount` = colDef(format = colFormat(prefix = "$", digits = 2))),
        bordered = TRUE,
        highlight = TRUE,
        defaultPageSize = 5
    
    
  )
  )
  
  
  
  
}

shinyApp(ui, server)
