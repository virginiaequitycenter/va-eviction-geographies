# Global ----

library(leaflet)
library(plotly)
library(reactable)
library(shiny)
library(sf)
library(tidyverse)

my_choices = list(
  "Population Measures" = list(
    "Households" = "housing_units",                               
    "Renting Households" = "rental_units",                        
    "Percent Renting Households" = "percent_rental_units",        
    "Median Rent" = "med_gross_rent",                             
    "Median Household Income" = "med_hh_income",                  
    "Cost-Burdened Renters" = "total_burdened",                   
    "Percent Cost-Burdened Renters" = "percent_burdened",         
    "Percent Poverty" = "percent_pov",
    "Percent White" = "percent_white",                            
    "Percent Black" = "percent_black",                            
    "Percent American Indian and Alaska Native" = "percent_aian", 
    "Percent Asian" = "percent_asian",                             
    "Percent Native Hawaiian and Other Pacific Islander" = "percent_nhpi", 
    "Percent Another Race not Listed" = "percent_other",                  
    "Percent Two or More Races" = "percent_two",                  
    "Percent Hispanic or Latino" = "percent_hispanic"),           
  "Eviction Measures" = list(
    "Eviction Cases" = "total_filed",                             
    "Eviction Cases per Rental Unit" = "filed_unit",              
    "Eviction Cases by Business" = "cases_plaintiff_business",    
    "Percent Filed by Businesses" = "percent_plaintiff_business", 
    "Median Eviction Amount" = "median_principal",                
    "Eviction Judgments" = "total_judgment",                      
    "Eviction Judgments per Rental Unit" = "judgment_rate",       
    "Percent Judgment Ruled" = "percent_judgment",                
    "Rent Exploitation Ratio" = "exploit"
  ))

my_choices_flat = flatten(my_choices)

my_colors <- c("Southwest Virginia Legal Aid Society" = "#E31A1C",
               "Legal Aid Society of Roanoke Valley" = "#FDBF6F",
               "Blue Ridge Legal Services" = "#1F78B4",
               "Virginia Legal Aid Society" = "#743089",
               "Central Virginia Legal Aid Society" = "#B2DF8A",
               "Legal Services of Northern Virginia" = "#FF7F00",
               "Legal Aid Works" = "#FB9A99",
               "Legal Aid Society of Eastern Virginia" = "#A6CEE3")

# Read data 
zcta_rent <- readRDS("data/zcta_rent.RDS") 
county_rent <- readRDS("data/county_rent.RDS")
lasa_rent <- readRDS("data/lasa_rent.RDS")
defs <- read_csv("data/eviction_definitions.csv")

defs_choices <- defs$variable %>%
  set_names(defs$definition)

# Fix names - for popups
zcta_rent <- zcta_rent %>% unite("locality", c("GEOID", "city", "locality"), remove = F, sep = ", ")
county_rent$locality <- county_rent$NAME
lasa_rent$locality <- lasa_rent$legal_aid_service_area

# UI ----

ui <- fluidPage(
  titlePanel("Exploring Eviction Geographies"),
  sidebarLayout(
    sidebarPanel(
      selectInput("geo", "Geographic Grouping:", choices = c("Zipcode", "County", "Legal Aid Service Area"), selected = "County"),
      selectInput("var1", "Exploratory Variable:", choices = my_choices, selected = my_choices[[2]][1]),
      selectInput("var2", "Comparison Variable:", choices = my_choices, selected = my_choices[[1]][2]),
      imageOutput("img")),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel(
          title = "Explore",
          icon = icon("map"),
          h4(textOutput("var1", inline = TRUE)),
          textOutput("def1", inline = TRUE),
          leafletOutput("map")),
        tabPanel(
          title = "Compare", 
          icon = icon("chart-line"),
          h4(textOutput("var2", inline = TRUE)),
          plotlyOutput("plt"),
          h4(("Variable Definitions")),
          htmlOutput("def2", inline = TRUE)),
        tabPanel(
          title = "Test",
          h4("Hold for Rent Exploitation Data Story"),
          icon = icon("pen")),
        tabPanel(
          title = "Download",
          icon = icon("table"),
          h4(textOutput("var2b", inline = TRUE)),
          h6(textOutput("geo", inline = TRUE)),
          reactableOutput("tbl"),
          downloadButton("downloadData", "Download"))
        )
    )))

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
  
  observeEvent(c(input$var1, input$var2), {
    rv$title_str = paste0(as.character(names(my_choices_flat[my_choices_flat == input$var1])), " compared to ", 
                          as.character(names(my_choices_flat[my_choices_flat == input$var2])))
    
    rv$suf = ""
    rv$pre = ""
    rv$suf2 = ""
    rv$pre2 = ""
    if (grepl("(percent)", input$var1)) { rv$suf = "%" }
    if (grepl("med", input$var1)) { rv$pre = "$"}
    if (grepl("(percent)", input$var2)) { rv$suf2 = "%" }
    if (grepl("med", input$var2)) { rv$pre2 = "$"}
  })
  
  output$img <- renderImage({
    list(
      src = file.path("images/service_areas_full.png"),
      contentType = "image/png",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  pal <- colorNumeric(palette = "viridis", domain = NULL, reverse = TRUE,
                      na.color = NA)
  
  output$var1 <- renderText({
    as.character(names(my_choices_flat[my_choices_flat == input$var1]))
  })
  
  output$geo <- renderText({paste0("Grouped by ", tolower(input$geo))})
  
  output$def1 <- renderText({
    as.character(names(defs_choices[defs_choices == input$var1]))
  })
  
  output$map <- renderLeaflet({
  
    rv$dat %>%
      st_transform(crs = 4326) %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%  
      addPolygons(stroke = TRUE,
                  weight = 0.5,
                  opacity = 1,
                  color = "black",
                  fillColor = ~pal(rv$dat[[input$var1]]),
                  fillOpacity = 0.5,
                  popup = paste0(as.character(names(my_choices_flat[my_choices_flat == input$var1])), ": ", rv$pre,
                                 scales::comma(round(rv$dat[[input$var1]])), rv$suf, "<br>",
                                 "Total Population: ", scales::comma(rv$dat[["total_pop"]]), "<br>",
                                 "Region: ", rv$dat[["locality"]]),
                  highlightOptions = highlightOptions(
                    fillOpacity = 1,
                    bringToFront = FALSE)) %>%
      addLegend("topright",
                pal = pal,
                values = ~ rv$dat[[input$var1]],
                title = names(my_choices_flat[my_choices_flat == input$var1]),
                labFormat = labelFormat(suffix = rv$suf, prefix = rv$pre),
                opacity = 1) 
  })
  
  output$var2 <- renderText({ rv$title_str })
  output$var2b <- renderText({ rv$title_str })
  
  output$def2 <- renderText({
    paste0("<b>", "X Axis - ", as.character(names(my_choices_flat[my_choices_flat == input$var1])), "</b>",
           ": ", as.character(names(defs_choices[defs_choices == input$var1])),
           "<br>",
           "<br>",
           "<b>", "Y Axis - ", as.character(names(my_choices_flat[my_choices_flat == input$var2])), "</b>",
           ": ", as.character(names(defs_choices[defs_choices == input$var2])))
  })
  
  output$plt <- renderPlotly({
    
    plt_median_x <- median(rv$dat[[input$var1]], na.rm = TRUE)
    plt_median_y <- median(rv$dat[[input$var2]], na.rm = TRUE)
      
    plt_dat <- rv$dat %>%
      filter(rv$dat[[input$var1]] > 0,
             rv$dat[[input$var2]] > 0,
             !is.na(legal_aid_service_area))
    
    plt <- ggplot(plt_dat, 
                  aes(x = .data[[input$var1]], y = .data[[input$var2]], 
                      color = legal_aid_service_area, size = total_pop,
                      text = paste0(names(my_choices_flat[my_choices_flat == input$var1]), 
                                    ": ", rv$pre, scales::comma(round(.data[[input$var1]])), rv$suf, "<br>",
                                    names(my_choices_flat[my_choices_flat == input$var2]), 
                                    ": ", rv$pre2, scales::comma(round(.data[[input$var2]])), rv$suf2, "<br>",
                                    "Total Population: ", scales::comma(total_pop), "<br>",
                                    "Region: ", locality))) +
      geom_hline(aes(yintercept = plt_median_y, text = "State Median"), linetype = "dashed", linewidth = 0.1) +
      geom_vline(aes(xintercept = plt_median_x, text = "State Median"), linetype = "dashed", linewidth = 0.1) +
      geom_point(alpha = 0.5) +
      scale_color_manual(values = my_colors) +
      scale_y_continuous(labels = scales::comma_format(prefix = rv$pre2, suffix = rv$suf2)) +
      scale_x_continuous(labels = scales::comma_format(prefix = rv$pre, suffix = rv$suf)) +
      guides(size = "none") +
      labs(x = names(my_choices_flat[my_choices_flat == input$var1]),
           y = names(my_choices_flat[my_choices_flat == input$var2]),
           color = "Legal Aid Service Area")
      
      ggplotly(plt, tooltip = c("text"))
    
  })
  
  data <- reactive({
    rv$dat %>%
      as_tibble() %>%
      select(locality, legal_aid_service_area, total_pop, total_renters, 
             total_burdened, med_gross_rent, med_hh_income,median_principal, total_filed, 
             total_default, total_immediate, n_d_attorney,
             cases_plaintiff_business) %>%
      rename("Region" = locality,
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
  
    output$geo <- renderText({
    paste0("Grouped by ", tolower(input$geo))
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
    
    tbl <- rv$dat %>%
      as_tibble() %>%
      select(locality, legal_aid_service_area, total_pop, total_renters, 
             total_burdened, med_gross_rent, med_hh_income,median_principal, total_filed, 
             total_default, total_immediate, n_d_attorney,
             cases_plaintiff_business) %>%
      rename("Region" = locality,
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
  
  output$process <- renderImage({
    list(
      src = file.path("images/process.png"),
      contentType = "image/png",
      width = "80%"
    )
  }, deleteFile = FALSE)
}

shinyApp(ui, server)
