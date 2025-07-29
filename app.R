# Global ----

library(bslib)
library(bsplus)
library(leaflet)
library(plotly)
library(reactable)
library(reactablefmtr)
library(shiny)
library(sf)
library(tidyverse)

# Read in HTML
header <- HTML(readLines("html/header"))
footer <- HTML(readLines("html/footer"))

# Setup variables 
my_choices = list(
  "Eviction Measures" = list(
    "Filing Rate" = "eviction_rate",  
    "Eviction Judgment Rate" = "judgment_rate",
    "Percent Filed by Businesses" = "pct_cases_business", 
    "Percent Serial Filings" = "pct_serial",
    "Percent Default Rulings" = "pct_default"
  ),
  "Population Measures (2018-2022 ACS)" = list(
    "Percent Renting Households" = "pct_rental_units",        
    "Median Rent" = "med_gross_rent",                             
    "Percent Cost-Burdened Renters" = "pct_burdened",         
    "Percent Poverty" = "pct_pov",
    "Percent White" = "pct_white",                            
    "Percent Black" = "pct_black",                            
    "Percent Hispanic or Latino" = "pct_hispanic",
    "Percent Minoritized" = "pct_nonwhite",
    "Rent Exploitation Ratio" = "exploit",
    "Population Density" = "pop_density",
    "Mobility Rate" = "mobility_rate")           
  )

my_choices_flat = flatten(my_choices)

my_colors <- c("Southwest Virginia Legal Aid Society" = "#E31A1C",
               "Legal Aid Society of Roanoke Valley" = "#FDBF6F",
               "Blue Ridge Legal Services" = "#1F78B4",
               "Virginia Legal Aid Society" = "#743089",
               "Central Virginia Legal Aid Society" = "#B2DF8A",
               "Legal Services of Northern Virginia" = "#FF7F00",
               "Legal Aid Works" = "#FB9A99",
               "Legal Aid Society of Eastern Virginia" = "#A6CEE3")

my_years <- list(
  "2018-2019 (pre-COVID)" = "2018-2019", 
  "2020-2021 (COVID)" = "2020-2021", 
  "2022-2023 (post-COVID)" = "2022-2023")

# Read data 
zip <- readRDS("data/app_data/zip.RDS") 
county <- readRDS("data/app_data/county.RDS")
lasa <- readRDS("data/app_data/lasa.RDS")
defs <- read_csv("data/eviction_definitions.csv")

defs_choices <- defs$variable %>%
  set_names(defs$definition)

# Create locality variable for popup
zip <- zip %>% unite("locality", c("zip", "approx_city"), remove = F, sep = ", ")
county$locality <- county$NAME
lasa$locality <- lasa$legal_aid_service_area

# UI ----

ui <- fluidPage(
  includeCSS("www/styles.css"),
  uiOutput("header"),
  titlePanel(tagList(
    span("Exploring Eviction Geographies", 
         span(actionButton("instr", "", icon = icon("circle-question")),
              style = "position:absolute;right:2em;")
    )
  ),
  windowTitle = "Exploring Eviction Geographies"
  ),
  hr(),
  fluidRow(column(width = 10, "Visualize and compare eviction trends and landlord behaviors across Virginia.")),
  br(),
  sidebarLayout(
    sidebarPanel(
      selectInput("yr", "Timeframe:", choices = my_years, selected = my_years[[3]]) %>%
        shinyInput_label_embed(
          shiny_iconlink() %>%
            bs_embed_popover(
              title = "Timeframe", content = "Select one of the three timeframes listed to explore 
              eviction trends either before, during, or after the COVID epidemic.", placement = "left", trigger = "focus"
            )
        ),
      selectInput("geo", "Geography:", choices = c("Zipcode", "County", "Legal Aid Service Area"), selected = "County") %>%
        shinyInput_label_embed(
          shiny_iconlink() %>%
            bs_embed_popover(
              title = "Geography", content = "Select one of the three geographic groupings listed to explore 
              eviction trends by Zip Code, County, or Legal Aid Service Area.", placement = "left", trigger = "focus"
            )
        ),
      selectInput("var1", "Variable to Map:", choices = my_choices, selected = my_choices[[1]][1]) %>%
        shinyInput_label_embed(
          shiny_iconlink() %>%
            bs_embed_popover(
              title = "Variable to Map", content = "Select one of the measures to explore in the map. 
              These variables range from population measures from the 2018-2022 American Community Survey to 
              eviction trends derived from court records.", placement = "left", trigger = "focus"
            )
        ),
      selectInput("var2", "Variable to Compare:", choices = my_choices[2], selected = my_choices[[2]][4]) %>%
        shinyInput_label_embed(
          shiny_iconlink() %>%
            bs_embed_popover(
              title = "Variable to Compare", content = "Select a measure to see it's relationship to the variable being mapped. 
              A scatterplot of two variables is on the second tab.", placement = "left", trigger = "focus"
            )
        ),
      imageOutput("areas_img")),
    mainPanel(
      tabsetPanel(
        id = "tabset",
        tabPanel(
          title = "Explore",
          icon = icon("magnifying-glass"),
          br(),
          fluidRow(
            column(
              width = 2,
              align = "right",
              actionButton("mapinstr", "Map Instructions", icon = icon("info-circle"))
          )),
          hr(),
          leafletOutput("map"),
          hr(),
          h4(("Variable Definition:")),
          h5(textOutput("var1", inline = TRUE)),
          htmlOutput("def1", inline = TRUE),
          fluidRow()
          ),
        tabPanel(
          title = "Compare", 
          icon = icon("chart-line"),
          br(),
          fluidRow(
            column(
              width = 2,
              align = "right",
              actionButton("pltinstr", "Plot Instructions", icon = icon("info-circle"))
            )),
          hr(),
          h4(textOutput("var2", inline = TRUE)),
          plotlyOutput("plt"),
          hr(),
          h4(("Variable Definitions:")),
          htmlOutput("def2", inline = TRUE)),
        tabPanel(
          title = "Download",
          icon = icon("download"),
          h4("Preview:", textOutput("yr", inline = TRUE), "eviction data ", textOutput("geo", inline = TRUE)), 
          reactableOutput("tbl"),
          downloadButton("downloadData", "Download")),
        tabPanel(
          title = "About",
          icon = icon("lightbulb"),
          imageOutput("va_img", inline = TRUE),
          includeMarkdown("about.md"),
        ),
        tabPanel(
          title = "Data Notes",
          icon = icon("table"), 
          includeMarkdown("data_notes.md")
        )
      )
    )),
  use_bs_popover(),
  uiOutput("footer"))

# Server ----

server <- function(input, output, session) {
  
  output$header <- renderUI(header)
  output$footer <- renderUI(footer)

# Instruction modals ----
  
  observe({ 
    showModal( 
      modalDialog( 
        title = "Dashboard Instructions", 
        easy_close = TRUE, 
        HTML(paste0("Make selections in the sidebar on the left to explore eviction trends in Virginia.", "<br>", "<br>",
                    "Variables include data related to social and demographic characteristics, eviction outcomes, and landlord behaviors.")),
        footer = modalButton("Okay"))) 
  }) %>% 
    bindEvent(input$instr) 
  
  observe({ 
    showModal( 
      modalDialog( 
        title = "Map Instructions", 
        easy_close = TRUE, 
        HTML(paste0("The map below shows values for the selected ", "<b>", "Exploratory Variable ", "</b>", 
             "based on the selected ", "<b>", "Geography ", "</b>", "within the selected ", "<b>", "Timeframe", "</b>", ".",
             "<br>", "<br>", 
             "Click on the regions to view additional information, such as the name of the region and 
             the total number of rental units. Zoom in to see specific areas more closely.", "<br>",
             "<br>", "The definition of the selected variable is printed below the map.")),
        footer = modalButton("Okay"))) 
  }) %>% 
    bindEvent(input$mapinstr) 
  
  observe({ 
    showModal( 
      modalDialog( 
        title = "Plot Instructions", 
        easy_close = TRUE, 
        HTML(paste0("The scatter plot below shows the relationship between the selected ", "<b>", "Exploratory ", "</b>", "and", "<b>", 
             " Outcome ", "</b>", "variables during the selected ", "<b>", "Timeframe", "</b>", ".", "<br>", "<br>",
             "Each circle represents a ", "<b>", "Zip Code", "</b>", ", ", "<b>", "County", "</b>", ", or ", "<b>", "Legal Aid Service Area ", "</b>", 
             "depending on the selected geographic grouping. The size of each circle  is based on the total number of rental units, with 
             areas with more renters appearing larger and areas with fewer renters appearing smaller. The color of the circle is based 
             on the Legal Aid Service Area.", "<br>", "<br>", 
             "Hover over the circles for more information such as the name of the region and the total number of rental units.", "<br>", "<br>",
             "To identify possible correlations between two variables, look at the graph and ask: As the value of the Exploratory variable 
             on the horizontal line increases, does the value of the Outcome variable notably increase or decrease? It can also be useful 
             to identify certain regions that have extreme values on both measures.", "<br>", "<br>",
             "The definitions of the selected variables are printed below the plot.")),
        footer = modalButton("Okay"))) 
  }) %>% 
    bindEvent(input$pltinstr) 
  
  # Images ----
  output$areas_img <- renderImage({
    list(
      src = file.path("images/service_areas_full.png"),
      contentType = "image/png",
      width = "100%"
    )
  }, deleteFile = FALSE)
  
  output$va_img <- renderImage({
    list(src = file.path("images/va5.png"),
         contentType = "image/png",
         width = "600px"
    )
  }, deleteFile = FALSE)
  
  # Data based on user selections ----
  rv <- reactiveValues()
  observeEvent(c(input$geo, input$yr), {
    if (input$geo == "Zipcode") {
      rv$dat = zip
    } else if (input$geo == "County") {
      rv$dat = county
    } else {
      rv$dat = lasa
    }
    rv$dat = rv$dat %>% filter(yrs == input$yr)
  })
  
  # Definitions, titles, prefixes/suffixes, etc. ----
  observeEvent(c(input$var1, input$var2), {
    rv$title_str = paste0(as.character(names(my_choices_flat[my_choices_flat == input$var1])), " compared to ", 
                          as.character(names(my_choices_flat[my_choices_flat == input$var2])))
    
    rv$suf = ""
    rv$pre = ""
    rv$suf2 = ""
    rv$pre2 = ""
    if (grepl("(pct)", input$var1)) { rv$suf = "%" }
    if (grepl("med", input$var1)) { rv$pre = "$"}
    if (grepl("(pct)", input$var2)) { rv$suf2 = "%" }
    if (grepl("med", input$var2)) { rv$pre2 = "$"}
  })
  
  output$geo <- renderText({paste0("Grouped by ", tolower(input$geo))})
  
  output$def1 <- renderText({
    paste0("<b>", as.character(names(my_choices_flat[my_choices_flat == input$var1])), "</b>",
           ": ", as.character(names(defs_choices[defs_choices == input$var1])))
    })
  
  output$var2 <- renderText({ rv$title_str })
  output$var2b <- renderText({ rv$title_str })
  
  output$def2 <- renderText({
    paste0("<b>", as.character(names(my_choices_flat[my_choices_flat == input$var1])), "</b>",
           ": ", as.character(names(defs_choices[defs_choices == input$var1])),
           "<br>",
           "<br>",
           "<b>", as.character(names(my_choices_flat[my_choices_flat == input$var2])), "</b>",
           ": ", as.character(names(defs_choices[defs_choices == input$var2])))
  })
  
  output$geo <- renderText({
    paste0("by ", tolower(input$geo))
  })
  
  output$yr <- renderText({
    paste0(input$yr)
  })
  
  # Map ----
  pal <- colorNumeric(palette = "viridis", domain = NULL, reverse = TRUE,
                      na.color = NA)
  
  output$map <- renderLeaflet({
    
    rv$dat %>%
      st_transform(crs = 4326) %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%  
      addPolygons(stroke = TRUE,
                  weight = 0.5,
                  opacity = 1,
                  color = "black",
                  fillColor = ~pal(rv$dat[[input$var1]]),
                  fillOpacity = 0.5,
                  popup = paste0("<b>", as.character(names(my_choices_flat[my_choices_flat == input$var1])), ": ", "</b>", rv$pre,
                                 scales::comma(round(rv$dat[[input$var1]], digits = 2)), rv$suf, "<br>",
                                 "<b>","Rental Units: ", "</b>", scales::comma(rv$dat[["rental_units"]]), "<br>",
                                 "<b>", "Total Eviction Filings: ", "</b>", scales::comma(rv$dat[["total_filed"]]), "<br>",
                                 "<b>", "Region: ", "</b>", rv$dat[["locality"]]),
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
  
  # Plot ----
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
                      text = paste0("<b>", names(my_choices_flat[my_choices_flat == input$var1]), 
                                    ": ", "</b>", rv$pre, scales::comma(round(.data[[input$var1]], digits = 2)), rv$suf, "<br>",
                                    "<b>", names(my_choices_flat[my_choices_flat == input$var2]), 
                                    ": ", "</b>", rv$pre2, scales::comma(round(.data[[input$var2]], digits = 2)), rv$suf2, "<br>",
                                    "<b>", "Rental Units: ", "</b>", scales::comma(rental_units), "<br>",
                                    "<b>", "Total Eviction Filings: ", "</b>", scales::comma(total_filed), "<br>",
                                    "<b>", "Region: ", "</b>", locality))) +
      geom_point(alpha = 0.5) +
      scale_color_manual(values = my_colors) +
      scale_y_continuous(labels = scales::comma_format(prefix = rv$pre2, suffix = rv$suf2)) +
      scale_x_continuous(labels = scales::comma_format(prefix = rv$pre, suffix = rv$suf)) +
      guides(size = "none") +
      theme_bw() +
      theme(legend.position = "bottom") +
      labs(x = names(my_choices_flat[my_choices_flat == input$var1]),
           y = names(my_choices_flat[my_choices_flat == input$var2]),
           color = "Legal Aid Service Area")
    
    ggplotly(plt, tooltip = c("text"))
    
  })
  
  # Download----
  dl <- reactive({
    rv$dat %>%
      as_tibble() %>%
      select("Region" = locality, "Total Rental Units" = rental_units, "Percent Renting Households" = pct_rental_units, 
             "Total Evictions Filed" = total_filed, "Filing Rate" = eviction_rate, "Eviction Judgment Rate" = judgment_rate, 
             "Percent Serial Filings" = pct_serial, "Percent Filed by Businesses" = pct_cases_business, 
             "Percent Default Rulings" = pct_default, 
             "Median Rent" = med_gross_rent, "Rent Exploitation Ratio" = exploit, "Percent Cost-Burdened Renters" = 
               pct_burdened, "Percent White" = pct_white, "Percent Black" = pct_black, "Percent Hispanic" = pct_hispanic,  
             "Percent Minoritized" = pct_nonwhite, "Population Density" = pop_density) 
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      # Use the selected dataset as the suggested file name
      paste0("evictions_", gsub("-", "_", input$yr), "_by_", tolower(input$geo), ".csv")
    },
    content = function(file) {
      # Write the dataset to the `file` that will be downloaded
      write.csv(dl(), file)
    }
  )
  
  # Build table ---- 
  output$tbl <- renderReactable(
  
    rv$dat %>%
      as_tibble() %>%
      select("Region" = locality, "Total Rental Units" = rental_units, "Percent Renting Households" = pct_rental_units, 
             "Total Evictions Filed" = total_filed, "Filing Rate" = eviction_rate, "Eviction Judgment Rate" = judgment_rate, 
             "Percent Serial Filings" = pct_serial, "Percent Filed by Businesses" = pct_cases_business, 
             "Percent Default Rulings" = pct_default, 
             "Median Rent" = med_gross_rent, "Rent Exploitation Ratio" = exploit, "Percent Cost-Burdened Renters" = 
             pct_burdened, "Percent White" = pct_white, "Percent Black" = pct_black, "Percent Hispanic" = pct_hispanic,  
             "Percent Minoritized" = pct_nonwhite, "Population Density" = pop_density) %>%
      reactable(
        defaultColDef = colDef(
          align = "center",
          defaultSortOrder = "desc",
          headerStyle = list(background = "#f7f7f8"),
          format = colFormat(separators = TRUE, digits = 2),
          na = "NA"),
        defaultSorted = list(`Filing Rate` = "desc"),
        searchable = TRUE,
        columns = list(
          `Median Rent` = colDef(format = colFormat(prefix = "$", digits = 2, separators = TRUE)),
          `Percent Renting Households` = colDef(format = colFormat(suffix = "%", digits = 1)),
          `Percent Serial Filings` = colDef(format = colFormat(suffix = "%", digits = 1)),
          `Percent Filed by Businesses` = colDef(format = colFormat(suffix = "%", digits = 1)),
          `Percent Default Rulings` = colDef(format = colFormat(suffix = "%", digits = 1)),
          `Percent Cost-Burdened Renters` = colDef(format = colFormat(suffix = "%", digits = 1)),
          `Percent White` = colDef(format = colFormat(suffix = "%", digits = 1)),
          `Percent Black` = colDef(format = colFormat(suffix = "%", digits = 1)),
          `Percent Hispanic` = colDef(format = colFormat(suffix = "%", digits = 1)),
          `Percent Minoritized` = colDef(format = colFormat(suffix = "%", digits = 1)),
          `Total Rental Units` = colDef(format = colFormat(digits = 0, separators = TRUE)),
          `Total Evictions Filed` = colDef(format = colFormat(digits = 0, separators = TRUE)),
          `Population Density` = colDef(format = colFormat(digits = 0, separators = TRUE))),
        bordered = TRUE,
        highlight = TRUE,
        defaultPageSize = 7
      ) 
  )
}

shinyApp(ui, server)