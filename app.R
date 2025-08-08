require(readr) #read_excel
require(dplyr)
require(leaflet)
require(shiny)
require(scales)

ui <- fluidPage(
  
  # App title ----
  titlePanel("Facts about the world countries, By: Ravi Vaid"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Selector for choosing model ----
      selectInput(inputId = "fact_type",
                  label = "Choose a fact to display:",
                  choices = c("population", "land_area", "fertility_rate","median_age",
                              "urban_pop_percentage","world_share")
      )
      
    ),
    # Main panel for displaying outputs ----
    mainPanel(
      tabsetPanel(
        type="tabs",
        tabPanel(
          "Country facts",
          # Output: Plot ----
          leafletOutput(outputId = "display_plot",width="1000px",height="600px")
        ),
        tabPanel(
          "Data",
          DT::dataTableOutput("raw_data")
        )
      )
    )
  ))

server <- function(input, output) {
  
  
  
  world_data_geo = read_csv("https://raw.githubusercontent.com/ravivaidazad/WorldFactsDashboard/main/world_data_geo.csv")
  
  output$display_plot <- renderLeaflet({
    
    label =
      switch(input$fact_type,
             "population" = format(world_data_geo$pop,big.mark=",",scientific=FALSE),
             "land_area" = format(world_data_geo$land_area, big.mark=",",scientific=FALSE),
             "fertility_rate" = world_data_geo$fert_rate,
             "median_age" = world_data_geo$med_age,
             "urban_pop_percentage" = percent(as.numeric(world_data_geo$urban_pop_per),accuracy = 0.1),
             "world_share" = world_data_geo$world_share
      )
    
    
    require(leaflet)
    leaflet(data = world_data_geo) |> 
      addTiles() |>
      addMarkers(~longitude, 
                 ~latitude,
                 label=label) |>
      addProviderTiles(providers$Esri.WorldStreetMap) %>%
      setView(lng = 0, lat = 0, zoom = 2)  # World view
    
    
  }
  )
  
  # render data table to ui
  output$raw_data <- DT::renderDT(
    world_data_geo
  )
  
}

# -------------------------
shinyApp(ui, server)


