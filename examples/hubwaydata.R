library(shiny)
library(leaflet)
library(jsonlite)

#neighborhood <- read.csv("http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.csv")
stations <- read.csv("https://s3.amazonaws.com/hubway-data/Hubway_Stations_2011_2016.csv")
trips2012 <- read.csv("https://s3.amazonaws.com/hubway-data/hubway_Trips_2012.csv")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session){
  output$map <- renderLeaflet({
    leaflet(neighborhood) %>%
      addTiles() %>%
      fitBounds(-71.5, 42, -70.5, 42.5)
  })
}

shinyApp(ui,server)

