library(shiny)
library(leaflet)

# IF YOU WANT TO CHANGE DATA SET, CHANGE LINK BELOW TO .csv LINK
markerData <- read.csv("http://bostonopendata-boston.opendata.arcgis.com/datasets/5409b7735d384798b2a360aa47c9b128_0.csv")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session){
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # centering the view on a specific location (Boston)
      fitBounds(-71.5, 42, -70.5, 42.5) %>%
      # AND ADJUST PARAMETERS SO THAT lng, lat, AND popup MATCH WITH THE CORRECT COLUMN
      # OF THE DATA SET
      addCircleMarkers(data = markerData, lng = ~X, lat = ~Y, color = "purple",
                       radius = 10, stroke = FALSE,
                       fillOpacity = 0.7, group = "markers",
                       popup = ~paste(sep = "<br/>",
                                     paste("<b>",Park_Name,"</b>"),
                                     Address_Te,
                                     Neighborho
                                     ),
                       clusterOptions = markerClusterOptions()
                       )
  })
}

shinyApp(ui, server)