library(shiny)
library(leaflet)

# Police zones
waterRiseLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/eebd3daed05a45678894db30d9bf0cfb_0.geojson"
waterRiseData <- readLines(waterRiseLink) %>% paste(collapse = "\n")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%")
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addGeoJSON(waterRiseData, weight = 3, color = "red", fill = FALSE) %>%
      fitBounds(-71.5, 42, -70.5, 42.5)
  })
}

shinyApp(ui, server)