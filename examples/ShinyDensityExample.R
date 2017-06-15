library(shiny)
library(leaflet)
library(jsonlite)

# Police zones
policeZoneLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/bfe3e93c27004a69921b629b92cd1f9f_0.geojson"
policeZoneData <- readLines(policeZoneLink) %>% paste(collapse = "\n")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%")
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.Watercolor,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addGeoJSON(policeZoneData, weight = 3, color = "red", fill = FALSE) %>%
      fitBounds(-71.5, 42, -70.5, 42.5)
  })
}

shinyApp(ui, server)