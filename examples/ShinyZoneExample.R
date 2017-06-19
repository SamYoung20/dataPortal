library(shiny)
library(leaflet)

# Police zones
policeZoneLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/9a3a8c427add450eaf45a470245680fc_5.geojson"
policeZoneData <- readLines(policeZoneLink) %>% paste(collapse = "\n")
# Police Stations
policeStationLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/e5a0066d38ac4e2abbc7918197a4f6af_6.geojson"
policeStationData <- readLines(policeStationLink) %>% paste(collapse = "\n")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                #             value = range(quakes$mag), step = 0.1
                # ),
                selectInput("datasets", "Data Set", c("Police Data")),
                checkboxInput("stationCheckbox", "Police Stations", TRUE),
                checkboxInput("zoneCheckbox", "Police Zones", TRUE)
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      fitBounds(-71.5, 42, -70.5, 42.5)
  });
  
  observe({
    m <- leafletProxy("map") %>%
      removeGeoJSON("policeZones") %>%
      addGeoJSON(ifelse(input$zoneCheckbox, policeZoneData, "{}"), layerId = "policeZones", weight = 2, color = "blue",
                 fill = TRUE)
  })
  
  observe({
    leafletProxy("map") %>%
      removeGeoJSON("policeStations") %>%
      addGeoJSON(ifelse(input$stationCheckbox, policeStationData, "{}"), layerId = "policeStations")
  })
}

shinyApp(ui, server)