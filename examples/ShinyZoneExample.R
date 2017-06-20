library(shiny)
library(leaflet)
require(spatialEco)

# Police zones
policeZoneLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/9a3a8c427add450eaf45a470245680fc_5.geojson"
policeZoneJson <- rgdal::readOGR(policeZoneLink, "OGRGeoJSON")
# Police Stations
policeStationLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/e5a0066d38ac4e2abbc7918197a4f6af_6.geojson"
policeStationJson <- rgdal::readOGR(policeStationLink, "OGRGeoJSON")
# Polling Locations
pollingLocationLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/f7c6dc9eb6b14463a3dd87451beba13f_5.geojson"
pollingJson <- rgdal::readOGR(pollingLocationLink, "OGRGeoJSON")
pts.poly <- point.in.poly(pollingJson, policeZoneJson)
numPollsInZones <- tapply(pts.poly@data$DISTRICT, pts.poly@data$ID, FUN=length)

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
      addLegend("bottomright", pal = colorNumeric(
        palette = "YlOrRd",
        domain = numPollsInZones
      ), values = numPollsInZones, title = "Polling Stations In Police District",
      opacity = 1) %>%
      fitBounds(-71.5, 42, -70.5, 42.5)
  });
  
  observe({
    m <- leafletProxy("map") %>%
      clearShapes()
    if(input$zoneCheckbox){
        m %>% addPolygons(data = policeZoneJson, weight = 2, color = "blue",
                          fill = TRUE, label = paste(as.character(numPollsInZones), " polling stations"), 
                          fillColor = colorQuantile("YlOrRd", numPollsInZones)(numPollsInZones))
    }
  })
  
  
  
  observe({
    proxy <- leafletProxy("map") %>%
      clearMarkers()
    if(input$stationCheckbox){
      proxy %>% addCircleMarkers(data = policeStationJson, color = "purple",
                               radius = 5, stroke = FALSE, label = ~NAME,
                               fillOpacity = 0.7)
    }
  })
}

shinyApp(ui, server)