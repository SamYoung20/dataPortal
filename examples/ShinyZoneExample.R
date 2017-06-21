library(shiny)
library(leaflet)
library(rgdal)
library(spatialEco)

# Police zones
policeZoneLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/9a3a8c427add450eaf45a470245680fc_5.geojson"
policeZoneFile <- "./data/policeZone.rds" 
# Police Stations
policeStationLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/e5a0066d38ac4e2abbc7918197a4f6af_6.geojson"
policeStationFile <- "./data/policeStation.rds" 
# Polling Locations
pollingLocationLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/f7c6dc9eb6b14463a3dd87451beba13f_5.geojson"
pollingLocationFile <- "./data/pollingLocation.rds"

policeZoneJson <- rgdal::readOGR(policeZoneLink, "OGRGeoJSON")
policeStationJson <- rgdal::readOGR(policeStationLink, "OGRGeoJSON")
pollingJson <- rgdal::readOGR(pollingLocationLink, "OGRGeoJSON")

# if(file.exists(policeZoneFile)){
#   policeZoneJson <- readRDS(policeZoneFile)
# }else{
#   policeZoneJson <- rgdal::readOGR(policeZoneLink, "OGRGeoJSON")
#   saveRDS(policeZoneJson, policeZoneFile)
# }
# 
# if(file.exists(policeStationFile)){
#   policeStationJson <- readRDS(policeStationFile)
# }else{
#   policeStationJson <- rgdal::readOGR(policeStationLink, "OGRGeoJSON")
#   saveRDS(policeStationJson, policeStationFile)
# }
# 
# if(file.exists(pollingLocationFile)){
#   pollingJson <- readRDS(policeStationFile)
# }else{
#   pollingJson <- rgdal::readOGR(pollingLocationLink, "OGRGeoJSON")
#   saveRDS(pollingJson, pollingLocationFile)
# }





# functions to get number of locations in each zone
pts.poly <- point.in.poly(pollingJson, policeZoneJson)
numPollsInZones <- tapply(pts.poly@data$DISTRICT, pts.poly@data$ID, FUN=length)


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                #             value = range(quakes$mag), step = 0.1
                # ),
                selectInput("datasets", "Data Set", c("Police and Polls", "Green Neighborhoods")),
                checkboxInput("stationCheckbox", "Police Stations", TRUE),
                checkboxInput("zoneCheckbox", "Police Zones", TRUE)
  )
)

server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      # the styling of the map itself
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # the legend for the shading of the zones
      addLegend("bottomright", pal = colorNumeric(
        palette = "YlOrRd",
        domain = numPollsInZones
      ), values = numPollsInZones, title = "Polling Stations In Police District",
      opacity = 1) %>%
      # centering the view on a specific location (Boston)
      fitBounds(-71.5, 42, -70.5, 42.5) %>%
      # adding the markers
      addCircleMarkers(data = policeStationJson, color = "purple",
                       radius = 5, stroke = FALSE, label = ~NAME,
                       fillOpacity = 0.7, group = "stations") %>%
      # adding the zones
      addPolygons(data = policeZoneJson, weight = 2, color = "blue",
                  fill = TRUE, label = paste(as.character(numPollsInZones), " polling stations"), 
                  fillColor = colorQuantile("YlOrRd", numPollsInZones)(numPollsInZones),
                  group = "zones")
  });
  
  # logic to show or hide different groups
  observe({
    proxy <- leafletProxy("map") 
    if (input$stationCheckbox){
      proxy %>% showGroup("stations")
    }else{
      proxy %>% hideGroup("stations")
    }
  })
  
  observe({
    m <- leafletProxy("map")
    if (input$zoneCheckbox){
      m %>% showGroup("zones")
    }else{
      m %>% hideGroup("zones")
    }
  })
  
  # observe({
  #   if(input$datasets == "Police and Polls"){
  #     policeZoneJson = rgdal::readOGR(policeZoneLink, "OGRGeoJSON")
  #   }else if(input$datasets == "Green Neighborhoods"){
  #     policeZoneJson = rgdal::readOGR(neighborhoodLink, "OGRGeoJSON")
  #   }
  #   print(input$datasets)
  # })
}

shinyApp(ui, server)