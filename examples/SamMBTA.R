library(shiny)
library(leaflet)
library(rgdal)

#To run this you need to change the path for the MBTA DATA on your machine.
# download it from here http://www.mass.gov/anf/research-and-tech/it-serv-and-support/application-serv/office-of-geographic-information-massgis/datalayers/mbta.html



tLines <- rgdal::readOGR(dsn = "./mbta_rapid_transit", layer = "MBTA_ARC")
tLines <- spTransform(tLines, CRS("+proj=longlat +datum=WGS84 +no_defs"))

tStops <- readOGR(dsn = "./mbta_rapid_transit",
                  layer = "MBTA_NODE") %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
boundaries <- rgdal::readOGR("./data/City_of_Boston_Boundary.geojson", "OGRGeoJSON")

neighborhoods <- read.csv("neighborhoodData.csv")
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session){
  output$map <- renderLeaflet({
    leaflet(neighborhoods) %>%
      addProviderTiles(providers$CartoDB.Positron)  %>%
      # centering the view on a specific location (Boston)
      setView(lng = -71.0589, lat = 42.4, zoom = 12) %>%
      addCircles(~lng, ~lat, popup= ~Name, weight = 1, radius=250, color="#000000", stroke = TRUE, fillOpacity = 1) %>%
      addPolylines(data = tLines, weight = 4, color = ~tolower(LINE), fill = FALSE,
                   opacity = 1) %>%
      addCircleMarkers(data = tStops, label = ~STATION, color = ~tolower(LINE),
                       fillOpacity = 0.5, weight = .5, radius = 5)
       
  })
}

shinyApp(ui,server)