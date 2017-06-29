library(shiny)
library(leaflet)
library(rgdal)

tLines <- rgdal::readOGR(dsn = "/Users/rupayanneogy/Downloads/mbta_rapid_transit", layer = "MBTA_ARC")
tLines <- spTransform(tLines, CRS("+proj=longlat +datum=WGS84 +no_defs"))

tStops <- readOGR(dsn = "/Users/rupayanneogy/Downloads/mbta_rapid_transit/",
                  layer = "MBTA_NODE") %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))


ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session){
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # centering the view on a specific location (Boston)
      setView(lng = -71.0589, lat = 42.4, zoom = 12) %>%
      addPolylines(data = tLines, weight = 4, color = ~tolower(LINE), fill = FALSE,
                   opacity = 1) %>%
      addCircleMarkers(data = tStops, label = ~STATION, color = ~tolower(LINE),
                       fillOpacity = 0.5, weight = 3, radius = 8)
  })
}

shinyApp(ui,server)