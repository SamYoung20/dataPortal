library(shiny)
library(leaflet)
library(rgdal)

censusTowns <- readOGR(dsn = "/Users/rupayanneogy/Downloads/CENSUS2010TOWNS_SHP",
                        layer = "CENSUS2010TOWNS_POLY") %>% spTransform(
                          CRS("+proj=longlat +datum=WGS84 +no_defs"))


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
      addPolygons(data = censusTowns, weight = 4, color = "blue",
                   opacity = 0.1, label = ~TOWN2)
  })
}

shinyApp(ui,server)