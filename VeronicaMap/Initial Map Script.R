library(dplyr)
library(shiny)
library(leaflet)
library(htmltools)
wins <- read.csv("Wins1.csv")
greenLeafIcon <- makeIcon(iconUrl = "Map_marker.png", iconWidth = 40, iconHeight = 60, iconAnchorX = 0, iconAnchorY = 0)
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")) 
server <- function(input, output) {
  output$map <- renderLeaflet({
    m <- leaflet(wins) %>% addProviderTiles(providers$CartoDB.Positron,
                                            options = providerTileOptions(noWrap = TRUE)) 
    m %>% setView(-72.690940, 41.651426, zoom = 5)
    m %>% addCircles(~lng, ~lat, popup=wins$type, weight = 7, radius=70, 
                     color="#0017bb", stroke = TRUE, fillOpacity = .8) 
    m %>% addMarkers(~lng, ~lat, icon = greenLeafIcon, popup = ~htmlEscape(Name))
  })
}
shinyApp(ui, server)





                          
                            