library(dplyr)
library(shiny)
library(leaflet)
library(htmltools)
wins <- read.csv("/Users/veronica/downloads/wins.csv")
greenLeafIcon <- makeIcon(iconUrl = "/Users/veronica/downloads/Untitled-1.svg", iconWidth = 50, iconHeight = 50, iconAnchorX = 0, iconAnchorY = 0)
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")) 
server <- function(input, output) {
  output$map <- renderLeaflet({
    m <- leaflet(wins) %>% addTiles('http://{s}.basemaps.cartocdn.com/light_all/{z}/{x}/{y}.png', 
                                    attribution='Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') 
    m %>% setView(-72.690940, 41.651426, zoom = 5)
    m %>% addCircles(~lng, ~lat, popup=wins$type, weight = 7, radius=70, 
                     color="#0017bb", stroke = TRUE, fillOpacity = .8) 
    m %>% addMarkers(~lng, ~lat, icon = greenLeafIcon, popup = ~htmlEscape(Name))
  })
}
shinyApp(ui, server)





                          
                            