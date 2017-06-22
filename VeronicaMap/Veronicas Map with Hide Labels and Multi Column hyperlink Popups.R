library(dplyr)
library(shiny)
library(leaflet)
library(htmltools)
wins <- read.csv("C:\\Users\\212617270\\Downloads\\wins12.csv")
greenLeafIcon <- makeIcon(iconUrl = "C:\\Users\\212617270\\Downloads\\Untitled-3.png", iconWidth = 40, iconHeight = 60, iconAnchorX = 0, iconAnchorY =)
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")) 
server <- function(input, output) {
  output$map <- renderLeaflet({
    m <- leaflet(wins) %>% addProviderTiles(providers$OpenStreetMap)
    m %>% setView(-72.690940, 41.651426, zoom = 5)
    m %>% addCircles(~lng, ~lat, popup=wins$type, weight = 7, radius=70, 
                     color="#0017bb", stroke = TRUE, fillOpacity = .8) 
    m %>% addMarkers(
      ~lng, ~lat,
      icon = greenLeafIcon,
      popup = paste("Where:", wins$Where,"<br>", "What:", wins$What,"<br>", "Who:", wins$Who, "<br>","<a href='",wins$link,"' target='_blank'>",wins$link,"</a>"),
      label = wins$Name,
      labelOptions = labelOptions(noHide = F)) 
     })
}
shinyApp(ui, server)

