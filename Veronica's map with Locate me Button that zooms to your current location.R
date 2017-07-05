library(dplyr)
library(shiny)
library(leaflet)
library(htmltools)
library(shinyjs)
jsCode <- '
shinyjs.geoloc = function() {
    navigator.geolocation.getCurrentPosition(onSuccess, onError);
    function onError (err) {
        Shiny.onInputChange("geolocation", false);
    }
    function onSuccess (position) {
        setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
        }, 5)
    }
};
'
wins <- read.csv("C:\\Users\\212617270\\Downloads\\wins12.csv")
greenLeafIcon <- makeIcon(iconUrl = "C:\\Users\\212617270\\Downloads\\Untitled-3.png", iconWidth = 40, iconHeight = 60, iconAnchorX = 0, iconAnchorY =)
ui <- bootstrapPage(
  useShinyjs(),
  extendShinyjs(text = jsCode),
  br(),
  actionButton("geoloc", "My Location", class="btn btn-primary", onClick="shinyjs.geoloc()"),
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
  ) 
server <- function(input, output){
  output$map <- renderLeaflet({
    map <- leaflet(wins) %>% addProviderTiles(providers$OpenStreetMap)
    map %>% setView(-72.690940, 41.651426, zoom = 12)
    map %>% addMarkers(~lng, ~lat,
                     icon = greenLeafIcon,
                     popup = paste("Where:", wins$Where,"<br>", "What:", wins$What,"<br>", "Who:", wins$Who,"<br>", "Why:", wins$Why,"<br>", "Order Date:", wins$OrderDate,"<br>", "Go Live Date:", wins$GoLiveDate, "<br>","<a href='",wins$link,"' target='_blank'>",wins$link,"</a>"),
                     clusterOptions = markerClusterOptions(color="#0017bb"),
                     label = wins$Name,
                     labelOptions = labelOptions(noHide = F))
  })
  observeEvent(input$geoloc, {
      js$geoloc()
  })
  observe({
       if(!is.null(input$lat)){
         m <- leafletProxy("map")
         dist <- 0.2
         lat <- input$lat
         lng <- input$long
         m %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
       }
     })
  }

  shinyApp(ui = ui, server = server)

