library(shiny)
library(leaflet)
library(rjson)

datasets <- fromJSON(readLines("https://data.boston.gov/api/3/action/package_search?q=geojson&rows=60"))$result$results
titles <- lapply(datasets, function(x){
  return (x$title)
})
urls <- lapply(resources, function(x){
  return (x[[1]]$url)
})
data <- setNames(urls,titles)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                # sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                #             value = range(quakes$mag), step = 0.1
                # ),
                selectInput("data", "Data Set", titles)
  )
)

server <- function(input, output, session) {
  
  jsonData <- reactive({
    readOGR(as.character(data[input$data]), "OGRGeoJSON")
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%
      # the styling of the map itself
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      # centering the view on a specific location (Boston)
      fitBounds(-71.5, 42, -70.5, 42.5) %>%
      # # adding the markers
      # addCircleMarkers(data = policeStationJson, color = "purple",
      #                  radius = 5, stroke = FALSE, label = ~NAME,
      #                  fillOpacity = 0.7, group = "stations") %>%
      # # adding the zones
      addMarkers(data = jsonData())
  })
}

shinyApp(ui, server)