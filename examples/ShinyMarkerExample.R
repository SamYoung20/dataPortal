library(shiny)
library(leaflet)
library(jsonlite)

json_file <- "https://data.boston.gov/api/action/datastore_search?resource_id=0b2be5cb-89c6-4328-93be-c54ba723f8db"
json_data <- fromJSON(json_file)
records <- json_data$result$records
points <- cbind(as.numeric(records$X), as.numeric(records$Y))

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%")
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points)
  })
}

shinyApp(ui, server)