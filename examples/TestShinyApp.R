library(shiny)
library(leaflet)
library(jsonlite)

r_colors <- rgb(t(col2rgb(colors()) / 255))
names(r_colors) <- colors()
json_file <- "https://data.boston.gov/api/action/datastore_search?resource_id=0b2be5cb-89c6-4328-93be-c54ba723f8db"
json_data <- fromJSON(json_file)
records <- json_data$result$records
points <- cbind(as.numeric(records$X), as.numeric(records$Y))

ui <- fluidPage(
  leafletOutput("mymap"),
  p()
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