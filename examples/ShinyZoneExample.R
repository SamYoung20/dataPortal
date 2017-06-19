library(shiny)
library(leaflet)
library(RColorBrewer)

# Police zones
policeZoneLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/9a3a8c427add450eaf45a470245680fc_5.geojson"
policeZoneData <- readLines(policeZoneLink) %>% paste(collapse = "\n")
# Police Stations
policeStationLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/e5a0066d38ac4e2abbc7918197a4f6af_6.geojson"
policeStationData <- readLines(policeStationLink) %>% paste(collapse = "\n")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("mymap", width = "100%", height = "100%"),
  absolutePanel(top = 10, right = 10,
                sliderInput("range", "Magnitudes", min(quakes$mag), max(quakes$mag),
                            value = range(quakes$mag), step = 0.1
                ),
                selectInput("colors", "Color Scheme",
                            rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
                ),
                checkboxInput("legend", "Show legend", TRUE)
  )
)

server <- function(input, output, session) {
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addGeoJSON(policeStationData) %>%
      addGeoJSON(policeZoneData, weight = 3, color = "red", fill = FALSE) %>%
      fitBounds(-71.5, 42, -70.5, 42.5)
  })
}

shinyApp(ui, server)