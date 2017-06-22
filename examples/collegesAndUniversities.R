library(shiny)
library(leaflet)
library(rgdal)


neighborhoodLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.geojson"

# IF YOU WANT TO CHANGE DATA SET, CHANGE LINK BELOW TO .csv LINK
# collegeData <- read.csv("http://bostonopendata-boston.opendata.arcgis.com/datasets/cbf14bb032ef4bd38e20429f71acb61a_2.csv")
# publicSchoolData <- read.csv("http://bostonopendata-boston.opendata.arcgis.com/datasets/1d9509a8b2fd485d9ad471ba2fdb1f90_0.csv")
bpsData <- read.csv("https://data.boston.gov/dataset/b2c5a9d3-609d-49ec-906c-b0e850a8d62a/resource/33c5f44a-3c67-4390-a1d5-1bf018e4728c/download/buildbps.csv")

zoneJson <- readOGR(neighborhoodLink, "OGRGeoJSON")
fairmount <- c("Roxbury","Dorchester","Mattapan","Hyde Park")
ratings <- c("Excellent", "Fair", "Good", "Not Assessed", "Poor")
colors <- c("#3b8e1f", "#6d7c30", "#eae610", "#e58022","#cc2910")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session){
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # centering the view on a specific location (Boston)
      fitBounds(-71.5, 42, -70.5, 42.5) %>%
      # adding the zones
      addPolygons(data = zoneJson, weight = 2,
                  color = ~ifelse(Name %in% fairmount, "#682f07", "blue"),
                  fill = TRUE, label = ~Name,
                  group = "zones",
                  highlightOptions = highlightOptions(color = "black", weight = 3)) %>%
      # addCircleMarkers(data = collegeData, lng = ~Longitude, lat = ~Latitude,
      #                  color = "purple",
      #                  radius = 5, stroke = FALSE,
      #                  fillOpacity = 0.7, group = "colleges",
      #                  popup = ~paste(sep = "<br/>",
      #                                paste("<b>",Name,"</b>"),
      #                                Address,
      #                                City
      #                                )
      #                  ) %>%
      addCircleMarkers(data = bpsData, lng = ~SMMA_longitude, lat = ~SMMA_latitude,
                     color = ~colors[SMMA_EA_Overall_EFE_learning_environments],
                     radius = 4, stroke = FALSE,
                     fillOpacity = 0.9, group = "schools",
                     popup = ~paste(sep = "<br/>",
                                    paste("<b>",BPS_School_Name,"</b>"),
                                    BPS_Address,
                                    BRA_Neighborhood
                     )
      ) %>%
      addLegend("bottomleft", colors=colors, labels=ratings, title="School Quality",
                layerId="colorLegend", opacity = 1)
  })
}

shinyApp(ui, server)