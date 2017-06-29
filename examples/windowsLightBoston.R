library(shiny)
library(leaflet)
library(rgdal)
library(spatialEco)
library(dplyr)
library(sp)

lightLink <- "./data/streetlight-locations.csv"
neighborhoodLink <- "./data/Boston_Neighborhoods.geojson"

lightFile <- "./data/light.rds"
lightData <- NULL


if(file.exists(lightFile)){
  lightData <- readRDS(lightFile)
}else{
  lightData <- read.csv(lightLink)
  saveRDS(lightData, lightFile)
}

neighborhoodJson <- readOGR(neighborhoodLink, "OGRGeoJSON")


# make sure there are no N/A entries in data
complete <- lightData[complete.cases(lightData),]

sp::coordinates(complete) <- ~Long+Lat
sp::proj4string(complete) <- sp::proj4string(neighborhoodJson)
pts.poly <- point.in.poly(complete, neighborhoodJson)
numLightsInNeighborhood <- tapply(pts.poly@data$OBJECTID, pts.poly@data$Name, FUN=length)

neighborhoodJson@data$totalLights <- unname(numLightsInNeighborhood[neighborhoodJson@data$Name])

neighborhoodJson@data$lightDensity <- neighborhoodJson@data$totalLights/neighborhoodJson@data$SqMiles

fairmount <- c("Roxbury","Dorchester","Mattapan","Hyde Park")
imp <- c("Downtown", "North End", "West End", "Beacon Hill", "Leather District",
         "Chinatown", "Bay Village", "South End", "Fenway", "Back Bay", 
         "South Boston", "South Boston Waterfront", fairmount)

pal <- colorNumeric(c("#0c0b2d", "white"), domain = neighborhoodJson@data$lightDensity)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session){
  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      # centering the view on a specific location (Boston)
      fitBounds(-71.5, 42, -70.5, 42.5) %>%
      # the legend for the shading of the zones
      addLegend("bottomright", pal = pal, values = neighborhoodJson@data$lightDensity,
                title = "Light Density In Neighborhoods",
                opacity = 1) %>%
      # adding the zones
      addPolygons(data = neighborhoodJson, weight = ~ifelse(Name %in% fairmount, 2, 0),
                  color = "#ffd6d6",
                  fill = ~(Name %in% imp), popup = ~paste("<b>", Name, "</b><br/>Street Lights: ",
                                                          totalLights, "<br/>Area: ",
                                                          SqMiles, " square miles<br/>Light Density: ",
                                                          lightDensity), group = "zones",
                  fillColor = ~pal(lightDensity),
                  fillOpacity = 0.95,
                  label = ~Name
      )
  })
}

shinyApp(ui,server)