library(shiny)
library(leaflet)
library(rgdal)
library(spatialEco)
library(dplyr)
library(sp)

lightLink <- "https://data.boston.gov/dataset/52b0fdad-4037-460c-9c92-290f5774ab2b/resource/c2fcc1e3-c38f-44ad-a0cf-e5ea2a6585b5/download/streetlight-locations.csv"
neighborhoodLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.geojson"
crimeLink <- "https://data.boston.gov/dataset/6220d948-eae2-4e4b-8723-2dc8e67722a3/resource/12cb3883-56f5-47de-afa5-3b1cf61b257b/download/crime.csv"

lightFile <- "./data/light.rds"
lightData <- NULL
crimeFile <- "./data/crime.rds"
crimeData <- NULL


if(file.exists(lightFile)){
  lightData <- readRDS(lightFile)
}else{
  lightData <- read.csv(lightLink)
  saveRDS(lightData, lightFile)
}

if(file.exists(crimeFile)){
  crimeData <- readRDS(crimeFile)
}else{
  crimeData <- read.csv(crimeLink)
  saveRDS(crimeData, crimeFile)
}

neighborhoodJson <- readOGR(neighborhoodLink, "OGRGeoJSON")


# for lights
complete <- lightData[complete.cases(lightData),]
sp::coordinates(complete) <- ~Long+Lat
sp::proj4string(complete) <- sp::proj4string(neighborhoodJson)
pts.poly <- point.in.poly(complete, neighborhoodJson)
numLightsInNeighborhood <- tapply(pts.poly@data$OBJECTID, pts.poly@data$Name, FUN=length)

neighborhoodJson@data$totalLights <- unname(numLightsInNeighborhood[neighborhoodJson@data$Name])

neighborhoodJson@data$lightDensity <- neighborhoodJson@data$totalLights/neighborhoodJson@data$SqMiles

# for crimes
complete <- crimeData[complete.cases(crimeData),]
sp::coordinates(complete) <- ~Long+Lat
sp::proj4string(complete) <- sp::proj4string(neighborhoodJson)
pts.poly <- point.in.poly(complete, neighborhoodJson)
numCrimesInNeighborhood <- tapply(pts.poly@data$INCIDENT_NUMBER, pts.poly@data$Name, FUN=length)

neighborhoodJson@data$totalCrimes <- unname(numCrimesInNeighborhood[neighborhoodJson@data$Name])

neighborhoodJson@data$crimeDensity <- neighborhoodJson@data$totalCrimes/neighborhoodJson@data$SqMiles


fairmount <- c("Roxbury","Dorchester","Mattapan","Hyde Park")
imp <- c("Downtown", "North End", "West End", "Beacon Hill", "Leather District",
         "Chinatown", "Bay Village", "South End", "Fenway", "Back Bay", 
         "South Boston", "South Boston Waterfront", fairmount)

pal <- colorNumeric(c("#0c0b2d", "white"), domain = neighborhoodJson@data$lightDensity)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%;margin:0;}"),
  absolutePanel(
    leafletOutput("map", width = "100%", height = "100%"),
    width = "50%", height = "100%"),
  absolutePanel(
    leafletOutput("crimeMap", width = "100%", height = "100%"),
    width = "50%", height = "100%", left = "50%"),
  absolutePanel(
    headerPanel("Light/Crime Density in the Fairmount Corridor"),
    tags$head(tags$style("h1{color: white;}")),
    width = "100%", height = "10%")
)

server <- function(input, output, session){
  output$map <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,minZoom = 12, maxZoom = 12,
                                     dragging = FALSE)) %>%
      setView(lng = -71.0589, lat = 42.3, zoom = 12) %>%
      addProviderTiles(providers$CartoDB.DarkMatter) %>%
      # the legend for the shading of the zones
      addLegend("bottomright", pal = pal, values = neighborhoodJson@data$lightDensity,
                title = "Light Density In Neighborhoods",
                opacity = 1) %>%
      # adding the zones
      addPolygons(data = neighborhoodJson, weight = ~ifelse(Name %in% fairmount, 4, 0),
                  color = "#ffd6d6",
                  fill = ~(Name %in% imp), popup = ~paste("<b>", Name, "</b><br/>Street Lights: ",
                                                          totalLights, "<br/>Area: ",
                                                          SqMiles, " square miles<br/>Light Density: ",
                                                          lightDensity), group = "light",
                  fillColor = ~pal(lightDensity),
                  fillOpacity = 0.95,
                  label = ~Name
      )
  })
  
  output$crimeMap <- renderLeaflet({
    leaflet(options = leafletOptions(zoomControl = FALSE,minZoom = 12, maxZoom = 12,
                                     dragging = FALSE)) %>%
      setView(lng = -71.0589, lat = 42.3, zoom = 12) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      # the legend for the shading of the zones
      addLegend("bottomright", pal = colorNumeric(
        palette = "YlOrRd",
        domain = neighborhoodJson@data$crimeDensity
      ), values = neighborhoodJson@data$crimeDensity, title = "Crime Density In Neighborhoods",
      opacity = 1) %>%
      # adding the zones
      addPolygons(data = neighborhoodJson, weight = ~ifelse(Name %in% fairmount, 4, 0),
                  color = "blue",
                  fill = ~(Name %in% imp), popup = ~paste("<b>", Name, "</b><br/>Crimes: ",
                                              totalCrimes, "<br/>Area: ",
                                              SqMiles, " square miles<br/>Crime Density: ",
                                              crimeDensity), group = "crime",
                  fillColor = ~colorQuantile("YlOrRd", crimeDensity)(crimeDensity),
                  fillOpacity = 0.6,
                  label = ~Name
      )
  })
}

shinyApp(ui,server)