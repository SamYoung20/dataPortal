library(shiny)
library(leaflet)
library(rgdal)
library(spatialEco)
library(dplyr)
library(sp)

publicWorkLink <- "https://data.boston.gov/dataset/f9f57091-92f3-463a-8f1c-93e46361296d/resource/36fcf981-e414-4891-93ea-f5905cec46fc/download/streetconstruction.csv"
crimeLink <- "https://data.boston.gov/dataset/6220d948-eae2-4e4b-8723-2dc8e67722a3/resource/12cb3883-56f5-47de-afa5-3b1cf61b257b/download/crime.csv"
neighborhoodLink <- "http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.geojson"

publicWorkFile <- "./data/publicWork.rds"
crimeFile <- "./data/crime.rds"


publicWorkData <- NULL
crimeData <- NULL

if(file.exists(publicWorkFile)){
  publicWorkData <- readRDS(publicWorkFile)
}else{
  publicWorkData <- read.csv(publicWorkLink)
  saveRDS(publicWorkData, publicWorkFile)
}

if(file.exists(crimeFile)){
  crimeData <- readRDS(crimeFile)
}else{
  crimeData <- read.csv(crimeLink)
  saveRDS(crimeData, crimeFile)
}

# publicWorkData <- read.csv(publicWorkLink)
# crimeData <- read.csv(crimeLink)
neighborhoodJson <- readOGR(neighborhoodLink, "OGRGeoJSON")

# neighborhoodWorkCount = data.frame(table(publicWorkData$Neighborhood))
# neighborhoodWorkCount %>% mutate_if(is.factor, as.character) -> neighborhoodWorkCount
# s <- strsplit(neighborhoodWorkCount$Var1, split = "/")
# neighborhoodWorkCount <- data.frame(neighborhood = unlist(s), freq = rep(neighborhoodWorkCount$Freq, sapply(s, length)))


complete <- crimeData[complete.cases(crimeData),]
sp::coordinates(complete) <- ~Long+Lat
sp::proj4string(complete) <- sp::proj4string(neighborhoodJson)
pts.poly <- point.in.poly(complete, neighborhoodJson)
numCrimesInNeighborhood <- tapply(pts.poly@data$INCIDENT_NUMBER, pts.poly@data$Name, FUN=length)

neighborhoodJson@data$totalCrimes <- unname(numCrimesInNeighborhood[neighborhoodJson@data$Name])

neighborhoodJson@data$crimeDensity <- neighborhoodJson@data$totalCrimes/neighborhoodJson@data$SqMiles

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
      # the legend for the shading of the zones
      addLegend("bottomright", pal = colorNumeric(
        palette = "YlOrRd",
        domain = neighborhoodJson@data$crimeDensity
      ), values = neighborhoodJson@data$crimeDensity, title = "Crime Density In Neighborhoods",
      opacity = 1) %>%
      # adding the zones
      addPolygons(data = neighborhoodJson, weight = 2, color = "blue",
                  fill = TRUE, popup = ~paste("<b>", Name, "</b><br/>Crimes: ",
                                              totalCrimes, "<br/>Area: ",
                                              SqMiles, " square miles<br/>Crime Density: ",
                                              crimeDensity), group = "zones", 
                  highlightOptions = highlightOptions(color = "black", weight = 3,
                                                      bringToFront = TRUE),
                  fillColor = ~colorQuantile("YlOrRd", crimeDensity)(crimeDensity),
                  fillOpacity = 0.6
                  )
  })
}

shinyApp(ui,server)