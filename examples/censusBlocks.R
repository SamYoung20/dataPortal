library(shiny)
library(leaflet)
library(rgdal)
library(rgeos)
library(raster)

# file location for population data
popFile <- "./data/pop.rds"
pop <- NULL

# logic for checking if local cache of population data already exists (speeds up
# application on successive runs)
if(file.exists(popFile)){
  pop <- readRDS(popFile)
}else{
  # reads census shapefile (path needs to be changed)
  censusBlocks <- readOGR(dsn = "/Users/rupayanneogy/Downloads/CENSUS2010_BLK_BG_TRCT_SHP",
                          layer = "CENSUS2010TRACTS_POLY", GDAL1_integer64=TRUE) %>% spTransform(
                            CRS("+proj=longlat +datum=WGS84 +no_defs"))
  # raster intersection to only look at blocks in Boston instead of all of MA
  smol <- intersect(neighborhoods,censusBlocks)
  # sums the population based on the name of the neighborhood
  pop <- aggregate(POP100_RE~Name, smol, sum)
  # saves population data to the local file (cache)
  saveRDS(pop, popFile)
}

# read neighborhood zones from analyze boston
neighborhoods <- readOGR("http://bostonopendata-boston.opendata.arcgis.com/datasets/3525b0ee6e6b427f9aab5d0a1d0a1a28_0.geojson", "OGRGeoJSON")
# merges the population data into the neighborhoods spdf
neighborhoods <- merge(neighborhoods, pop, by="Name")
# density of population in each neighborhood
neighborhoods$popDensity <- (neighborhoods$POP100_RE/neighborhoods$SqMiles)

# color palette function for the population density (domain can be any range)
pal <- colorNumeric("YlOrRd", domain = neighborhoods$popDensity)

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%")
)

server <- function(input, output, session){
  output$map <- renderLeaflet({
    leaflet() %>%
      # provider tiles might need to be changed for PC
      addProviderTiles(providers$CartoDB.Positron) %>%
      # centering the view on a specific location (Boston)
      setView(lng = -71.0589, lat = 42.4, zoom = 12) %>%
      # adding spdf to leaflet
      addPolygons(data = neighborhoods, weight = 1, stroke = FALSE,
                  fillOpacity = 0.8,fillColor = ~pal(popDensity),
                  label = ~paste(Name, popDensity))
  })
}

shinyApp(ui,server)