library(shiny)
library(leaflet)
library(rgdal)

shape <- readOGR("CENSUS2010BLOCKS_POLY")
