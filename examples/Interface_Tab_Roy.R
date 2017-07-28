library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(htmltools)
library(jsonlite)
library(geojsonio)
library(leaflet.extras)
library(shinyjs)
library(V8)
library(RCurl)
library(rgdal)

tLines <- rgdal::readOGR(dsn = "./mbta_rapid_transit", layer = "MBTA_ARC")
tLines <- spTransform(tLines, CRS("+proj=longlat +datum=WGS84 +no_defs"))

tStops <- readOGR(dsn = "./mbta_rapid_transit",
                  layer = "MBTA_NODE") %>% spTransform(CRS("+proj=longlat +datum=WGS84 +no_defs"))
boundaries <- rgdal::readOGR("./data/City_of_Boston_Boundary.geojson", "OGRGeoJSON")

neighborhoods <- read.csv("neighborhoodData.csv")
members <- data.frame(name=c("Subway Delays", "Commuter Rail Delays"), nr=c('subway.mbtatrains.com','mbtatrains.com'))
emergencyparking <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/53ebc23fcc654111b642f70e61c63852_0.geojson",what = "sp")
parking_popuptext <- paste(sep="<br/>",
                           emergencyparking$Name,
                           emergencyparking$Address,
                           emergencyparking$Fee)

chargingstations <-geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/465e00f9632145a1ad645a27d27069b4_2.geojson",what="sp")
charging_popuptext <-paste(sep="<br/>",
                           chargingstations$Station_Name,
                           chargingstations$Street_Address)
bikelanes <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/d02c9d2003af455fbc37f550cc53d3a4_0.geojson", what = "sp")

hubwaystations <- read.csv("https://s3.amazonaws.com/hubway-data/Hubway_Stations_2011_2016.csv")
hubwaypoints <-cbind(as.numeric(hubwaystations$Longitude),as.numeric(hubwaystations$Latitude))

parking_popuptext <- paste(sep="<br/>",
                           emergencyparking$Name,
                           emergencyparking$Address,
                           emergencyparking$Fee)

hubway_popuptext <- paste(sep="<br/>",
                          hubwaystations$Station)
ui = dashboardPage(
  dashboardHeader(title = 'Fellows Map (WIP)',
                  dropdownMenuOutput('task_menu')),
  dashboardSidebar(sidebarMenu(
    id = 'menu_tabs',
    menuItem('Transportation', tabName = 'Transportation'),
    menuItem('Tester Tab', tabName = 'test'))),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Transportation',
              fluidPage(
                column(width = 12,
                  box(width =12,title = "Bike", solidHeader = TRUE,collapsible = TRUE, status = 'primary'
                       ,fluidRow(box(width = 8, title = "Bike Map", solidHeader = TRUE, collapsible = FALSE, 
                                     leafletOutput("bikemap", height = "522px")
                                     ), 
                 column(width = 4, box(width = 12,  solidHeader = FALSE, collapsible = FALSE,
                        fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/VeronicaMap/BikeCommuteIB2030.png",width = 275),href="https://www.boston.gov/transportation/go-boston-2030", 
                        target = "_blank")
                        ), h6("See what the city is doing to improve transportation by clicking the picture above.", align = "center")), 
                                        box(width = 12, title = "Further Research", solidHeader = TRUE, collapsible = FALSE,
                                        h6(a("Imagine Boston 2030 Transportation Plan",href="http://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Transportation.pdf",target="_blank")),
                                        h6(a("Hubway Bike Share System",href="https://www.thehubway.com",target="_blank")),
                                        h6(a("Boston Bike",href="https://www.boston.gov/departments/boston-bikes",target="_blank"))
                                        )
                 )
                        ), 
                 fluidRow(
                   box(width = 4,  solidHeader = FALSE, collapsible = FALSE, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/HubwayPrice.png", width = 275)
                   ))),
                   box(width = 4,  solidHeader = FALSE, collapsible = FALSE
                   ),
                   box(width = 4,  solidHeader = FALSE, collapsible = FALSE
                   )
                   ),
                 fluidRow(box(width = 12, title = "How Does Boston Compare to Other Cities?", solidHeader = TRUE, collapsible = FALSE, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/48ea29c88ca50cfeebcfc18c1bea66114c92db0b/Screenshot%202017-07-27%2013.39.44.png", width = 1000)
                 )),h6("How Los Angeles compares with their bike data.", align = "center")))
                 )
                                                     
              
      
                  , box(width = 12, title = "Drive", solidHeader = TRUE,collapsible = TRUE, status = 'primary', 
                        fluidRow(box(width = 8, title = "Drive Map", solidHeader = TRUE, collapsible = FALSE,
                                              leafletOutput("drivemap", height = "522px")
                  ), 
                  column(width = 4, box(width = 12,  solidHeader = FALSE, collapsible = FALSE,
                                        fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/DriveCommuteIB2030.png",width = 275),href="http://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Transportation.pdf", 
                                                         target = "_blank")
                                        ), h6("See what the city is doing to improve transportation by clicking the picture above.", align = "center")), 
                         box(width = 12, title = "How can you make the city of Boston safer?", solidHeader = TRUE, collapsible = FALSE,
                             fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/VisionZero.PNG",width = 275),href="http://www.visionzeroboston.org/", 
                                              target = "_blank")
                             ), h6("See what you can do to improve transportation by clicking the picture above.", align = "center"))
                  )
                  ), 
                  fluidRow(
                    box(width = 4,  solidHeader = FALSE, collapsible = FALSE
                    ),
                    box(width = 4,  solidHeader = FALSE, collapsible = FALSE
                    ),
                    box(width = 4,  solidHeader = FALSE, collapsible = FALSE
                    )
                  ),
                  fluidRow(box(width = 12, title = "How Does Boston Compare to Other Cities?", solidHeader = TRUE, collapsible = FALSE, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/48ea29c88ca50cfeebcfc18c1bea66114c92db0b/Screenshot%202017-07-27%2013.39.44.png", width = 1000)
                  )),h6("How Los Angeles compares with their bike data.", align = "center"))))
                  , 

box(width = 12, title = "Transit", solidHeader = TRUE,collapsible = TRUE, status = 'primary', 
                        fluidRow(box(width = 8, title = "MBTA Map", solidHeader = TRUE, collapsible = FALSE,
                                            leafletOutput("MBTAmap", height = "522px")
                  ), 
                  column(width = 4, box(width = 12,  solidHeader = FALSE, collapsible = FALSE,
                                        fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/VeronicaMap/BikeCommuteIB2030.png",width = 275),href="https://www.boston.gov/transportation/go-boston-2030", 
                                                         target = "_blank")
                                        ), h6("See what the city is doing to improve transportation by clicking the picture above.", align = "center")), 
                         box(width = 12, title = "Further Research", solidHeader = TRUE, collapsible = FALSE,
                             h6(a("Imagine Boston 2030 Transportation Plan",href="http://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Transportation.pdf",target="_blank")),
                             h6(a("Hubway Bike Share System",href="https://www.thehubway.com",target="_blank")),
                             h6(a("Boston Bike",href="https://www.boston.gov/departments/boston-bikes",target="_blank"))
                         )
                  )
                  ), 
                  fluidRow(
                    box(width = 8,  solidHeader = FALSE, collapsible = FALSE
                    ),
                    column(width = 4,
                    box(width = 12,  solidHeader = FALSE, collapsible = FALSE
                    ),
                    box(width = 12,  solidHeader = FALSE, collapsible = FALSE
                    ),
                    box(width = 12,  solidHeader = FALSE, collapsible = FALSE
                    )
                  )),
                  fluidRow(box(width = 12, title = "How Does Boston Compare to Other Cities?", solidHeader = TRUE, collapsible = FALSE, fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/48ea29c88ca50cfeebcfc18c1bea66114c92db0b/Screenshot%202017-07-27%2013.39.44.png", width = 1000)
                  )),h6("How Los Angeles compares with their bike data.", align = "center"))))
              ))
      )
      )
      
    )
      
    )

    
server = function(input, output){
  output$bikemap <- renderLeaflet({
    leaflet(height = 100)%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng =-71.057083, lat = 42.3601, zoom = 11) %>%
      addPolylines(data=bikelanes,group="Bike Network",weight=4)%>%
      addMarkers(data=hubwaypoints,popup=hubway_popuptext,group="Hubway Stations",clusterId = 'bikes',clusterOptions = markerClusterOptions())%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Boston",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addMiniMap(width = 75, height = 75)%>%
      addLayersControl(
        overlayGroups = c("Bike Network","Hubway Stations"),
        options=layersControlOptions(collapsed=FALSE)
      )
  })
  observeEvent(input$geoloc, {
  })
  observe({
    if(!is.null(input$lat)){
      m <- leafletProxy("bikemap")
      dist <- 0.005
      lat <- input$lat
      lng <- input$long
      m %>% fitBounds(lng - dist, lat - dist, lng + dist, lat + dist)
    }
  })
  observe({
    click <- input$map_marker_click
    zoom <- isolate(input$map_zoom)
    if(is.null(click))
      return()
    
    leafletProxy('bikemap') %>% 
      setView(click$lng, click$lat, zoom = 15)
  })
  
  output$drivemap <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      setView(lng =-71.057083, lat = 42.3601, zoom = 11) %>%
      addMarkers(data=emergencyparking,popup=parking_popuptext,group="Emergency Parking",clusterId = 'parking',clusterOptions = markerClusterOptions())%>%
      addMarkers(data=chargingstations,popup=parking_popuptext,group="Charging Stations",clusterId = 'charging stations',clusterOptions = markerClusterOptions())%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Boston",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addMiniMap(width = 75, height = 75)%>%
      addLayersControl(
        baseGroups=c("Emergency Parking","Charging Stations"),
        options=layersControlOptions(collapsed=FALSE)
      )
  })
  
  output$MBTAmap <- renderLeaflet({
    leaflet(neighborhoods) %>%
      addProviderTiles(providers$CartoDB.Positron)  %>%
      # centering the view on a specific location (Boston)
      setView(lng = -71.0589, lat = 42.4, zoom = 12) %>%
      addCircles(~lng, ~lat, popup= ~Name, weight = 1, radius=250, color="#000000", stroke = TRUE, fillOpacity = 1) %>%
      addPolylines(data = tLines, weight = 4, color = ~tolower(LINE), fill = FALSE,
                   opacity = 1) %>%
      addCircleMarkers(data = tStops, label = ~STATION, color = ~tolower(LINE),
                       fillOpacity = 0.5, weight = .5, radius = 5)
    
  })
  
}
shinyApp(ui= ui, server = server)  
 