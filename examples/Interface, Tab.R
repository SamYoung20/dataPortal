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
bikelanes <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/d02c9d2003af455fbc37f550cc53d3a4_0.geojson", what = "sp")

hubwaystations <- read.csv("https://s3.amazonaws.com/hubway-data/Hubway_Stations_2011_2016.csv")
hubwaypoints <-cbind(as.numeric(hubwaystations$Longitude),as.numeric(hubwaystations$Latitude))

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
                column(width = 12
                  ,box(width =12,title = "Bike", solidHeader = TRUE,collapsible = TRUE, status = 'primary',
                       fluidPage(box(width = 8,  height = "100%", title = "Bike Map", solidHeader = TRUE, collapsible = FALSE, 
                                     status = 'primary',
                                     leafletOutput("bikemap")
                                     ), 
                 column(width = 4, box(width = 12,  solidHeader = FALSE, collapsible = FALSE,
                        fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/VeronicaMap/BikeCommuteIB2030.png",width = 275),href="https://www.boston.gov/transportation/go-boston-2030", 
                        target = "_blank")
                        ), h6("See what the city is doing to improve transportation by clicking the picture above.", align = "center")), 
                                        box(width = 12, title = "Further Research", solidHeader = TRUE, collapsible = FALSE,
                                        h6(a("Go Boston 2030 Plan",href="https://www.boston.gov/transportation/go-boston-2030#report-chapters",target="_blank")),
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
                 fluidRow(box(width = 12,  solidHeader = FALSE, collapsible = FALSE))
                 )
                                                     
              
      
                  , box(width = 12, title = "Drive", solidHeader = TRUE,collapsible = TRUE, status = 'primary')
                  , box(width = 12, title = "Transit", solidHeader = TRUE,collapsible = TRUE, status = 'primary')
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
  
}
shinyApp(ui= ui, server = server)  
 