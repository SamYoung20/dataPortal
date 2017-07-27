library(shiny)
library(shinydashboard)
library(leaflet)
library(dplyr)
library(leaflet)
library(htmltools)
library(shiny)
library(shiny)
library(jsonlite)
library(geojsonio)
library(leaflet)
library(shinyjs)
rm(list = ls())
library(shiny)
library(shiny)
library(leaflet)
library(htmltools)
library(shinyjs)
library(RCurl)
library(V8)
library(rgdal)
library(twitteR)
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
ui <- dashboardPage(
  skin = "green",
  dashboardHeader(title = 'Expanding Neighborhoods',
                  titleWidth = 350,
                  # task list for status of data processing
                  dropdownMenuOutput('task_menu')),
  dashboardSidebar(sidebarMenu(
    id = 'menu_tabs'
    , menuItem('Transit', tabName = 'Transit')
    , menuItem('Bike', tabName = 'Bike')
    , menuItem('Social Media', tabName = 'SM')
    , menuItem('Drive', tabName = 'Dashboard')),
    tags$head(tags$style(HTML('.shiny-server-account { display: none; }'))),
    # The dynamically-generated user panel
    uiOutput("userpanel")),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Bike', fluidRow(box(width = 6,title = 'Bike',collapsible = TRUE, collapse = TRUE,status = "primary", solidHeader = TRUE,(tags$style(type = "text/css", "#map {height: calc(200vh - 80px) !important;}")),
                                             leafletOutput("mymap")),box(width = 6, title ='Learn More', status = "primary", solidHeader = TRUE, collapsible = TRUE,fluidPage(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/VeronicaMap/BikeCommuteIB2030.png", width=400),
                                                                                                                                                                              helpText(a("Imagine Boston 2030 Transportation Plan", href="http://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Transportation.pdf", target="_blank")))))),
      
      tabItem(tabName = 'Dashboard', fluidPage(box(width = 8, title = "Drive", status = "primary",collapsible = TRUE, solidHeader = TRUE, tags$style(type = "text/css", "#map {height: calc(100vh - 80px) !important;}"),
                                                   leafletOutput("map")), box( width = 4,title ='Learn More',collapsible = TRUE, solidHeader = TRUE, status = "primary",fluidPage(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/VeronicaMap/DriveCommuteIB2030.png", width=300),helpText(a("Imagine Boston 2030 Transportation Plan", href="http://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Transportation.pdf")))), box(width = 4, title = "How Can You Help Make Driving Safer?", status = "primary",collapsible = TRUE, solidHeader = TRUE, fluidPage((img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/VisionZero.PNG",width = 400)),helpText(a("Vision Zero: Improve Transportation Safety", href="http://www.visionzeroboston.org/"),
                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                               target= "_blank"))))),
      tabItem(tabName = 'SM',fluidPage(box(width = 8,title = "What's Trending?",status = "primary", collapsible = TRUE,
                                           fluidRow(
                                             column(4, textInput("searchkw", label = "search", value = "#Boston")),
                                             column(4, textInput("lat", label = "latitude", value = 42.31)),
                                             column(4, textInput("long", label = "longitude", value = -71.05)),
                                             column(12, leafletOutput("twitterMap")),
                                             column(12, tableOutput('table')))),box(width =4,title = "City of Boston",status = "primary", collapsible = TRUE, tags$head(tags$script('!function(d,s,id){var js,fjs=d.getElementsByTagName(s)    [0],p=/^http:/.test(d.location)?\'http\':\'https\';if(!d.getElementById(id)){js=d.createElement(s);js.id=id;js.src=p+"://platform.twitter.com/widgets.js";fjs.parentNode.insertBefore(js,fjs);}}(document,"script","twitter-wjs");')),
                                                                                    column(width=12, fluidPage(width=NULL, height=NULL,
                                                                                                               a("Tweets by @CityOfBoston", class="twitter-timeline"
                                                                                                                 , href = "https://twitter.com/CityOfBoston"
                                                                                                               )
                                                                                    ))))),
      tabItem(tabName = 'Transit', fluidPage(
        box(width = 8, title ='Delays', status = "primary",collapsible = TRUE, solidHeader = TRUE, 
            titlePanel("MBTA Delays"), sidebarLayout(
              sidebarPanel(
                fluidRow(
                  column(8, selectInput("Member", label=h5("Select An Option"),
                                        choices=members$name)))),
              mainPanel(fluidRow(
                htmlOutput("frame"))))),
        box( width = 4,title ='Learn More',collapsible = TRUE, solidHeader = TRUE, status = "primary", fluidPage(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/VeronicaMap/TransportationService.PNG", width=300),
                                                                                                                 helpText(a("Imagine Boston 2030 Transportation Plan", href="http://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Transportation.pdf")))), box(width = 4,title ='Explore',collapsible = TRUE, solidHeader = TRUE, status = "primary", fluidPage(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/Capture.PNG", width=250),
                                                                                                                                                                                                                                                                                                                                                                                       helpText(a("MBTA Data Explorer", href="http://www.mbtabackontrack.com/performance/index.html#/home"),target= "_blank"))))))))
server <- function(input, output) {
  consumer_key <- "9FbqGr2uc4MZDYD6COXqsefmq"
  consumer_secret <- "F2Fb8yqSJ9wTJfLfBbxefykhEc62OOduAlOEacTWp2lMo4U4mv"
  access_token <- "1618803432-Ug9jbf1rW7nZMQGbc70KUaPTSIDMMlk772QHDWl"
  access_secret <- "t414aeqScMyyq2HaPRYjcWtzIgGuQmnvCgNgtm66Cg2Xd"
  options(httr_oauth_cache = TRUE)
  setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)
  
  # Issue search query to twitter
  dataInput <- reactive({
    tweets <- twListToDF(searchTwitter(input$searchkw, n = 5, geocode = paste0(input$lat, ",",input$long, ",10km")))
    tweets$created <- as.character(tweets$created)
    tweets$longitude <- as.numeric(tweets$longitude)
    tweets$latitude <- as.numeric(tweets$latitude)
    tweets <- tweets[!is.na(tweets[,"longitude"]), ]
  })
  
  #Create reactive leaflet map
  # mapTweets <- reactive({
  #   data <- dataInput()
  #   print(as.numeric(data$longitude))
  #   map <- leaflet()
  #   print(as.numeric(data$latitude))
  # })
  
  output$twitterMap <- renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addMarkers(data=dataInput(), lng=~longitude, lat=~latitude, popup=~text)
  })
  observe({
    query <- members[which(members$name==input$Member),2]
    test <<- paste0("https://",query)
  })
  output$frame <- renderUI({
    input$Member
    my_test <- tags$iframe(src=test, height=500, width=400)
    print(my_test)
    my_test
  })
  
  output$mymap <- renderLeaflet({
    leaflet()%>%
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
  output$map <- renderLeaflet({
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
  
}

shinyApp(ui, server)