library(shiny)
library(leaflet)
library(twitteR)

ui = fluidPage(
    fluidRow(
      column(4, textInput("searchkw", label = "search", value = "#dinner")),
      column(4, textInput("lat", label = "latitude", value = 42.31)),
      column(4, textInput("long", label = "longitude", value = -71.05)),
      column(8, leafletOutput("myMap")),
      column(12, tableOutput('table'))
    )
  )
  
server = function(input, output) {
    
    #OAuth authentication
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
    
    output$myMap <- renderLeaflet({
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addMarkers(data=dataInput(), lng=~longitude, lat=~latitude, popup=~text)
    })
    
    #Create reactive table
    # output$table <- renderTable(
    #   dataInput()[, c("text", "screenName", "longitude", "latitude", "created")]
    # )
  }


shinyApp(ui, server)


