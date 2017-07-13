rm(list = ls())
library(shiny)
members <- data.frame(name=c("Subway Delays", "Commuter Rail Delays"), nr=c('subway.mbtatrains.com','mbtatrains.com'))

ui <- fluidPage(titlePanel("MBTA Delays"), 
                sidebarLayout(
                  sidebarPanel(
                    fluidRow(
                      column(6, selectInput("Member", label=h5("Choose a option"),choices=members$name)
                      ))),
                  mainPanel(fluidRow(
                    htmlOutput("frame")
                  )
                  )
                ))

server <- function(input, output) {
  observe({ 
    query <- members[which(members$name==input$Member),2]
    test <<- paste0("https://",query)
  })
  output$frame <- renderUI({
    input$Member
    my_test <- tags$iframe(src=test, height=1050, width=900)
    print(my_test)
    my_test
  })
}

shinyApp(ui, server)