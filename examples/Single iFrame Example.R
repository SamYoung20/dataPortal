library(shiny)

rm(list = ls())


ui <- dashboardBody(box(width = 12, solidHeader = TRUE, collapsible = FALSE,
                        htmlOutput("frame"))
      )

server <- function(input, output) {
  output$frame <- renderUI({
    tags$iframe(src="http://app01.cityofboston.gov/VZSafety/", height=1050, width=900)
  })
}

shinyApp(ui, server)