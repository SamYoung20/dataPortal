ui <- fluidPage(
  checkboxInput("checkbox", label = "Choice A", value = TRUE),
  hr(),
  fluidRow(column(3, verbatimTextOutput("value")))
)
server<- function(input, output) {
  
  # You can access the value of the widget with input$checkbox, e.g.
  output$value <- renderPrint({ input$checkbox })
  
}
shinyApp(ui, server)