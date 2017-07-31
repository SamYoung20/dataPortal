library(shiny)
library(shinyjs)



ui <- fluidPage(
    textOutput("currentTime"), #MUST keep in order for picture to pop up
    conditionalPanel(
                     condition="output['currentTime'] == '1'",
                     mainPanel(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/VeronicaMap/BikeCommuteIB2030.png", width = 600))
                     ),
                  
    conditionalPanel(
                     condition="output['currentTime'] == '2'",
                     mainPanel(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/master/HubwayPrice.png", width = 600))
)
)

server <- function(input,output,session){
  
  
  observe(
    
  output$currentTime <- renderText({
    #Re-executes every 1 second
    invalidateLater(1000, session)
    if(as.numeric(format(Sys.time(), "%S"))<16){
      return('1')
    }else if(as.numeric(format(Sys.time(), "%S"))>15 & as.numeric(format(Sys.time(), "%S"))<30){
      return('2')
    }
  })
  )
  

}

shinyApp(ui=ui,server=server)