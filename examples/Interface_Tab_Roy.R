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


#BERDO Declarations START
BERDO <- geojsonio::geojson_read("http://bostonopendata-boston.opendata.arcgis.com/datasets/82595a1b793a49c2bce7d61b751bdca5_2.geojson", what = "sp")

BERDO$EnergyStar_Score <- ifelse(is.na(BERDO$EnergyStar_Score),0, BERDO$EnergyStar_Score)
binScore <- c(seq(0,100,20)) # bind bind it with INF
palScore <- colorBin("YlGn", domain = BERDO$EnergyStar_Score, bins = binScore)

BERDO$Site_Energy_Use <- ifelse(is.na(BERDO$Site_Energy_Use),0, BERDO$Site_Energy_Use)
Energyuse_IQR <- IQR(BERDO$Site_Energy_Use)
Energyuse_Q3 <- summary(BERDO$Site_Energy_Use)[5] #returns 3rd quartile of data set
Energyuse_UpperBound <- Energyuse_Q3 + Energyuse_IQR*1.5
binUsage <- c(seq(0,Energyuse_UpperBound,Energyuse_UpperBound/5), Inf)
palUsage <- colorBin("OrRd", domain = BERDO$Site_Energy_Use, bins = binUsage)

BERDO$GHG_Emissions <- ifelse(is.na(BERDO$GHG_Emissions),0, BERDO$GHG_Emissions)
GHG_Emissions_IQR <- IQR(BERDO$GHG_Emissions)
GHG_Emissions_Q3 <- summary(BERDO$GHG_Emissions)[5] #returns 3rd quartile of data set
GHG_Emissions_UpperBound <- GHG_Emissions_Q3 + GHG_Emissions_IQR*1.5
binEmissions <- c(seq(0,GHG_Emissions_UpperBound,GHG_Emissions_UpperBound/5), Inf)
palEmissions <- colorBin("OrRd", domain = BERDO$GHG_Emissions,bins = binEmissions)
#BERDO DECLARATIONS END

ui = dashboardPage(
  dashboardHeader(title = 'Fellows Map (WIP)',dropdownMenuOutput('task_menu')),
  #Creates the Tabs
  dashboardSidebar(
                   sidebarMenu(
                               id = 'menu_tabs',
                               menuItem('Energy and Environment', tabName = 'Energy'),
                               menuItem('Tester Tab', tabName = 'test')
                               )
                   ),
  #Creates the Energy and Environment tab
  dashboardBody(
    tabItems(
      tabItem(tabName = 'Energy',
              fluidPage(
                column(width = 12,
                  box(
                       width =12,title = "Energy and Environment", solidHeader = TRUE,collapsible = TRUE, status = 'primary',
                       fluidRow(
                                box(
                                    width = 8, title="Energy Usage by Building", solidHeader = TRUE, collapsible = FALSE, 
                                    leafletOutput("BERDOmap", height = "522px") 
                                    ), 
                                column(
                                       width = 4, 
                                       box(
                                           h3("This map displays the 2015 BERDO Report which includes 1,502 buildings and represents 31% of Boston's total building energy use."),
                                           width = 12,solidHeader = FALSE, collapsible = FALSE, status='primary',background="blue"
                                           )
                                       )
                                ),
                        fluidRow(
                                 box(width = 4,solidHeader = TRUE, collapsible = FALSE, status='primary',fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/f7af9015f5baebdf5cddfd04a79d09379fabe4c7/examples/Energy%20and%20Environment%20pics/emissions.JPG", width = 275)))),
                                 box(width = 4,  solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/f7af9015f5baebdf5cddfd04a79d09379fabe4c7/examples/Energy%20and%20Environment%20pics/types%20of%20energy.JPG", width = 188,style="display: block; margin-left: auto; margin-right: auto;"))))
                                 ),
                       fluidRow(
                                box(width = 12, title="Boston's Sea Level",solidHeader = FALSE, collapsible = FALSE, status="primary",
                                    h4("See how Boston's waterfront is expected to change in the years to come."),
                                    htmlOutput("frame"),
                                    h3("Some More Facts on the Sea Level Rise"),
                                    fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/f7af9015f5baebdf5cddfd04a79d09379fabe4c7/examples/Energy%20and%20Environment%20pics/sea%20level%20rise.JPG", width = 500))) #Style assignment centers the image
                                    )
                                
                                 ),
                       fluidRow(
                         box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.jpg", width = 150),href="https://imagine.boston.gov/wp-content/uploads/2017/07/Ib2030%20BOOK_Spreads--Energy%20and%20Environment.pdf",target="_blank"))),
                         box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(2).jpg", width = 150),href="http://climatechangedata.boston.gov/",target="_blank"))),
                         box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed%20(1).jpg", width = 150),href="http://www.greenribboncommission.org/",target="_blank"))),
                         box(width = 3,solidHeader = TRUE, collapsible = FALSE, status='primary', fluidPage(tags$a(img(src="https://raw.githubusercontent.com/SamYoung20/dataPortal/6e0cc8ab3bedf1d5a42dbf0cbc4ce4558f858c2d/examples/Energy%20and%20Environment%20pics/unnamed.png", width = 150),href="http://www.greenovateboston.org/",target="_blank")))
                                )
                       )
              ))))))

server = function(input, output){
  #BERDO SERVER START
  output$BERDOmap <- renderLeaflet({
    leaflet()%>%
      addProviderTiles(providers$Esri.WorldGrayCanvas) %>%
      setView(lng =-71.057083, lat = 42.3601, zoom = 15) %>%
      addPolygons(
        data=BERDO,
        group="Energy Rating",
        weight=1,
        opacity=10,
        color="black",
        fillColor=~palScore(EnergyStar_Score),
        fillOpacity=5,
        popup=paste(BERDO$Property_Name,"<br/>","Type:",BERDO$Property_Uses,"<br/>","Energy Score:",ifelse(BERDO$EnergyStar_Score==0,"Not Enough Info",BERDO$EnergyStar_Score))
      )%>%
      addPolygons(
        data=BERDO,
        group="Energy Usage",
        weight=1,
        opacity=10,
        color="black",
        fillColor=~palUsage(Site_Energy_Use),
        fillOpacity=5,
        popup=paste(BERDO$Property_Name,"<br/>","Type:",BERDO$Property_Uses,"<br/>","Energy Usage:",ifelse(BERDO$Site_Energy_Use==0,"Not Enough Info",BERDO$Site_Energy_Use))
      )%>%
      addPolygons(
        data=BERDO,
        group="GHG Emissions",
        weight=1,
        opacity=10,
        color="black",
        fillColor=~palEmissions(GHG_Emissions),
        fillOpacity=5,
        popup=paste(BERDO$Property_Name,"<br/>","Type:",BERDO$Property_Uses,"<br/>","GHG Emissions:", ifelse(BERDO$GHG_Emissions==0,"Not Enough Info",BERDO$GHG_Emissions))
      )%>%
      addLayersControl(
        baseGroups = c("Energy Rating","Energy Usage","GHG Emissions"),
        options=layersControlOptions(collapsed=FALSE)
      )%>%
      addEasyButton(easyButton(
        icon="fa-globe", title="Zoom to Boston",
        onClick=JS("function(btn, map){ map.setZoom(8); }"))) %>%
      addEasyButton(easyButton(
        icon="fa-crosshairs", title="Locate Me",
        onClick=JS("function(btn, map){ map.locate({setView: true}); }")))%>%
      addMiniMap(width = 75, height = 75)
  })
    #BERDO SERVER END
  
  #IFRAME of Sea Level rise
  output$frame <- renderUI({
    tags$iframe(src="http://seachange.sasaki.com/", height=600, width=950)
  })

}
shinyApp(ui= ui, server = server)  
 