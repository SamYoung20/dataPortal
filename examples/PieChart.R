library(plotly)

Categorie = c('Drive Alone','Public Transit','Walk','Carpool','Other/Work from Home','Bike')
data1 = c(39,34,14,6,5,2)
X1960 = c(1,2,3,4,5,6)
data2 <- data.frame(Categorie, data1, X1960)
data <- data2[, c('Categorie', 'X1960')]

colors <- c('rgb(51, 255, 51)', 'rgb(0, 128, 255)', 'rgb(250, 0, 50)', 'rgb(127, 0, 255)','rgb(255, 51, 153)', 'rgb(30, 144, 255)')

p <- plot_ly(data2, labels = Categorie, values = data1, type = 'pie',
             textposition = 'inside',
             textinfo = 'label',
             insidetextfont = list(color = '#FFFFFF'),
             hoverinfo = 'percent',
             marker = list(colors = colors,
                           line = list(color = '#FFFFFF', width = 1)),
             #The 'pull' attribute can also be used to create space between the sectors
             showlegend = FALSE) %>%
  layout(title = 'How Bostonians Get to Work Today',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
print(p)
