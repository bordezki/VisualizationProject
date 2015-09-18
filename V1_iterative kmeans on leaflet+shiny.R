library(shiny)
library(RColorBrewer)
library(clusterSim)
library(leaflet)
library (osmar)
library(sp)
library(maptools)
library(rgdal)
library(rgeos)
library(htmltools)
library(dplyr)
library(magrittr)

data <- read.csv("")  
selectedData <- data.frame(data$Lat,data$Lon)
pal <- colorFactor(palette= brewer.pal(10,"Set3"), domain = c(1:10))

ui <- fluidPage(
  headerPanel('Locations clustering by k-means'),
  sidebarPanel(
  sliderInput('clusters', 'Number of clusters',2 , min = 2, max = 10),
  sliderInput('dist', 'Search Distance',100 , min = 10, max = 200)
  ),
  mainPanel(
  leafletOutput('mymap'))
) #ui

server <- function(input, output, session) {
   
  output$mymap <- renderLeaflet({ 
   
    km <- kmeans(selectedData, input$clusters) 
    clustered_data <- data.frame(selectedData, km$cluster)
    
    leaflet(selectedData) %>% 
        fitBounds(min(selectedData$data.Lon),min(selectedData$data.Lat),
                  max(selectedData$data.Lon),max(selectedData$data.Lat))  %>%
        addTiles(group = "OSM (default)") %>%
        addProviderTiles("Stamen.Toner", group = "Toner") %>%
        addProviderTiles("Stamen.TonerLite", group = "Toner Lite") %>%
        addProviderTiles("OpenStreetMap.BlackAndWhite", group = "BlackandWhite") %>%
        addProviderTiles("CartoDB.DarkMatter", group="Dark") %>%  
        addCircleMarkers(~clustered_data$data.Lon, ~clustered_data$data.Lat, 
                         radius = 7, color =  ~pal(km$cluster), opacity=1)  %>%
        addLayersControl(baseGroups = c("OSM (default)", "Toner", "Toner Lite", "BlackandWhite", "Dark"),
                         options = layersControlOptions(collapsed = FALSE))  %>%
        addLegend("bottomleft",pal=pal,values = clustered_data$km.cluster,
                   title = "data by clusters",opacity = 1)
    })
  
 
} #server
    
shinyApp(ui, server)











