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
library(plyr)
library(dplyr)
library(magrittr)

data <- read.csv("")  
selectedData <- data.frame(data$Lon, data$Lat)
dataMatrix <- as.matrix(selectedData)

num_of_clusters <- 4
search_dist <- 100

km <- kmeans(selectedData, num_of_clusters)
clustered_data <- data.frame(selectedData,km$cluster) 
semantic_data <- data.frame() 


pal <- colorFactor(palette= brewer.pal(10,"Set3"), domain = c(1:10)) # 10 is the max num of clusters

allDataMap <- leaflet(selectedData) %>% 
              fitBounds(min(clustered_data$data.Lon),min(clustered_data$data.Lat),
                        max(clustered_data$data.Lon),max(clustered_data$data.Lat))  %>%
              
              addProviderTiles("CartoDB.DarkMatter", group="Dark") %>%
              addProviderTiles("Stamen.Toner", group = "Toner") %>%
              addTiles(group = "OSM") %>%
  
              addCircleMarkers(~selectedData$data.Lon, ~selectedData$data.Lat,
                               radius = 10, color = "black",fillColor=  ~pal(km$cluster), 
                               fillOpacity=0.5,  group = "Data Points")  %>%
  
              addLayersControl(baseGroups = c("Dark","Toner", "OSM"),
                               overlayGroups = c("Data Points", "Semantic Locations", "Semantics Summary"),
                               options = layersControlOptions(collapsed = FALSE)) %>% 
  
              addLegend("bottomleft",pal=pal,values = clustered_data$km.cluster,
                        labels = clustered_data$km.cluster, title = "data by clusters",opacity = 1)
                                 
allDataMap                              

# creating a table of the box values of each cluster
data_box_df <- data.frame() 

#adding the bbox of each cluster
for (i in 1:num_of_clusters){
  ci_data <- filter(clustered_data, km.cluster==i)
  min_lon <- min(ci_data$data.Lon)
  min_lat <- min(ci_data$data.Lat)
  max_lon <- max(ci_data$data.Lon)
  max_lat <- max(ci_data$data.Lat)
  data_box <- corner_bbox(left=min_lon, bottom=min_lat, right=max_lon, top=max_lat)
  
  data_box_df <- rbind(data_box_df,data_box)
  allDataMap <- allDataMap %>% 
                addPolylines(c(data_box["left"],  data_box["right"],data_box["right"],
                               data_box["left"],  data_box["left"]),
                             c(data_box["top"],   data_box["top"],   
                               data_box["bottom"],data_box["bottom"], data_box["top"]),
                             col=(pal(x = i)), opacity = 1)  
}

allDataMap

colnames(data_box_df) <- c("left", "bottom", "right","top")

src <- osmsource_api()

#############################################################################################################################

for (cNum in 1:num_of_clusters){ 
  
  temp_c_data <- filter(clustered_data,km.cluster==cNum)
  
  for (j in 1:nrow(temp_c_data)){
    bb <- center_bbox(temp_c_data[j,1], temp_c_data[j,2], search_dist, search_dist)
    ctown <- get_osm(bb, source = src)

################################################################################################################################
  semanticNodes <- find(ctown, node(tags((k == "amenity")|(k == "building")|(k == "emergency")|
                                           (k == "leisure")|(k == "office")|
                                           (k == "public_transport")|(k == "public transport")|
                                           (k == "shop")|(k == "sport")|(k == "tourism")
                                         )))
  
  semanticNodes <- find_down(ctown, node(semanticNodes))
  semanticNodes <- subset(ctown, ids = semanticNodes)
  semanticNodes
  
  if (nrow(semanticNodes$nodes$attrs) > 0) {     #check if there are any semantic nodes
    
    semanticNodes_attrs <- semanticNodes$nodes$attrs  
    semanticNodes_tags <- semanticNodes$nodes$tags
    
    testLocs1 <-semanticNodes_tags$k == "amenity" | semanticNodes_tags$k == "building" | 
                semanticNodes_tags$k == "emergency" | semanticNodes_tags$k == "leisure" | 
                semanticNodes_tags$k == "office" | semanticNodes_tags$k == "public_transport" | 
                semanticNodes_tags$k == "public transport" | semanticNodes_tags$k == "shop" | 
                semanticNodes_tags$k == "sport" | semanticNodes_tags$k == "tourism"
    
    semanticNodes_tags <- semanticNodes_tags[testLocs1,]    

    coords_table <- select(semanticNodes_attrs,id,lon,lat)
    semanticNodes_tags_join <- semanticNodes_tags %>% left_join(coords_table, by = "id")
    
    #render the semantic nodes+popups
    allDataMap <- allDataMap %>% 
                 addMarkers(~semanticNodes_tags_join$lon, ~semanticNodes_tags_join$lat, group = "Semantic Locations",
                            popup = ~htmlEscape(paste(semanticNodes_tags_join$k,":", semanticNodes_tags_join$v))
                            )
    
    ####################################################################################################################
    temp_column <- matrix(data=rep(cNum,nrow(semanticNodes_tags_join)),nrow = nrow(semanticNodes_tags_join), ncol=1)
    temp_column <- as.data.frame(temp_column)
    colnames(temp_column) <- c("cNum")
    temp_bind <- cbind(temp_column, semanticNodes_tags_join)
    semantic_data <- rbind(semantic_data,temp_bind)
    ####################################################################################################################
                            
  }#if- semanticNodes
  
  
#########################################################################################################################
  
  semanticWays <- find(ctown, way(tags((k == "amenity")|(k == "building")|(k == "emergency")|
                                         (k == "leisure")|(k == "office")|
                                         (k == "public_transport")|(k == "public transport")|
                                         (k == "shop")|(k == "sport")|(k == "tourism")
                                        )))
  
  semanticWays <- find_down(ctown, way(semanticWays))
  semanticWays <- subset(ctown, ids = semanticWays)
  semanticWays
 
  if (nrow(semanticWays$ways$attrs) > 0){
    semantic_ways_poly <- as_sp(semanticWays,"polygons") #Convert an osmar object to a sp object.the object will be converted in a SpatialPolygonsDataFrame. It consists of elements which are in obj$ways slot.
    ways_bbox <- bbox(semantic_ways_poly) #the bbox surrounding all polygons
    
    semanticWays_tags_join <- data.frame()
    
    # To add popups for polygons
    semanticWays_ways_attrs <- semanticWays$ways$attrs
    semanticWays_ways_tags <- semanticWays$ways$tags
    
    testLocs2 <-semanticWays_ways_tags$k == "amenity" |          semanticWays_ways_tags$k == "building" | 
                semanticWays_ways_tags$k == "emergency" |        semanticWays_ways_tags$k == "leisure" | 
                semanticWays_ways_tags$k == "office" |           semanticWays_ways_tags$k == "public_transport" | 
                semanticWays_ways_tags$k == "public transport" | semanticWays_ways_tags$k == "shop" | 
                semanticWays_ways_tags$k == "sport" |            semanticWays_ways_tags$k == "tourism"
    
    semanticWays_ways_tags <-  semanticWays_ways_tags[testLocs2,]
    
    #extracting from all polygons, jusy the specified semantic ones
    for(i in 1:length(semantic_ways_poly@polygons)){
      
      poly1 <- (semantic_ways_poly@polygons[i])  #list of polygons
      poly11 <- poly1[[1]]@Polygons              #list of 1 polygon
      poly11_id <- poly1[[1]]@ID
      amenity_V_index <- match(poly11_id,semanticWays_ways_tags$id, nomatch = 0) #the positions of poly11 in the tags data
      
      if (amenity_V_index != 0){
          poly1.coords <- poly11[[1]]@coords
          
          allDataMap <- allDataMap %>% 
                        addPolygons(lng = poly1.coords[,1], lat = poly1.coords[,2], 
                                    group = "Semantic Locations",
                                    color = (pal(x = cNum)), opacity =0.5, 
                                    popup =paste(semanticWays_ways_tags[amenity_V_index,"k"], ":",
                                                 semanticWays_ways_tags[amenity_V_index,"v"])) %>%
                        addMarkers(lng = mean(poly1.coords[,1]), lat = mean(poly1.coords[,2]),
                                   group = "Semantic Locations",
                                   popup =paste("Center of:", 
                                                semanticWays_ways_tags[amenity_V_index,"k"], ":",
                                                semanticWays_ways_tags[amenity_V_index,"v"]))
          
          mean_coords <- data.frame(lon=mean(poly1.coords[,1]), lat = mean(poly1.coords[,2]))
          semanticWays_tags_temp <- cbind(semanticWays_ways_tags[amenity_V_index,],mean_coords)
          semanticWays_tags_join <- rbind(semanticWays_tags_join,semanticWays_tags_temp)
          
       }#if
    } #for
    
    ##################################################################################################################################
    
    temp_column2 <- matrix(data=rep(cNum,nrow(semanticWays_tags_join)),nrow = nrow(semanticWays_tags_join), ncol=1)
    temp_column2 <- as.data.frame(temp_column2)
    colnames(temp_column2) <- c("cNum")
    temp_bind2 <- cbind(temp_column2, semanticWays_tags_join)
    semantic_data <- rbind(semantic_data,temp_bind2)
    ###################################################################################################################################

  } #if - semanticWays


#######################################################################################################################################
  

  } # for - all points in the cluster

} #for- cNum

allDataMap


#don't want to count the same semantic location in each cluster several times
unique_data <- unique(semantic_data)
row.names(unique_data) <- NULL

unique_semantic_data <- select(unique_data, cNum, k, v)

count_summary <- ddply(unique_semantic_data,.(cNum, k, v), nrow)


leafIcons <- icons(iconUrl = "http://icons.iconarchive.com/icons/dapino/office-women/48/eyes-office-women-glasses-icon.png",
                   iconWidth = 38, iconHeight = 95)

allDataMap2 <- allDataMap
for (i in 1:num_of_clusters) {
  summary <- filter(count_summary, cNum==i)
  summary <- select(summary, k,v, V1)
  
  content <- ""
  
  for (j in 1:nrow(summary)){
    t <- paste(summary[j,1], summary[j,2],": ", summary[j,3])
    content <- paste(sep = "<br/>",
                     content, 
                     t)
  }
   
  lon <- mean(c(data_box_df[i,"left"], data_box_df[i,"right"]))
  lat <- mean(c(data_box_df[i,"bottom"], data_box_df[i,"top"]))
  
  allDataMap2 <- allDataMap2 %>% addMarkers(lng = lon,lat = lat,
                                           icon=leafIcons, group="Semantics Summary", 
                                           popup = content
                                          )
}
              
allDataMap2



















