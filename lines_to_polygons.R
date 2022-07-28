library(sf)

#set-up some lines for example and plot to see how they look
shore <- st_linestring(rbind(c(3,1), c(2,10)))
seaward <- st_linestring(rbind(c(5,2), c(7,5), c(5,11)))
north <- st_linestring(rbind(c(1,9),c(8,11)))
south <- st_linestring(rbind(c(1,3), c(8,2)))
plot(shore, col = "brown")
plot(seaward, col = "skyblue", add=TRUE)
plot(north, col = "red", add=TRUE)
plot(south, col = "green", add=TRUE)

#get bounding box for north south
bbox_ns <- st_union(north, south) %>% 
  st_bbox()

#union all the lines into one multi-linestring
bounded_coast <- st_union(shore, seaward) %>% 
  st_union(north) %>% 
  st_union(south)

#polygonize that to get the polygon we want
coast_polygon <- st_polygonize(bounded_coast)

#plot everything
plot(coast_polygon, col = "blue")
plot(shore, col = "black", add=TRUE)
plot(seaward, col = "skyblue", add=TRUE)
plot(north, col = "red", add=TRUE)
plot(south, col = "green", add=TRUE)