library(dplyr)
library(ggplot2)
library(sf)

DF = data.frame(x=c(1,2,3,4,5), y1=c(2,4,6,8,10), y2=c(20,18,8,3,1))

#create lines as sf objects
line_1 <- DF %>%
  select(x, y1) %>% 
  #st_linestring wants a matrix
  as.matrix() %>%
  st_linestring()
  
line_2 <- DF %>%
  select(x, y2) %>% 
  as.matrix() %>%
  st_linestring()

#this gives you the intersection point, and converting it to a dataframe as I presume this is what you'd want
intersection_pt <- st_intersection(line_1, line_2) %>% 
  st_coordinates() %>% 
  as.data.frame()
  

ggplot() +
  geom_sf(data = line_1) +
  geom_sf(data = line_2) +
  geom_point(data = intersection_pt, aes(x= X, y = Y), color = "red")