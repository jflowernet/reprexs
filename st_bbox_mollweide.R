library(sf)
library(rnaturalearth)

#get OZ country polygon and project to Molleweide
aus <- ne_countries(country = "australia", returnclass = "sf") |>
  st_geometry() %>% 
  st_transform("ESRI:54009")

aus_4326 <- st_transform(aus, 4326)
  
#get bbox and project to 4326
aus_bbox_projected <- aus |>
  st_bbox() |>
  st_as_sfc() |>
  st_transform(4326)

#get new bbox of polygon in 4326 projection
aus_bbox_4326 <- aus_bbox_projected |> 
  st_bbox() |>
  st_as_sfc() 

#plot everything
plot(aus_bbox_4326, lty = "dashed", axes=T)
plot(aus_bbox_projected, lty = "dotted", add=T)
plot(aus_4326, add=T)


#Same code follows, but using eez rather than country polygon

#get Oz eez
eez <- offshoredatr::get_area("Australia") %>% 
  st_geometry() %>% 
  st_transform("ESRI:54009")

eez_4326 <- st_transform(eez, 4326)

#get bbox and project
eez_bbox_projected <- eez |>
  st_bbox() |>
  st_as_sfc() |>
  st_transform(4326) 

#get new bbox of polygon in 4326 projection
eez_bbox_4326 <- eez_bbox_projected |> 
  st_bbox() |>
  st_as_sfc() 

#plot everything
plot(eez_bbox_4326, lty = "dashed", axes=T)
plot(eez_bbox_projected, lty = "dotted", add=T)
plot(eez_4326, add=T)