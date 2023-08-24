library(sf)
library(rnaturalearth)

#get australia country polygon in lon lat
aus_lonlat <- ne_countries(country = "australia", returnclass = "sf") |>
  st_geometry()

#project to Mollweide
aus_moll <- aus_lonlat |>
  st_transform("ESRI:54009")

#define bounding box I'm interested in - this is for Australia's EEZ and is in Mollweide projection
polygon_bbox_moll <- st_bbox(c(xmin =9797297, xmax = 15540552, ymin = -5576498, ymax = -1096691), crs = st_crs("ESRI:54009")) |>
  st_as_sfc() %>% 
  st_as_sf()

#plot everything - Mollweide
plot(polygon_bbox_moll, lty = "dotted", axes=T)
plot(aus_moll, add=T)

#project bounding box to lon lat coordinates
polygon_bbox_lonlat <- polygon_bbox_moll|>
  st_transform(4326) 

#plot everything - lon-lat
plot(polygon_bbox_lonlat, lty = "dotted", axes=T)
plot(aus_lonlat, add=T)

#lon lat polygon has one less coordinate
st_coordinates(polygon_bbox_moll)

st_coordinates(polygon_bbox_lonlat)