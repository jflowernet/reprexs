library(tmap)
library(terra)
library(sf)
packageVersion("tmap")

#Bounding box for South America
south_america <- rnaturalearth::ne_countries(continent = "South America", returnclass = "sv")

bbox_terra <- ext(south_america)

tm_shape(vect(World)) + 
  tm_polygons("HPI") +
  tm_crs("auto", bbox = bbox_terra)

bbox_sf <- st_bbox(st_as_sf(south_america))

#works
tm_shape(World, bbox = bbox_sf) + 
  tm_polygons("HPI")

#doesn't
tm_shape(vect(World), bbox = bbox_terra) + 
  tm_polygons("HPI")

#works
tm_shape(vect(World), bbox = st_bbox(bbox_terra)) + 
  tm_polygons("HPI")



