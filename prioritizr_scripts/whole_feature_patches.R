library(prioritizr)

#create a simple planning grid
pu_raster <- raster(nrows = 20, ncols = 20, xmn = 0, xmx = 20, ymn = 0, ymx = 20, vals = 0)

#create some areas - 3 points that are buffered
seamount_areas <- st_multipoint(rbind(c(2, 2), c(8, 17), c(17, 5))) %>% 
  st_buffer(2) %>% 
  as("Spatial")

#rasterize the seamounts
seamount_raster <- rasterize(seamount_areas, pu_raster, field = 1)

plot(seamount_raster)

#these would be the grid-cell level and seamount-level planning units then? With each seamount a separate unit
par(mfrow = c(1,2))

plot(rasterToPolygons(pu_raster))

plot(rasterToPolygons(seamount_raster, dissolve = TRUE))
