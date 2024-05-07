#terra rasterize polygons mean not working as expected

library(terra)

#some polygon data
polys <- vect(system.file("ex/lux.shp", package="terra"))

#create raster of same extent
ras <- rast(polys, ncol = 75, nrow = 100)

#plot the polygon data for population
plot(polys, "POP", type = "classes")

#rasterize the polygons using the population column
rasterized_polys <- rasterize(polys, ras, field = "POP", fun = "mean")

#plot
plot(rasterized_polys, type = "classes")
lines(polys)