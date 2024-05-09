library(terra)

r <- rast(system.file("ex/elev.tif", package="terra"))

plet(r)

plet(r, tiles = "Streets")

packageVersion("leaflet")

gdal(lib="gdal")