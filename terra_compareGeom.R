library(terra)

v <- vect(rbind(c(10,10), c(0,60)), crs="+proj=merc")

v_dupes <- vect(rbind(c(10,10), c(0,60), c(0,60)), crs="+proj=merc")

compareGeom(v, v)

compareGeom(v_dupes)

gdal(lib = "all")
