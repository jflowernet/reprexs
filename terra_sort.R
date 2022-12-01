library(terra)

#borrowing from the sort() help page
f <- system.file("ex/elev.tif", package="terra")
r <- rast(f)
r <- c(r, r/2, r*2)

#names the raster layers in order
names(r) <- c("one", "two", "three")

plot(r)

#sorting them re-orders the rasters, but not the names
plot(sort(r))