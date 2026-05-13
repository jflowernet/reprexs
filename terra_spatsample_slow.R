library(terra)
packageVersion("terra")

r <- rast(nrow = 1000, ncol = 3000)
values(r) <- rep(c(0,1,NA), ncell(r)/3)

ncell(r)

system.time(spatSample(r, 500, na.rm = TRUE))

#really big raster
r2 <- rast(nrow = 3000, ncol = 30000)
values(r2) <- rep(c(0,1,NA), ncell(r2)/3)

ncell(r2)

system.time(spatSample(r2, 500, na.rm = TRUE))

detach("package:terra", unload = TRUE)

library(terra, lib.loc = .libPaths()[3])
packageVersion("terra")

r <- rast(nrow = 1000, ncol = 3000)
values(r) <- rep(c(0,1,NA), ncell(r)/3)

ncell(r)

system.time(spatSample(r, 500, na.rm = TRUE))

#really big raster
r2 <- rast(nrow = 3000, ncol = 30000)
values(r2) <- rep(c(0,1,NA), ncell(r2)/3)

ncell(r2)

system.time(spatSample(r2, 500, na.rm = TRUE))
