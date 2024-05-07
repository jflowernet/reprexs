library(terra)

r <- rast(xmin = 0, xmax = 5, ymin = 0, ymax = 5, resolution = 1)
values(r) <- 1:ncell(r)

v <- ext(0, 6, 0, 6) |>
  vect() |>
  set.crs("epsg:4326")

#works as expected
plot(r, ext = ext(v))
lines(v, col = "red", lwd = 2)

#Albers Africa projection
proj_albers <- "ESRI:102022"

r_albers <- project(r, proj_albers)

v_albers <- project(v, proj_albers)

plot(r_albers)

#larger extent, but not encompassing full extent of v_albers as I would expect
plot(r_albers, ext = ext(v_albers))
lines(v_albers, col = "red", lwd = 2)

#works as expected: extent of v_albers
plot(r_albers, xlim = c(ext(v_albers)$xmin, ext(v_albers)$xmax), ylim = c(ext(v_albers)$ymin, ext(v_albers)$ymax))
lines(v_albers, col = "red", lwd = 2)

gdal()
