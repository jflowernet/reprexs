library(terra)

r <- rast(xmin = 0, xmax = 5, ymin = 0, ymax = 5, resolution = 1)
values(r) <- 1:ncell(r)

#Molleweide
proj_moll <- "ESRI:54009"

#create vector grid of raster
v_moll <- r |>
  as.polygons(dissolve = FALSE) |>
  project(proj_moll)

#project raster to Molleweide
r_moll <- project(r, proj_moll)

#plot
plot(r_moll, xlim = c(ext(v_moll)$xmin, ext(v_moll)$xmax), ylim = c(ext(v_moll)$ymin, ext(v_moll)$ymax))
lines(v_moll, col = "red", lwd = 2)
