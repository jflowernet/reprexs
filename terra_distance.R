library(terra)

#this seems to work fine
r <- rast(ncols=10, nrows=10, ext = c(0,10, 0, 10))

r[21:28] <- 1
r[61:68] <- 2

d <- distance(r, values = TRUE)

plot(c(r, d), main = c("input", "nearest input value"))

#this works fine except the output value of 1 in the east (should be 3)
pts_buff <- vect(rbind(c(2,4), c(4,3), c(5, 6)), crs = crs(r)) |> 
  buffer(2e5) |> 
  cbind(c(1,2,3)) |>
  rasterize(r, field = "y")

d2 <- distance(pts_buff, values = TRUE)
plot(c(pts_buff,d2), main = c("input", "nearest input value"))

r2 <- rast(ncols=100, nrows=100, ext = c(0,10, 0, 10))

#things start going even more wrong at higher resolution

pts_buff2 <- vect(rbind(c(2,4), c(4,3), c(5, 6)), crs = crs(r)) |> 
  buffer(2e5) |> 
  cbind(c(1,2,3)) |>
  rasterize(r2, field = "y")

d3 <- distance(pts_buff2, values = TRUE)
plot(c(pts_buff2,d3), main = c("input", "nearest input value"))
