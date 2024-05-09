library(terra)
#> terra 1.7.71
set.seed(0)
r <- rast(nrows = 10, ncols = 10)
values(r) <- sample(3, ncell(r), replace = TRUE)
cls <- data.frame(id = 1:3, cover = c("forest", "water", "urban"))
levels(r) <- cls

class(levels(r))

#multi-layer raster
r2 <- c(r,r)

levels(r2)

