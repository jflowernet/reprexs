library(terra)

v <- vect(system.file("ex/lux.shp", package="terra"))

#create two circles overlaying vector v
circles <- vect(rbind(c(6.05, 49.6), c(6.1, 49.8)), crs = "epsg:4326") |>
  buffer(10e3)

circles$ID_1 <- 4:5

#bind everything into a single vector
v2 <- rbind(v, circles)

plot(v2, "ID_1", alpha = 0.8)

#I want to sum the polygons using "ID_1"
#aggregating doesn't sum overlapping areas: the bottom circle should have a value of 3+4 = 7, but it is just overlaying the aggregated polygon with value (ID_1) = 3
v2_agg <- aggregate(v2, by = "ID_1", fun = "sum") 
  
plot(v2_agg, "ID_1", alpha = 0.8)

#response from Robert 

u <- union(v2)
s <- rowSums(values(u) * v2$ID_1)
u$sum <- s
# or
values(u) <- data.frame(sum=s)

plot(u, "sum")
