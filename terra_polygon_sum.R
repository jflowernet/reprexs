library(terra)

v <- vect(system.file("ex/lux.shp", package="terra"))

v <- aggregate(v, by = "ID_1")[,1]

#create two circles overlaying vector v
circles <- vect(rbind(c(6.05, 49.6), c(6.1, 49.8)), crs = "epsg:4326") |>
  buffer(10e3)

circles$ID_1 <- 4:5

#bind everything into a single vector
v2 <- rbind(v, circles)

plot(v2, "ID_1", alpha = 0.8, col = map.pal("roygbiv"))

#I want to sum the polygons using "ID_1"
#aggregating doesn't sum overlapping areas: the bottom circle should have a value of 3+4 = 7, but it is just overlaying the aggregated polygon with value (ID_1) = 3
v2_agg <- aggregate(v2, by = "ID_1", fun = "sum") 

par(mfrow =c(1,2))
  
plot(v2_agg, "ID_1", alpha = 0.8, col = map.pal("roygbiv"))

#response from Robert 

u <- union(v2)
s <- rowSums(values(u) * v2$ID_1[col(u)])
u$sum <- s
# or
values(u) <- data.frame(sum=s)

##new answer!
u <- union(v2)

#this creates new geometry 'cut-outs' where polygons overlap:
par(mfrow = c(3,2))
for(i in 1:nrow(u)){plot(u[i,], col = 1)}
par(mfrow = c(1,1))

#so 6 geometries as opposed to the original 5

#next part gets the values that should go with each of those geometries:
#values(u) is a dataframe with id columns 1 to 5 representing the original 5 geometries, and a value of 1 in that column indicating if that geometry overlaps with the new geometry (each row). This dataframe is transposed and multiplied by the values of each of the original geometries, so that each column is now the values that fall within each (new) geometry. Summing these columns therefore gives the sum of values in each polygon
s <- colSums(v2$ID_1 * t(values(u)))

# the values are attached to the geometry, overwriting any other values of 'u'
values(u) <- data.frame(sum=s)

plot(u, "sum", col = map.pal("roygbiv"))
