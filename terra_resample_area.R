library(terra)

r <- rast(nrows = 1, ncols = 3)
values(r) <- c(3,5,7)
r2 <- rast(nrows = 1, ncols = 2)

plot(r)
text(r, digits = 1)
lines(as.polygons(r2), col = "red")

r_resample <- resample(r, r2, method = "sum")
plot(r_resample)
text(r_resample, digits = 1)

#this is agrees with manual area weighted sum: 
#left hand pixel
(3*1 + 5*0.5)

#right hand pixel 
(5*0.5 + 7*1)

#average

r_resample <- resample(r, r2, method = "average")
plot(r_resample)
text(r_resample, digits = 1)

#this agrees with my manual calculation following the gdal Github issue: https://github.com/OSGeo/gdal/pull/2363

#this is area weighted average: 
#left hand pixel
(3*1 + 5*0.5)/1.5

#right hand pixel 
(5*0.5 + 7*1)/1.5