library(terra)
library(geodata)
library(dplyr)

#GET SOME DATA

#California state boundary
ca_boundary <- geodata::gadm(country = "usa", level = 1, path = tempdir()) |>
  subset(NAME_1 == "California", NSE = TRUE)

#Global monthly average temperature data (returns raster with 12 layers - 1 for each month)
world_monthly_avg_temp <- geodata::worldclim_global(var = "tavg", res = 10, path = tempdir())
######################################

df_extracted <- terra::extract(world_monthly_avg_temp, ca_boundary, ID= FALSE, xy = TRUE, exact = TRUE) %>% #returns data frame with first 12 columns as monthly temperature data, then x and y coordinates for cell centers, and final column is fraction of that cell that is covered by the ca_boundary polygon
  dplyr::mutate(dplyr::across(1:terra::nlyr(world_monthly_avg_temp), ~.*fraction)) %>% #multiply each data column (in this case temperature) by the fraction of the cell that is covered by the polygon: note the resulting data frame drops the original data
  dplyr::select(x, y, 1:terra::nlyr(world_monthly_avg_temp)) #to make a raster we need x, y and then our data

#now we can conver the data frame back into a raster
ca_ras <- terra::rast(df_extracted, type = "xyz", crs = "epsg:4326")

plot(ca_ras) #can see that cells along edges have lower values as they are multiplied by fraction of the polygon they overlap

#aggregate (reduce resolution) of the raster by factor of 2, taking average values for new cells
ca_ras_resampled <- terra::aggregate(ca_ras, fact = 2, fun = "mean")

plot(ca_ras_resampled)
