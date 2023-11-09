library(terra)
library(geodata)

#equal area projection for Fiji from projectionwizard.org
fiji_crs <- "+proj=laea +lon_0=-181.8896484 +lat_0=-17.73775 +datum=WGS84 +units=m +no_defs"

#get temperature data for Fiji
fiji_temp <- bio_oracle(path=tempdir(), "Temperature", "Mean", benthic=FALSE, time="Present")

#split data into left and right hand side of the antimeridian (just taking the first raster from the stack of rasters for this example)
fiji_temp_lhs <- crop(fiji_temp[[1]], ext(176, 180, -21, -12))
fiji_temp_rhs <- crop(fiji_temp[[1]], ext(-180, -177, -21, -12))

#project the two halves - they now have slightly different resolution
fiji_temp_list <- lapply(list(fiji_temp_lhs, fiji_temp_rhs), function(x) project(x, fiji_crs))

fiji_temp_list

# resample the RHS raster to have the same resolution as the LHS
fiji_temp_list[[2]] <- resample(fiji_temp_list[[2]], rast(extent = ext(fiji_temp_list[[2]]), res = res(fiji_temp_list[[1]]), crs = crs(fiji_temp_list[[2]])))

#can now merge the resulting rasters but there are missing values where the two halves of the data have been merged
fiji_temp_list |>
  sprc() |>
  merge() |>
  plot()

#build a bounding box covering the area
vect(ext(176, ))