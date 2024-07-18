library(gfwr)
library(sf)


#data and query from ?get_raster
data(test_shape)
test_data <- get_raster(spatial_resolution = 'HIGH',
           temporal_resolution = 'YEARLY',
           start_date = '2021-01-01',
           end_date = '2021-10-01',
           region = test_shape,
           region_source = 'USER_JSON')

#returned data does not cover full extent of test_shape: compare min and max
st_bbox(test_shape)
summary(test_data[1:2])

