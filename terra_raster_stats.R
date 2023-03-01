#mismatch betwen terra and raster package stats

library(raster)
library(terra)

# create a temporary filename for the downloaded file
f <- file.path(tempdir(), "coral.tif")

#increase timeout to 30mins to allow sufficient time to download the file which is 110MB  -download can be slow
options(timeout = 30*60)

download.file("https://store.pangaea.de/Publications/YessonC-etal_2016/YessonEtAl_DSR2_2016_AntipathariaHSM.tif", f)

#load via terra and raster
coral_terra <- rast(f)
coral_raster <- raster(f)

# all the following give min = 0 and max = 90 for the raster
minmax(coral_terra)

minValue(coral_raster)
maxValue(coral_raster)

rgdal::GDALinfo(f)

#read everything to disk - this requires about 20GB of memory due to the large raster size
coral_raster_on_disk <- readAll(coral_raster)

minValue(coral_raster_on_disk, min) #min is now 1
maxValue(coral_raster_on_disk) #max is now 92

#large file so remove it
rm(coral_raster_on_disk)
gc()

#read everything to disk for terra
set.values(coral_terra)

setMinMax(coral_terra, force=TRUE)
minmax(coral_terra) #min is still 0 and max is still 90

#remove the file
unlink(f)
