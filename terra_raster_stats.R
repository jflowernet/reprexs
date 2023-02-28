#mismatch betwen terra and raster package stats

library(raster)
library(terra)

# create a temporary filename for the downloaded file
f <- file.path(tempdir(), "coral.tif")

#increase timeout to 10mins to allow sufficient time to download the file which is 110MB
options(timeout = 30*60)

download.file("https://store.pangaea.de/Publications/YessonC-etal_2016/YessonEtAl_DSR2_2016_AntipathariaHSM.tif", f)

f <- file.path("../../Documents/Gdrive_sync/emlab_shared/emlab/data/antipatharia-distribution/YessonEtAl_DSR2_2016_AntipathariaHSM.tif")

coral_terra <- rast(f)
coral_raster <- raster(f)

minmax(coral_terra)

minValue(coral_raster)
maxValue(coral_raster)

rgdal::GDALinfo(f)

#read everything to disk
coral_raster_on_disk <- readAll(coral_raster)
minmax(coral_terra, compute=FALSE)
#remove the file
unlink(f)
