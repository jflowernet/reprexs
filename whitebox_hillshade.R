library(terra)
library(tmap)
library(whitebox)

wbt_init(exe_path = file.path("WBT", "whitebox_tools"))

data_folder <- "data"

dem <- rast(file.path(data_folder, "dem.tif"))

wbt_hillshade(dem = file.path(data_folder, "dem.tif"),
              output = file.path(data_folder, "hillshade.tif"))

hillshade <- rast(file.path(data_folder, "hillshade.tif"))

tm1 <- tm_shape(dem)+
  tm_raster(style = "cont", palette = "BrBG", title = "Height (m)") +
  tm_graticules() +
  tm_layout(legend.outside = TRUE, panel.labels = "DEM") 

tm2 <- tm_shape(hillshade)+
  tm_raster(style = "cont", palette = "-Greys", title = "Hillshade")+
  tm_graticules() +
  tm_layout(legend.outside = TRUE, panel.labels = "Hillshade")

tmap_arrange(tm1, tm2, nrow = 2)