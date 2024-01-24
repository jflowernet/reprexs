library(terra)

#project and resample rather than project then resample

a <- rast(ncols=40, nrows=40, xmin=0, xmax= 10, ymin=0, ymax=10)
values(a) <- 1:ncell(a)

plot(a)

template_for_resampling <- rast(ncols=5, nrows=5, xmin=-1, xmax= 11, ymin=-1, ymax=11)

template_vect <- as.polygons(template_for_resampling)

resample_method <- c("near", "bilinear", "average")

results_list <- list()

for(i in 1:length(resample_method)){
  results_list[[i]] <- resample(a, template_for_resampling, method = resample_method[i])
}

results_rast <- rast(results_list)

panel(results_rast, fun = function(x)lines(template_vect), main = resample_method)
