#boundary matrix with sf issue

library(prioritizr)
library(terra)
library(tibble)
library(dplyr)

# set seed for reproducibility
set.seed(500)

# define number of fish spp
n_fish <- 4

# create a simple planning grid (assuming cost of each planning unit is equal)
pu_raster <- raster(
  nrows = 200, ncols = 200,
  xmn = 0, xmx = 200, ymn = 0, ymx = 200, vals = 1
)
# create data for fish species
fish_stack <- raster::stack(lapply(seq_len(n_fish), function(x) {
  setValues(pu_raster, round(runif(ncell(pu_raster)) > 0.5))
})) %>% setNames(paste0("fish_", seq_len(n_fish)))

plot(fish_stack)                 

# create sea mount data
seamount_raster <-
  st_multipoint(rbind(c(20,50), c(45, 70), c(65, 90), c(100, 100), c(120, 150), c(30, 160), c(10, 20), c(15, 80), c(20, 180))) %>%
  st_buffer(7) %>%
  as("Spatial") %>%
  rasterize(pu_raster, field = 1)

# create a separate layer for each sea mount
seamount_stack <-
  seamount_raster %>%
  as("SpatRaster") %>%
  terra::patches() %>%
  as("Raster") %>%
  {lapply(sort(unique(values(.))), function(i) {
    raster::Which(. == i)
  })} %>%
  raster::stack() %>%
  setNames(paste0("seamount_", seq_len(raster::nlayers(.))))

plot(seamount_stack)

# create grid-cell level planning unit data
## initialize data
pu_grid_data <-
  ### add in indices for planning units in raster to be organized
  tibble(id = as.list(seq_len(ncell(pu_raster)))) %>%
  ### add in cost data
  mutate(cost = raster::as.data.frame(pu_raster)[[1]]) %>%
  ### add in fish data
  bind_cols(as_tibble(raster::as.data.frame(fish_stack))) %>%
  ### add in seamount data
  mutate(seamount = 0)

pu_grid_data

# create sea mount-level planning unit data
pu_sm_data <- lapply(names(seamount_stack), function(i) {
  ## initialize data for planning unit that corresponds to i'th sea mount
  curr_sm_pu <-
    ## add in indices for planning units in raster to be organized
    tibble(
      id = list(raster::Which(seamount_stack[[i]] > 0.5, cells = TRUE))
    ) %>%
    ## add in cost data
    mutate(
      cost = raster::cellStats(
        pu_raster * seamount_stack[[i]], "sum", na.rm = TRUE
      )[[1]]
    ) %>%
    ## calculate total amount of each non-sea mount feature in i'th sea mount pu
    bind_cols(
      raster::as.data.frame(fish_stack * seamount_stack[[i]]) %>%
        setNames(names(fish_stack)) %>%
        dplyr::summarize_all(sum)
    ) %>%
    ## add data for i'th seamount
    mutate(
      seamount = unname(
        raster::cellStats(seamount_stack[[i]], "sum", na.rm = TRUE)
      )
    )
  ## return data
  curr_sm_pu
}) %>% do.call(what = bind_rows) %>% as_tibble()

pu_sm_data

# merge sea mount-level data and grid-cell level data togeather
pu_data <- bind_rows(pu_grid_data, pu_sm_data)

pu_data

#get seamount planning unit ids
seamount_pu_ids <- pu_data %>% 
  slice_tail(n = nlayers(seamount_stack)) %>% 
  pull(1)

pu_grid_sf <- rasterToPolygons(pu_raster, na.rm=FALSE) %>% 
  st_as_sf() %>% 
  dplyr::select(-1)

pu_sf <-
  pu_grid_sf %>%
  bind_rows(
    lapply(seamount_pu_ids, function(i) {
      st_sf(layer = 1, geometry = st_union(pu_grid_sf[i, , drop = FALSE]))
    }) %>%
      do.call(what = bind_rows) %>%
      dplyr::select(-layer)
  )

boundary_matrix(pu_sf)

