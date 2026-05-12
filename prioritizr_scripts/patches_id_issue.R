# load packages
library(prioritizr)
library(terra)
library(tibble)
library(dplyr)
library(sf)

# set seed for reproducibility
set.seed(500)

# define number of fish spp
n_fish <- 4

# create a simple planning grid (assuming cost of each planning unit is equal)
pu_raster <- raster::raster(
  nrows = 20, ncols = 20,
  xmn = 0, xmx = 20, ymn = 0, ymx = 20, vals = 1
)

# create data for fish species
fish_stack <- raster::stack(lapply(seq_len(n_fish), function(x) {
  setValues(pu_raster, round(runif(ncell(pu_raster)) > 0.5))
})) %>% setNames(paste0("fish_", seq_len(n_fish)))

# create sea mount data
seamount_raster <-
  st_multipoint(rbind(c(2, 2), c(8, 17), c(17, 5))) %>%
  st_buffer(2) %>%
  as("Spatial") %>%
  raster::rasterize(pu_raster, field = 1)

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

# merge sea mount-level data and grid-cell level data togeather
pu_data <- bind_rows(pu_grid_data, pu_sm_data)

# calculate feature data
feature_data <-
  ## initialize data with the name and total amount of each feature
  ## note we use seamount_raster here because we want to set targets
  ## based on overall distribution of seamounts (not each one separately)
  raster::stack(fish_stack, setNames(seamount_raster, "seamount")) %>%
  raster::cellStats(sum, na.rm = TRUE) %>%
  {tibble(id = seq_along(.), name = names(.), total = unname(.))} %>%
  ## set targets, let's use 20% as an example
  mutate(rel_target = 0.2) %>%
  ## now compute the targets as absolute values
  ## (this is needed because we have spatial overlaps in the planning unit
  ## data, so the  if we gave prioritizr relative targets then it wouldn't
  ## calculate the percentages correctly)
  mutate(abs_target = rel_target * total)

# create a conservation planning problem
## formulate problem with all the pu and feature data
prob <-
  problem(x = pu_data, features = feature_data$name, cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_absolute_targets(feature_data$abs_target) %>%
  add_binary_decisions() %>%
  add_default_solver(gap = 0)

## add in linear constraints to ensure that the solution won't select
## spatially overlapping grid-cell level and seamount-level planning units
for (i in seq_len(nrow(pu_sm_data))) {
  for (j in pu_sm_data$id[[i]]) {
    ### specify planning unit indices for constraints
    v <- rep(0, nrow(pu_data))      # initialize with zeros
    v[nrow(pu_grid_data) + i] <- 1  # specify seamount-level planning unit
    v[j] <- 1                       # specify grid cell-level planning unit
    ### add constraints to problem
    prob <- add_linear_constraints(prob, threshold = 1, sense = "<=", data = v)
  }
}

# solve problem
raw_sol <- solve(prob)

# since the solution contains a combination of grid cell-level and
# seamount-level planning units, we need to do some post-processing
# to identify which "real" planning units (i.e. raster pixels) were selected
pu_ids <- unique(unlist(pu_data$id[which(raw_sol$solution_1 > 0.5)]))

# we can now use these values to create a raster
sol <- pu_raster * 0
sol[pu_ids] <- 1
names(sol) <- "solution"

# plot solution
plot(sol)