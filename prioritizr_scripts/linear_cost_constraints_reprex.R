# Reprex for prioritization
 
# Load libraries
library(raster)
library(sf)
library(dplyr)
library(prioritizr)
library(prioritizrdata)

# Load data
data(salt_pu)
data(salt_features)

# Create planning unit raster
pu_raster <- salt_pu
pu_raster[!is.na(pu_raster)] <- 1

plot(pu_raster, main = "Planning Area")

# Plot cost layer
plot(salt_pu, main = "Cost") 

# Plot features
plot(salt_features)

# Create seamount data 
seamount_df<- as.data.frame(pu_raster, xy = T) %>% 
  filter(!is.na(salt_pu))

set.seed(1234)
seamount_df <- seamount_df[sample(1:nrow(seamount_df), size = 3),]

seamount_raster <- seamount_df %>% 
  sf::st_as_sf(., coords = c("x","y"), crs = st_crs(pu_raster)) %>% 
  st_buffer(700) %>% 
  rasterize(pu_raster, field = 1)

# Create dummy cost option (== 1) and add original cost option to features
features <- stack(salt_features, salt_pu)
names(features) <- c(names(salt_features), "cost_option")

cost <- pu_raster # essentially cost == 1

###
# Run first using these targets
###

# Create features targets
targets <- c(rep(0.1, nlayers(salt_features)), 0.0005) # 10% of features and 0.05% of the "cost"
seamounts_target <- 0.1 # 10% of all seamounts, using whole seamount features

# Create a separate layer for each seamount group 
seamount_groups <-
  seamount_raster %>%
  as("SpatRaster") %>%
  terra::patches(directions = 4) %>%
  as("Raster") 

# Create a stack with each seamount group as a layer
seamount_stack <- seamount_groups %>%
  {lapply(sort(unique(values(.))), function(i) {
    raster::Which(. == i)
  })} %>%
  raster::stack() %>%
  setNames(paste0("seamount_", seq_len(raster::nlayers(.))))

plot(seamount_stack)

# Create grid-cell level planning unit data
# Initialize data
pu_grid_data <-
  ### add in indices for planning units in raster to be organized
  tibble(id = as.list(seq_len(ncell(pu_raster)))) %>%
  ### add in cost data 
  bind_cols(as_tibble(raster::as.data.frame(cost))) %>%
  ### add in feature data
  bind_cols(as_tibble(raster::as.data.frame(features))) %>%
  ### add in seamount data
  mutate(seamount = 0)

# Create seamount level planning unit data
pu_sm_data <- lapply(names(seamount_stack), function(i) {
  ## initialize data for planning unit that corresponds to i'th sea mount
  curr_sm_pu <-
    ## add in indices for planning units in raster to be organized
    tibble(
      id = list(raster::Which(seamount_stack[[i]] > 0.5, cells = TRUE))
    ) %>%
    ## add in cost data - modified for multiple cost inputs
    bind_cols(
      raster::as.data.frame(cost* seamount_stack[[i]]* 0) %>% 
        setNames(names(cost)) %>% 
        dplyr::summarize_all(sum, na.rm=TRUE)
    ) %>% 
    ## calculate total amount of each non-sea mount feature in i'th seamount pu
    bind_cols(
      raster::as.data.frame(features * seamount_stack[[i]]) %>%
        setNames(names(features)) %>%
        dplyr::summarize_all(sum, na.rm = TRUE)
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

# Merge seamount-level data and grid-cell level data together
pu_data <- bind_rows(pu_grid_data, pu_sm_data)

# calculate feature data
feature_data <-
  ## initialize data with the name and total amount of each feature
  ## note we use seamount_raster here because we want to set targets
  ## based on overall distribution of seamounts (not each one separately)
  raster::stack(features, setNames(seamount_raster, "seamount")) %>%
  raster::cellStats(sum, na.rm = TRUE) %>%
  {tibble(id = seq_along(.), name = names(.), total = unname(.))} %>%
  ## set targets
  mutate(rel_target = c(targets, seamounts_target)) %>%
  ## now compute the targets as absolute values
  ## (this is needed because we have spatial overlaps in the planning unit
  ## data, so if we gave prioritizr relative targets then it wouldn't
  ## calculate the percentages correctly)
  mutate(abs_target = rel_target * total)

# Add in constraints to ensure that the solution won't select
# spatially overlapping grid-cell level and seamount-level planning units
constraints <- data.frame(test  = rep(0,nrow(pu_data)))
index <- 1

# This makes a vector with a '1' for each combination of seamount level planning unit and     
# grid-cell level planning unit that overlaps with that seamount. 
for (i in seq_len(nrow(pu_sm_data))) {
  print(paste0("Processing seamount ", i, " of ", nrow(pu_sm_data)))
  
  for (j in pu_sm_data$id[[i]]) {
    
    ### specify planning unit indices for constraints
    v <- rep(0, nrow(pu_data))      # initialize with zeros
    v[nrow(pu_grid_data) + i] <- 1  # specify seamount-level planning unit
    v[j] <- 1                       # specify grid cell-level planning unit
    
    constraints <- cbind(constraints, v)
    index <- index+1
  }
}

# Remove dummy row
constraints <- constraints[, -1]

# Add the constraints as features to the planning data
pu_data_final <- constraints %>% 
  setNames(sprintf("constraint_%d", seq.int(1:ncol(.)))) %>% 
  bind_cols(pu_data, .)

# Add constraints features to feature data which contains target
feature_data_w_constraints <- tibble(id = seq(from = nrow(feature_data)+1, to = (nrow(feature_data) +     length(constraints)), by = 1), total = 2) %>% 
  mutate(name = sprintf("constraint_%d", id-nrow(feature_data)), .after = id) %>% 
  mutate(rel_target = NA_real_,
         abs_target = 1) %>% 
  bind_rows(feature_data, .)

# Create targets dataframe for manual targets
# These targets are absolute and the targets for the features are all >=, 
# i.e. have to meet or exceed the target, whereas the constrain targets are 
# all <= 1, which means you cannot select the same planning unit     
# at the grid cell level and seamount level planning grid, i.e. you can't select 
# both the seamount and then select grid-cells that overlap the seamount, 
# only one or the other.
manual_targets <- tibble(feature = feature_data_w_constraints$name,
                         type = "absolute",
                         sense = c(rep(">=", nrow(feature_data)), rep("<=", length(constraints))),
                         target = c(pull(feature_data, abs_target), rep(1, length(constraints))))

# Update manual targets so that the cost option is less than or equal to xx%
manual_targets <- manual_targets %>% 
  mutate(sense = ifelse(feature == "cost_option", "<=", sense))

# Create an sf object with all pu data
pu_grid_sf <- rasterToPolygons(pu_raster, na.rm=FALSE) %>% 
  st_as_sf() %>% 
  dplyr::select(-1)

# Get patches in the right order
pu_sf <-
  pu_grid_sf %>%
  bind_rows(
    lapply(pu_sm_data$id, function(i) {
      st_sf(layer = 1, geometry = st_union(pu_grid_sf[i, , drop = FALSE]))
    }) %>%
      do.call(what = bind_rows) %>%
      dplyr::select(-layer)
  )

# Create boundary matrix
bnd_mat <- boundary_matrix(pu_sf)

# Establish problem
p <- problem(x = pu_data_final, features = feature_data_w_constraints$name, 
             cost_column = names(cost)) %>%
  add_min_set_objective() %>%
  add_manual_targets(manual_targets) %>%
  add_binary_decisions() %>%
  add_boundary_penalties(penalty = 0.0001, data = bnd_mat) %>% 
  add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1, time_limit = 60*5)

# Solve
sp <- solve(p)

# Since the solution contains a combination of grid cell-level and
# seamount-level planning units, we need to do some post-processing
# to identify which "real" planning units (i.e. raster pixels) were selected
pu_ids <- unique(unlist(pu_data$id[which(sp$solution_1 > 0.5)]))

# We can now use these values to create a raster
solution <- pu_raster * 0
solution[pu_ids] <- 1
names(solution) <- "cost_option"

# Save solution for later
solution_stack <- stack(solution)

# Show solution
plot(solution, main = "Solution")

# Show % of features and seamounts in solution
cellStats(addLayer(features, seamount_raster)*solution, sum, na.rm = TRUE) %>%
  bind_cols(feature_data, .) %>% 
  rename(abs_held = ncol(.)) %>% 
  mutate(relative_held = sprintf("%0.2f", 100*abs_held/total)) %>% 
  relocate(rel_target, .before = relative_held) %>% 
  dplyr::select(-1)

###
## Try without constraints and with cost
###

# Establish problem
p <- problem(x = pu_data_final, features = feature_data_w_constraints$name[-6], 
             cost_column = "cost_option") %>%
  add_min_set_objective() %>%
  add_manual_targets(manual_targets[-6,]) %>%
  add_binary_decisions() %>%
  add_boundary_penalties(penalty = 0, data = bnd_mat) %>% 
  add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1, time_limit = 60*5)

# Solve
sp <- solve(p)

# Since the solution contains a combination of grid cell-level and
# seamount-level planning units, we need to do some post-processing
# to identify which "real" planning units (i.e. raster pixels) were selected
pu_ids <- unique(unlist(pu_data$id[which(sp$solution_1 > 0.5)]))

# We can now use these values to create a raster
solution <- pu_raster * 0
solution[pu_ids] <- 1
names(solution) <- "cost_option"

# Save solution for later
solution_stack <- stack(solution_stack, solution)

# Show solution
plot(solution, main = "Solution")

# Show % of features and seamounts in solution
cellStats(addLayer(features, seamount_raster)*solution, sum, na.rm = TRUE) %>%
  bind_cols(feature_data, .) %>% 
  rename(abs_held = ncol(.)) %>% 
  mutate(relative_held = sprintf("%0.2f", 100*abs_held/total)) %>% 
  relocate(rel_target, .before = relative_held) %>% 
  dplyr::select(-1)


###
# Now run with different cost targets
###

# Create features targets
targets <- c(rep(0.1, nlayers(salt_features)), 0.1) # 10% of features and 10% of the "cost"
seamounts_target <- 0.1 # 10% of all seamounts, using whole seamount features

# Create a separate layer for each seamount group 
seamount_groups <-
  seamount_raster %>%
  as("SpatRaster") %>%
  terra::patches(directions = 4) %>%
  as("Raster") 

# Create a stack with each seamount group as a layer
seamount_stack <- seamount_groups %>%
  {lapply(sort(unique(values(.))), function(i) {
    raster::Which(. == i)
  })} %>%
  raster::stack() %>%
  setNames(paste0("seamount_", seq_len(raster::nlayers(.))))

# Create grid-cell level planning unit data
# Initialize data
pu_grid_data <-
  ### add in indices for planning units in raster to be organized
  tibble(id = as.list(seq_len(ncell(pu_raster)))) %>%
  ### add in cost data 
  bind_cols(as_tibble(raster::as.data.frame(cost))) %>%
  ### add in feature data
  bind_cols(as_tibble(raster::as.data.frame(features))) %>%
  ### add in seamount data
  mutate(seamount = 0)

# Create seamount level planning unit data
pu_sm_data <- lapply(names(seamount_stack), function(i) {
  ## initialize data for planning unit that corresponds to i'th sea mount
  curr_sm_pu <-
    ## add in indices for planning units in raster to be organized
    tibble(
      id = list(raster::Which(seamount_stack[[i]] > 0.5, cells = TRUE))
    ) %>%
    ## add in cost data - modified for multiple cost inputs
    bind_cols(
      raster::as.data.frame(cost* seamount_stack[[i]]* 0) %>% 
        setNames(names(cost)) %>% 
        dplyr::summarize_all(sum, na.rm=TRUE)
    ) %>% 
    ## calculate total amount of each non-sea mount feature in i'th seamount pu
    bind_cols(
      raster::as.data.frame(features * seamount_stack[[i]]) %>%
        setNames(names(features)) %>%
        dplyr::summarize_all(sum, na.rm = TRUE)
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

# Merge seamount-level data and grid-cell level data together
pu_data <- bind_rows(pu_grid_data, pu_sm_data)

# calculate feature data
feature_data <-
  ## initialize data with the name and total amount of each feature
  ## note we use seamount_raster here because we want to set targets
  ## based on overall distribution of seamounts (not each one separately)
  raster::stack(features, setNames(seamount_raster, "seamount")) %>%
  raster::cellStats(sum, na.rm = TRUE) %>%
  {tibble(id = seq_along(.), name = names(.), total = unname(.))} %>%
  ## set targets
  mutate(rel_target = c(targets, seamounts_target)) %>%
  ## now compute the targets as absolute values
  ## (this is needed because we have spatial overlaps in the planning unit
  ## data, so if we gave prioritizr relative targets then it wouldn't
  ## calculate the percentages correctly)
  mutate(abs_target = rel_target * total)

# Add in constraints to ensure that the solution won't select
# spatially overlapping grid-cell level and seamount-level planning units
constraints <- data.frame(test  = rep(0,nrow(pu_data)))
index <- 1

# This makes a vector with a '1' for each combination of seamount level planning unit and     
# grid-cell level planning unit that overlaps with that seamount. 
for (i in seq_len(nrow(pu_sm_data))) {
  print(paste0("Processing seamount ", i, " of ", nrow(pu_sm_data)))
  
  for (j in pu_sm_data$id[[i]]) {
    
    ### specify planning unit indices for constraints
    v <- rep(0, nrow(pu_data))      # initialize with zeros
    v[nrow(pu_grid_data) + i] <- 1  # specify seamount-level planning unit
    v[j] <- 1                       # specify grid cell-level planning unit
    
    constraints <- cbind(constraints, v)
    index <- index+1
  }
}

# Remove dummy row
constraints <- constraints[, -1]

# Add the constraints as features to the planning data
pu_data_final <- constraints %>% 
  setNames(sprintf("constraint_%d", seq.int(1:ncol(.)))) %>% 
  bind_cols(pu_data, .)

# Add constraints features to feature data which contains target
feature_data_w_constraints <- tibble(id = seq(from = nrow(feature_data)+1, to = (nrow(feature_data) +     length(constraints)), by = 1), total = 2) %>% 
  mutate(name = sprintf("constraint_%d", id-nrow(feature_data)), .after = id) %>% 
  mutate(rel_target = NA_real_,
         abs_target = 1) %>% 
  bind_rows(feature_data, .)

# Create targets dataframe for manual targets
# These targets are absolute and the targets for the features are all >=, 
# i.e. have to meet or exceed the target, whereas the constrain targets are 
# all <= 1, which means you cannot select the same planning unit     
# at the grid cell level and seamount level planning grid, i.e. you can't select 
# both the seamount and then select grid-cells that overlap the seamount, 
# only one or the other.
manual_targets <- tibble(feature = feature_data_w_constraints$name,
                         type = "absolute",
                         sense = c(rep(">=", nrow(feature_data)), rep("<=", length(constraints))),
                         target = c(pull(feature_data, abs_target), rep(1, length(constraints))))

# Update manual targets so that the cost option is less than or equal to xx%
manual_targets <- manual_targets %>% 
  mutate(sense = ifelse(feature == "cost_option", "<=", sense))

# Create an sf object with all pu data
pu_grid_sf <- rasterToPolygons(pu_raster, na.rm=FALSE) %>% 
  st_as_sf() %>% 
  dplyr::select(-1)

# Get patches in the right order
pu_sf <-
  pu_grid_sf %>%
  bind_rows(
    lapply(pu_sm_data$id, function(i) {
      st_sf(layer = 1, geometry = st_union(pu_grid_sf[i, , drop = FALSE]))
    }) %>%
      do.call(what = bind_rows) %>%
      dplyr::select(-layer)
  )

# Create boundary matrix
bnd_mat <- boundary_matrix(pu_sf)

# Establish problem
p <- problem(x = pu_data_final, features = feature_data_w_constraints$name, 
             cost_column = names(cost)) %>%
  add_min_set_objective() %>%
  add_manual_targets(manual_targets) %>%
  add_binary_decisions() %>%
  add_boundary_penalties(penalty = 0.0001, data = bnd_mat) %>% 
  add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1, time_limit = 60*5)

# Solve
sp <- solve(p)

# Since the solution contains a combination of grid cell-level and
# seamount-level planning units, we need to do some post-processing
# to identify which "real" planning units (i.e. raster pixels) were selected
pu_ids <- unique(unlist(pu_data$id[which(sp$solution_1 > 0.5)]))

# We can now use these values to create a raster
solution <- pu_raster * 0
solution[pu_ids] <- 1
names(solution) <- "cost_option"

# Save solution for later
solution_stack <- stack(solution_stack, solution)

###
# Now run changing the cost option == 1 to cost option == 0
###

# Create dummy cost option (== 0) and add original cost option to features
features <- stack(salt_features, salt_pu)
names(features) <- c(names(salt_features), "cost_option")

cost <- salt_pu*0

# Create features targets
targets <- c(rep(0.1, nlayers(salt_features)), 0.0005) # 10% of features and 0.05% of the "cost"
seamounts_target <- 0.1 # 10% of all seamounts, using whole seamount features

# Create a separate layer for each seamount group 
seamount_groups <-
  seamount_raster %>%
  as("SpatRaster") %>%
  terra::patches(directions = 4) %>%
  as("Raster") 

# Create a stack with each seamount group as a layer
seamount_stack <- seamount_groups %>%
  {lapply(sort(unique(values(.))), function(i) {
    raster::Which(. == i)
  })} %>%
  raster::stack() %>%
  setNames(paste0("seamount_", seq_len(raster::nlayers(.))))

# Create grid-cell level planning unit data
# Initialize data
pu_grid_data <-
  ### add in indices for planning units in raster to be organized
  tibble(id = as.list(seq_len(ncell(pu_raster)))) %>%
  ### add in cost data 
  bind_cols(as_tibble(raster::as.data.frame(cost))) %>%
  ### add in feature data
  bind_cols(as_tibble(raster::as.data.frame(features))) %>%
  ### add in seamount data
  mutate(seamount = 0)

# Create seamount level planning unit data
pu_sm_data <- lapply(names(seamount_stack), function(i) {
  ## initialize data for planning unit that corresponds to i'th sea mount
  curr_sm_pu <-
    ## add in indices for planning units in raster to be organized
    tibble(
      id = list(raster::Which(seamount_stack[[i]] > 0.5, cells = TRUE))
    ) %>%
    ## add in cost data - modified for multiple cost inputs
    bind_cols(
      raster::as.data.frame(cost* seamount_stack[[i]]* 0) %>% 
        setNames(names(cost)) %>% 
        dplyr::summarize_all(sum, na.rm=TRUE)
    ) %>% 
    ## calculate total amount of each non-sea mount feature in i'th seamount pu
    bind_cols(
      raster::as.data.frame(features * seamount_stack[[i]]) %>%
        setNames(names(features)) %>%
        dplyr::summarize_all(sum, na.rm = TRUE)
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

# Merge seamount-level data and grid-cell level data together
pu_data <- bind_rows(pu_grid_data, pu_sm_data)

# calculate feature data
feature_data <-
  ## initialize data with the name and total amount of each feature
  ## note we use seamount_raster here because we want to set targets
  ## based on overall distribution of seamounts (not each one separately)
  raster::stack(features, setNames(seamount_raster, "seamount")) %>%
  raster::cellStats(sum, na.rm = TRUE) %>%
  {tibble(id = seq_along(.), name = names(.), total = unname(.))} %>%
  ## set targets
  mutate(rel_target = c(targets, seamounts_target)) %>%
  ## now compute the targets as absolute values
  ## (this is needed because we have spatial overlaps in the planning unit
  ## data, so if we gave prioritizr relative targets then it wouldn't
  ## calculate the percentages correctly)
  mutate(abs_target = rel_target * total)

# Add in constraints to ensure that the solution won't select
# spatially overlapping grid-cell level and seamount-level planning units
constraints <- data.frame(test  = rep(0,nrow(pu_data)))
index <- 1

# This makes a vector with a '1' for each combination of seamount level planning unit and     
# grid-cell level planning unit that overlaps with that seamount. 
for (i in seq_len(nrow(pu_sm_data))) {
  print(paste0("Processing seamount ", i, " of ", nrow(pu_sm_data)))
  
  for (j in pu_sm_data$id[[i]]) {
    
    ### specify planning unit indices for constraints
    v <- rep(0, nrow(pu_data))      # initialize with zeros
    v[nrow(pu_grid_data) + i] <- 1  # specify seamount-level planning unit
    v[j] <- 1                       # specify grid cell-level planning unit
    
    constraints <- cbind(constraints, v)
    index <- index+1
  }
}

# Remove dummy row
constraints <- constraints[, -1]

# Add the constraints as features to the planning data
pu_data_final <- constraints %>% 
  setNames(sprintf("constraint_%d", seq.int(1:ncol(.)))) %>% 
  bind_cols(pu_data, .)

# Add constraints features to feature data which contains target
feature_data_w_constraints <- tibble(id = seq(from = nrow(feature_data)+1, to = (nrow(feature_data) +     length(constraints)), by = 1), total = 2) %>% 
  mutate(name = sprintf("constraint_%d", id-nrow(feature_data)), .after = id) %>% 
  mutate(rel_target = NA_real_,
         abs_target = 1) %>% 
  bind_rows(feature_data, .)

# Create targets dataframe for manual targets
# These targets are absolute and the targets for the features are all >=, 
# i.e. have to meet or exceed the target, whereas the constrain targets are 
# all <= 1, which means you cannot select the same planning unit     
# at the grid cell level and seamount level planning grid, i.e. you can't select 
# both the seamount and then select grid-cells that overlap the seamount, 
# only one or the other.
manual_targets <- tibble(feature = feature_data_w_constraints$name,
                         type = "absolute",
                         sense = c(rep(">=", nrow(feature_data)), rep("<=", length(constraints))),
                         target = c(pull(feature_data, abs_target), rep(1, length(constraints))))

# Update manual targets so that the cost option is less than or equal to xx%
manual_targets <- manual_targets %>% 
  mutate(sense = ifelse(feature == "cost_option", "<=", sense))

# Create an sf object with all pu data
pu_grid_sf <- rasterToPolygons(pu_raster, na.rm=FALSE) %>% 
  st_as_sf() %>% 
  dplyr::select(-1)

# Get patches in the right order
pu_sf <-
  pu_grid_sf %>%
  bind_rows(
    lapply(pu_sm_data$id, function(i) {
      st_sf(layer = 1, geometry = st_union(pu_grid_sf[i, , drop = FALSE]))
    }) %>%
      do.call(what = bind_rows) %>%
      dplyr::select(-layer)
  )

# Create boundary matrix
bnd_mat <- boundary_matrix(pu_sf)

# Establish problem
p <- problem(x = pu_data_final, features = feature_data_w_constraints$name, 
             cost_column = names(cost)) %>%
  add_min_set_objective() %>%
  add_manual_targets(manual_targets) %>%
  add_binary_decisions() %>%
  add_boundary_penalties(penalty = 0.0001, data = bnd_mat) %>% 
  add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1, time_limit = 60*5)

# Solve
sp <- solve(p)

# Since the solution contains a combination of grid cell-level and
# seamount-level planning units, we need to do some post-processing
# to identify which "real" planning units (i.e. raster pixels) were selected
pu_ids <- unique(unlist(pu_data$id[which(sp$solution_1 > 0.5)]))

# We can now use these values to create a raster
solution <- pu_raster * 0
solution[pu_ids] <- 1
names(solution) <- "cost_option"

# Save solution for later
solution_stack <- stack(solution_stack, solution)

# Show solution
plot(solution, main = "Solution")

# Show % of features and seamounts in solution
cellStats(addLayer(features, seamount_raster)*solution, sum, na.rm = TRUE) %>%
  bind_cols(feature_data, .) %>% 
  rename(abs_held = ncol(.)) %>% 
  mutate(relative_held = sprintf("%0.2f", 100*abs_held/total)) %>% 
  relocate(rel_target, .before = relative_held) %>% 
  dplyr::select(-1)

###
# Now run again with different cost targets
###

# Create features targets
targets <- c(rep(0.1, nlayers(salt_features)), 0.1) # 10% of features and 10% of the "cost"
seamounts_target <- 0.1 # 10% of all seamounts, using whole seamount features

# Create a separate layer for each seamount group
seamount_groups <-
  seamount_raster %>%
  as("SpatRaster") %>%
  terra::patches(directions = 4) %>%
  as("Raster")

# Create a stack with each seamount group as a layer
seamount_stack <- seamount_groups %>%
  {lapply(sort(unique(values(.))), function(i) {
    raster::Which(. == i)
  })} %>%
  raster::stack() %>%
  setNames(paste0("seamount_", seq_len(raster::nlayers(.))))

# Create grid-cell level planning unit data
# Initialize data
pu_grid_data <-
  ### add in indices for planning units in raster to be organized
  tibble(id = as.list(seq_len(ncell(pu_raster)))) %>%
  ### add in cost data
  bind_cols(as_tibble(raster::as.data.frame(cost))) %>%
  ### add in feature data
  bind_cols(as_tibble(raster::as.data.frame(features))) %>%
  ### add in seamount data
  mutate(seamount = 0)

# Create seamount level planning unit data
pu_sm_data <- lapply(names(seamount_stack), function(i) {
  ## initialize data for planning unit that corresponds to i'th sea mount
  curr_sm_pu <-
    ## add in indices for planning units in raster to be organized
    tibble(
      id = list(raster::Which(seamount_stack[[i]] > 0.5, cells = TRUE))
    ) %>%
    ## add in cost data - modified for multiple cost inputs
    bind_cols(
      raster::as.data.frame(cost* seamount_stack[[i]]* 0) %>%
        setNames(names(cost)) %>%
        dplyr::summarize_all(sum, na.rm=TRUE)
    ) %>%
    ## calculate total amount of each non-sea mount feature in i'th seamount pu
    bind_cols(
      raster::as.data.frame(features * seamount_stack[[i]]) %>%
        setNames(names(features)) %>%
        dplyr::summarize_all(sum, na.rm = TRUE)
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

# Merge seamount-level data and grid-cell level data together
pu_data <- bind_rows(pu_grid_data, pu_sm_data)

# calculate feature data
feature_data <-
  ## initialize data with the name and total amount of each feature
  ## note we use seamount_raster here because we want to set targets
  ## based on overall distribution of seamounts (not each one separately)
  raster::stack(features, setNames(seamount_raster, "seamount")) %>%
  raster::cellStats(sum, na.rm = TRUE) %>%
  {tibble(id = seq_along(.), name = names(.), total = unname(.))} %>%
  ## set targets
  mutate(rel_target = c(targets, seamounts_target)) %>%
  ## now compute the targets as absolute values
  ## (this is needed because we have spatial overlaps in the planning unit
  ## data, so if we gave prioritizr relative targets then it wouldn't
  ## calculate the percentages correctly)
  mutate(abs_target = rel_target * total)

# Add in constraints to ensure that the solution won't select
# spatially overlapping grid-cell level and seamount-level planning units
constraints <- data.frame(test  = rep(0,nrow(pu_data)))
index <- 1

# This makes a vector with a '1' for each combination of seamount level planning unit and
# grid-cell level planning unit that overlaps with that seamount.
for (i in seq_len(nrow(pu_sm_data))) {
  print(paste0("Processing seamount ", i, " of ", nrow(pu_sm_data)))

  for (j in pu_sm_data$id[[i]]) {

    ### specify planning unit indices for constraints
    v <- rep(0, nrow(pu_data))      # initialize with zeros
    v[nrow(pu_grid_data) + i] <- 1  # specify seamount-level planning unit
    v[j] <- 1                       # specify grid cell-level planning unit

    constraints <- cbind(constraints, v)
    index <- index+1
  }
}

# Remove dummy row
constraints <- constraints[, -1]

# Add the constraints as features to the planning data
pu_data_final <- constraints %>%
  setNames(sprintf("constraint_%d", seq.int(1:ncol(.)))) %>%
  bind_cols(pu_data, .)

# Add constraints features to feature data which contains target
feature_data_w_constraints <- tibble(id = seq(from = nrow(feature_data)+1, to = (nrow(feature_data) +     length(constraints)), by = 1), total = 2) %>%
  mutate(name = sprintf("constraint_%d", id-nrow(feature_data)), .after = id) %>%
  mutate(rel_target = NA_real_,
         abs_target = 1) %>%
  bind_rows(feature_data, .)

# Create targets dataframe for manual targets
# These targets are absolute and the targets for the features are all >=,
# i.e. have to meet or exceed the target, whereas the constrain targets are
# all <= 1, which means you cannot select the same planning unit
# at the grid cell level and seamount level planning grid, i.e. you can't select
# both the seamount and then select grid-cells that overlap the seamount,
# only one or the other.
manual_targets <- tibble(feature = feature_data_w_constraints$name,
                         type = "absolute",
                         sense = c(rep(">=", nrow(feature_data)), rep("<=", length(constraints))),
                         target = c(pull(feature_data, abs_target), rep(1, length(constraints))))

# Update manual targets so that the cost option is less than or equal to xx%
manual_targets <- manual_targets %>%
  mutate(sense = ifelse(feature == "cost_option", "<=", sense))

# Create an sf object with all pu data
pu_grid_sf <- rasterToPolygons(pu_raster, na.rm=FALSE) %>%
  st_as_sf() %>%
  dplyr::select(-1)

# Get patches in the right order
pu_sf <-
  pu_grid_sf %>%
  bind_rows(
    lapply(pu_sm_data$id, function(i) {
      st_sf(layer = 1, geometry = st_union(pu_grid_sf[i, , drop = FALSE]))
    }) %>%
      do.call(what = bind_rows) %>%
      dplyr::select(-layer)
  )

# Create boundary matrix
bnd_mat <- boundary_matrix(pu_sf)

# Establish problem
p <- problem(x = pu_data_final, features = feature_data_w_constraints$name,
             cost_column = names(cost)) %>%
  add_min_set_objective() %>%
  add_manual_targets(manual_targets) %>%
  add_binary_decisions() %>%
  add_boundary_penalties(penalty = 0.0001, data = bnd_mat) %>%
  add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1, time_limit = 60*5)

# Solve
sp <- solve(p)

# Since the solution contains a combination of grid cell-level and
# seamount-level planning units, we need to do some post-processing
# to identify which "real" planning units (i.e. raster pixels) were selected
pu_ids <- unique(unlist(pu_data$id[which(sp$solution_1 > 0.5)]))

# We can now use these values to create a raster
solution <- pu_raster * 0
solution[pu_ids] <- 1
names(solution) <- "cost_option"

# Save solution for later
solution_stack <- stack(solution_stack, solution)
# This one does not come to an answer by the 5 minute time limit (takes forever)


# Show results
plot(subset(solution_stack, c(3, 1, 2, 5, 4)), 
     main = c("\n\nCost = 1\nManual Target <= 10% salt_pu\nBoundary Penalty = 0.0001", 
              "\n\nCost = 1\nManual Target <= 0.05% salt_pu\nBoundary Penalty = 0.0001",
              "\n\nCost = salt_pu\nBoundary Penalty = 0",
              "\n\nCost = 0\nManual Target <= 10% salt_pu\nBoundary Penalty = 0.0001\nDoes not converge within 5min", 
              "\n\nCost = 0\nManual Target <= 0.05% salt_pu\nBoundary Penalty = 0.0001"), 
     font= 2,  
     axes = FALSE, 
     box = FALSE,
     nc = 3)
