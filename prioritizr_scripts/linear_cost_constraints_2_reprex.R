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

bnd_mtrx <- boundary_matrix(salt_pu)

bnd_mtrx@x <- scales::rescale(bnd_mtrx@x, to = c(0.01, 100))


cost_new <- sampleRandom(sum(salt_features), ncell(salt_features[[1]])*.05, asRaster=TRUE)
cost_new[is.na(cost_new)] <- pu_raster[is.na(cost_new)]
plot(cost_new)

#define a baseline problem with no boundary penalty
p <- problem(cost_new, salt_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.1) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1, time_limit = 60*5)

sp <- solve(p)

plot(sp)

#total cost of the above solution
(sp_cost <- eval_cost_summary(p, sp)$cost)

#set cost thresholds
threshold <- sp_cost + (sp_cost * seq(1e-5, 0.5, length.out = 4))
threshold <- ceiling(threshold)

# print cost thresholds
print(threshold)

#problem with cost = 0
cost_0 <- reclassify(salt_pu, c(0, cellStats(salt_pu, max), 0))

p0 <- problem(cost_0, salt_features) %>%
  add_min_set_objective() %>%
  add_boundary_penalties(penalty = 1, data = bnd_mtrx) %>% 
  add_relative_targets(0.1) %>%
  add_binary_decisions()

hierarchical_results <- stack()
for (i in threshold) {
  s <- p0 %>%
    add_linear_constraints(threshold = i, sense = "<=", data = cost_new) %>%
    add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1, time_limit = 60*5) %>% 
    solve()
  
  names(s) <- paste0("threshold_", i)
  hierarchical_results <- addLayer(hierarchical_results, s)
}

# plot results
plot(hierarchical_results)

#total cost of each solution
hierarchical_results_costs <- cellStats(salt_pu*hierarchical_results, sum)

names(hierarchical_results_costs) <- names(hierarchical_results)

hierarchical_results_costs

#all are close to threshold, but this is expected since the function in minimizing boundary penalty, but keeping cost within linear constraint

###############################################################
#Run above with patches (seamounts)

# Create planning unit raster
pu_raster <- salt_pu
pu_raster[!is.na(pu_raster)] <- 1

# Create seamount data 
seamount_df<- as.data.frame(pu_raster, xy = T) %>% 
  filter(!is.na(salt_pu))

#create seamounts raster
set.seed(1234)
seamount_df <- seamount_df[sample(1:nrow(seamount_df), size = 3),]

seamount_raster <- seamount_df %>% 
  sf::st_as_sf(., coords = c("x","y"), crs = st_crs(pu_raster)) %>% 
  st_buffer(700) %>% 
  rasterize(pu_raster, field = 1) %>% 
  mask(pu_raster)

plot(sum(pu_raster, seamount_raster, na.rm = TRUE))

#create new cost to use as second constraint
cost_new <- sampleRandom(sum(salt_features), ncell(salt_features[[1]])*.05, asRaster=TRUE)
cost_new[is.na(cost_new)] <- pu_raster[is.na(cost_new)]
plot(cost_new)

names(cost_new) <- "cost_secondary"
costs <- stack(salt_pu, cost_new)
names(costs[[1]]) <- "cost"

# Create features targets
targets <- c(rep(0.1, nlayers(salt_features))) # 10% of features
seamounts_target <- 0.3 # 30% of all seamounts, using whole seamount features

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
  bind_cols(as_tibble(raster::as.data.frame(costs))) %>%
  ### add in feature data
  bind_cols(as_tibble(raster::as.data.frame(salt_features))) %>%
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
      raster::as.data.frame(costs* seamount_stack[[i]]) %>% 
        setNames(names(costs)) %>% 
        dplyr::summarize_all(sum, na.rm=TRUE)
    ) %>% 
    ## calculate total amount of each non-sea mount feature in i'th seamount pu
    bind_cols(
      raster::as.data.frame(salt_features * seamount_stack[[i]]) %>%
        setNames(names(salt_features)) %>%
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
  raster::stack(salt_features, setNames(seamount_raster, "seamount")) %>%
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

bnd_mat@x <- scales::rescale(bnd_mat@x, to = c(0.01, 100))

# Establish problem
p <- problem(x = pu_data_final, features = feature_data_w_constraints$name, 
             cost_column = "cost") %>%
  add_min_set_objective() %>%
  add_manual_targets(manual_targets) %>%
  add_binary_decisions() %>%
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
names(solution) <- "cost_salt_pu"

# Show solution
plot(solution, main = names(solution))
plot(rasterToPolygons(seamount_raster, dissolve = TRUE), add=TRUE)

#cost
total_cost <- cellStats(solution*salt_pu, sum)

#feature representation
cellStats(stack(salt_features, seamount_raster)*solution, sum) %>% 
  as_tibble() %>%
  rename(in_solution = 1) %>% 
  bind_cols(feature_data, .) %>% 
  mutate("% of features in solution" = 100*in_solution/total)

#cost without seamounts
zero_seamounts <- pu_raster
zero_seamounts[seamount_raster == 1] <- 0

#solution without seamounts is MUCH cheaper
cellStats(solution*zero_seamounts*salt_pu, sum)

################################################
#using seamounts and thresholds

#set cost thresholds
threshold <- total_cost + (total_cost * seq(1e-5, 0.3, length.out = 4))
threshold <- ceiling(threshold)

# print cost thresholds
print(threshold)

#problem with cost = 0
pu_data_final <- pu_data_final %>% 
  mutate(cost_0 = 0)

 manual_targets_w_thresholds <- manual_targets %>%
   add_row(feature = "cost",
           type = "absolute",
           sense = "<=",
           target = 1, 
           .before = 1) #set arbitrary target since this will be set using the thresholds in the loop

p0 <- problem(x = pu_data_final, features = c("cost",feature_data_w_constraints$name), 
             cost_column = "cost_0") %>%
  add_min_set_objective() %>%
  add_boundary_penalties(penalty = 1, data = bnd_mat) %>%
  add_binary_decisions() 


hierarchical_results_seamount <- stack()

for (i in threshold) {

  manual_targets_w_thresholds <- manual_targets_w_thresholds %>% 
    mutate(target = ifelse(feature == "cost", i, target))
  
  s <- p0 %>%
    add_manual_targets(manual_targets_w_thresholds) %>%
    #adding linear constraint does not work - "Error: argument to data correspond to fields with NA values in the planning unit data associated with x"
    #add_linear_constraints(threshold = i, sense = "<=", data = "cost") %>%
    add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1, time_limit = 60*5) %>% 
    solve()
  
  # Since the solution contains a combination of grid cell-level and
  # seamount-level planning units, we need to do some post-processing
  # to identify which "real" planning units (i.e. raster pixels) were selected
  pu_ids <- unique(unlist(pu_data$id[which(s$solution_1 > 0.5)]))
  
  # We can now use these values to create a raster
  solution <- pu_raster * 0
  solution[pu_ids] <- 1
  names(solution) <- paste0("threshold_", i)
  
  hierarchical_results_seamount <- addLayer(hierarchical_results_seamount, solution)
}

# plot results
plot(hierarchical_results_seamount)

cellStats(hierarchical_results_seamount*salt_pu, sum)

###################################################################
#MULTIPLE CONSTRAINTS

#using a secondary cost
plot(cost_new)

# Establish problem
p <- problem(x = pu_data_final, features = feature_data_w_constraints$name, 
             cost_column = "cost_secondary") %>%
  add_min_set_objective() %>%
  add_manual_targets(manual_targets) %>%
  add_binary_decisions() %>%
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
names(solution) <- "cost_secondary"

# Show solution
plot(solution, main = names(solution))
plot(rasterToPolygons(seamount_raster, dissolve = TRUE), add=TRUE)

#cost
total_cost_secondary <- cellStats(solution*cost_new, sum)

#set cost thresholds
threshold_secondary <- total_cost_secondary + (total_cost_secondary * seq(1e-5, 0.2, length.out = 4))
threshold_secondary <- ceiling(threshold_secondary)

# print cost thresholds
print(threshold_secondary)

#problem with cost = 0
manual_targets_w_thresholds_secondary <- manual_targets_w_thresholds %>%
  slice(-1) %>% 
  add_row(feature = "cost_secondary",
          type = "absolute",
          sense = "<=",
          target = 1, 
          .before = 1) #set arbitrary target since this will be set using the thresholds in the loop

p0 <- problem(x = pu_data_final, features = c("cost_secondary",feature_data_w_constraints$name), 
              cost_column = "cost_0") %>%
  add_min_set_objective() %>%
  add_boundary_penalties(penalty = 1, data = bnd_mat) %>%
  add_binary_decisions() 


hierarchical_results_seamount_new <- stack()

for (i in (threshold_secondary)) {
  
  manual_targets_w_thresholds_secondary <- manual_targets_w_thresholds_secondary %>% 
    mutate(target = ifelse(feature == "cost_secondary", i, target))
  
  s <- p0 %>%
    add_manual_targets(manual_targets_w_thresholds_secondary) %>%
    add_gurobi_solver(gap = 0.1, threads = parallel::detectCores()-1, time_limit = 60*5) %>% 
    solve()
  
  # Since the solution contains a combination of grid cell-level and
  # seamount-level planning units, we need to do some post-processing
  # to identify which "real" planning units (i.e. raster pixels) were selected
  pu_ids <- unique(unlist(pu_data$id[which(s$solution_1 > 0.5)]))
  
  # We can now use these values to create a raster
  solution <- pu_raster * 0
  solution[pu_ids] <- 1
  names(solution) <- paste0("threshold_", i)
  
  hierarchical_results_seamount_new <- addLayer(hierarchical_results_seamount_new, solution)
}

# plot results
plot(hierarchical_results_seamount_new)

cellStats(hierarchical_results_seamount_new*cost_new, sum)

100*cellStats(hierarchical_results_seamount_new*cost_new, sum)/cellStats(cost_new, sum)

100*cellStats(hierarchical_results_seamount*salt_pu, sum)/cellStats(salt_pu, sum)

#################################################
#MULTIPLE CONSTRAINTS

manual_targets_multi <- manual_targets %>%
  add_row(feature = "cost",
          type = "absolute",
          sense = "<=",
          target = threshold[4], 
          .before = 1) %>% 
  add_row(feature = "cost_secondary",
          type = "absolute",
          sense = "<=",
          target = threshold_secondary[4], 
          .after = 1) 

p_multi <- problem(x = pu_data_final, features = c(names(costs),feature_data_w_constraints$name), 
        cost_column = "cost_0") %>%
  add_min_set_objective() %>%
  add_boundary_penalties(penalty = 1, data = bnd_mat) %>%
  add_binary_decisions() %>% 
  add_manual_targets(manual_targets_multi) %>%
  add_gurobi_solver(gap = 0.3, threads = parallel::detectCores()-1, time_limit = 60*5)

sp_multi <- solve(p_multi)

pu_ids <- unique(unlist(pu_data$id[which(sp_multi$solution_1 > 0.5)]))

# We can now use these values to create a raster
solution <- pu_raster * 0
solution[pu_ids] <- 1
names(solution) <- "multi_threshold"

plot(solution)
plot(rasterToPolygons(seamount_raster, dissolve = TRUE), add=TRUE)

#costs
100*cellStats(solution*cost_new, sum)/cellStats(cost_new, sum)

100*cellStats(solution*salt_pu, sum)/cellStats(salt_pu, sum)

plot(stack(hierarchical_results_seamount[[1]], 
           hierarchical_results_seamount_new[[1]], solution))