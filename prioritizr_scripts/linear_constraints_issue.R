library(prioritizr)
library(prioritizrdata)

data(salt_pu)
data(salt_features)

set.seed(500)

#baseline problem with 30% targets and boundary penalties
p <- problem(salt_pu, salt_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%
  add_binary_decisions() %>%
  #add_boundary_penalties(0.01) %>% 
  add_gurobi_solver(gap = 0, verbose = FALSE)

sp <- solve(p)

##############################################################
#second problem with max total area representation set at 31%

#create cost layer with each cell value set = 1. We will use this in the linear constraints to set a maximum area representation
uniform_cost <- reclassify(salt_pu, c(0, Inf, 1))

#set the max threshold for area representation in the solution as 31%
threshold_31 <- cellStats(uniform_cost, sum) * 0.31

p2 <- problem(salt_pu, salt_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%
  add_binary_decisions() %>%
  #add_boundary_penalties(0.01) %>% 
  add_linear_constraints(threshold = threshold_31,
                         sense = "<=", 
                         data = uniform_cost) %>% 
  add_gurobi_solver(gap = 0, verbose = FALSE)

sp2 <- solve(p2)

################################################
#plot all solutions and summaries
par(mfrow = c(1,2))
plot(sp)
plot(sp2)

#number of planning units in solution as % of total
eval_n_summary(p, sp)$cost/number_of_planning_units(p)
eval_n_summary(p2, sp2)$cost/number_of_planning_units(p2)

#costs of solutions
eval_cost_summary(p, sp)
eval_cost_summary(p2, sp2)

eval_feature_representation_summary(p, sp)
eval_feature_representation_summary(p2, sp2)

for (i in 1:nlayers(salt_features)) {
  par(mfrow = c(1,2))
  plot(salt_features[[i]]*sp)
  plot(salt_features[[i]]*sp2)
}

plot(stack(sp*salt_pu, sp2*salt_pu), zlim = c(0, 1.4), main = c("Baseline scenario", "Linear constraints scenario"))

####################################################
#re-run above using binary species distribution (i.e. 1 = species present)
# and targets 30% for first 3 features and 40% for last two
####################################################

#reclassify features with everything above 0.5 as 1 (i.e. species present), everything below as NA (species not present)
salt_features2 <- reclassify(salt_features, c(0, 0.6, NA, 0.6, 1, 1))

#set targets
targets <- c(0.3, 1, 1, 0.35, 0.36)

#baseline problem with 30% targets and boundary penalties
p3 <- problem(salt_pu, salt_features2) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets) %>%
  add_binary_decisions() %>%
  #add_boundary_penalties(0.01) %>% 
  add_gurobi_solver(gap = 0, verbose = FALSE)

sp3 <- solve(p3)

##############################################################
#second problem with max total area representation set at 31%

#create cost layer with each cell value set = 1. We will use this in the linear constraints to set a maximum area representation
uniform_cost <- reclassify(salt_pu, c(0, Inf, 1))

#set the max threshold for area representation in the solution as 31%
threshold_31 <- cellStats(uniform_cost, sum) * 0.31

p4 <- problem(salt_pu, salt_features2) %>%
  add_min_set_objective() %>%
  add_relative_targets(targets) %>%
  add_binary_decisions() %>%
  #add_boundary_penalties(0.01) %>% 
  add_linear_constraints(threshold = threshold_31,
                         sense = "<=", 
                         data = uniform_cost) %>% 
  add_gurobi_solver(gap = 0, verbose = FALSE)

sp4 <- solve(p4)

################################################
#plot all solutions and summaries
par(mfrow = c(1,2))
plot(sp3)
plot(sp4)

#number of planning units in solution as % of total
eval_n_summary(p3, sp3)$cost/number_of_planning_units(p3)
eval_n_summary(p4, sp4)$cost/number_of_planning_units(p4)

#costs of solutions
eval_cost_summary(p3, sp3)
eval_cost_summary(p4, sp4)

eval_feature_representation_summary(p3, sp3)
eval_feature_representation_summary(p4, sp4)

for (i in 1:nlayers(salt_features2)) {
  par(mfrow = c(1,2))
  plot(salt_features2[[i]]*sp3)
  plot(salt_features2[[i]]*sp4)
}

plot(stack(sp3*salt_pu, sp4*salt_pu), zlim = c(0, 1.4))