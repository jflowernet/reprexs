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

plot(stack(sp*salt_pu, sp2*salt_pu), zlim = c(0, 1.4))
