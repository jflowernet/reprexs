library(prioritizr)
library(prioritizrdata)

data(salt_pu)
data(salt_features)

set.seed(500)

#generic function for rescaling a raster between new_min and new_max
func_rescale <- function(x, new_min, new_max){
  rescaled_value <- ((x - cellStats(x, min, na.rm=TRUE)) * (new_max - new_min) / (cellStats(x, max, na.rm=TRUE) - cellStats(x, min, na.rm=TRUE))) + new_min
}

#baseline problem with 30% targets 
p <- problem(salt_pu, salt_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%
  add_binary_decisions() %>%
  add_gurobi_solver(gap = 0.1, verbose = TRUE)

sp <- solve(p)

#####################################################
#second problem, same as above but boundary penalties added
p2 <- problem(salt_pu, salt_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%
  add_binary_decisions() %>%
  add_boundary_penalties(0.001) %>% 
  add_gurobi_solver(gap = 0.1, verbose = TRUE)

sp2 <- solve(p2)
##############################################################
#third problem, same as baseline problem but cost rescaled

#rescale cost layer between 0.01 and 100, setting min above zero to avoid feature over-representation
salt_pu_rescaled <- salt_pu %>% func_rescale(new_min = 0.01, new_max = 100)

p3 <- problem(salt_pu_rescaled, salt_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%
  add_binary_decisions() %>%
  #add_boundary_penalties(0.001) %>% 
  add_gurobi_solver(gap = 0.1, verbose = TRUE)

sp3 <- solve(p3)

##############################################################
#fourth problem, same as third problem but with boundary penalty added as in second problem

p4 <- problem(salt_pu_rescaled, salt_features) %>%
  add_min_set_objective() %>%
  add_relative_targets(0.30) %>%
  add_binary_decisions() %>%
  add_boundary_penalties(0.001) %>% 
  add_gurobi_solver(gap = 0.1, verbose = TRUE)

sp4 <- solve(p4)

################################################

#plot costs on quantile scale
par(mfrow =c(1,2))
brks <- c(0, as.numeric(quantile(salt_pu)))
plot(salt_pu, breaks = brks, lab.breaks = brks, col = hcl.colors(n = length(brks)-1, palette = "viridis"), main = c("Baseline scenario cost"))

brks <- c(0,as.numeric(quantile(salt_pu_rescaled)))
plot(salt_pu_rescaled, breaks = brks, lab.breaks = brks, col = hcl.colors(n = length(brks)-1, palette = "viridis"), main = c("Rescaled cost (0.01 - 100)"))

#show quantiles for each cost layer
quantile(salt_pu) 

quantile(salt_pu_rescaled)

#plot all solutions
plot(stack(sp, sp2, sp3, sp4), main = c("Baseline scenario", "Baseline scenario with bp", "Rescaled cost (0.01 - 100)", "Rescaled cost with bp"))

#number of planning units in solution as % of total
eval_n_summary(p, sp)$cost/number_of_planning_units(p)
eval_n_summary(p2, sp2)$cost/number_of_planning_units(p2)
eval_n_summary(p3, sp4)$cost/number_of_planning_units(p3)
eval_n_summary(p4, sp4)$cost/number_of_planning_units(p4)

#costs of solutions as percent of total
100*eval_cost_summary(p, sp)$cost/ cellStats(salt_pu, sum, na.rm=TRUE)
100*eval_cost_summary(p2, sp2)$cost/ cellStats(salt_pu, sum, na.rm=TRUE)
100*eval_cost_summary(p3, sp3)$cost/ cellStats(salt_pu_rescaled, sum, na.rm=TRUE)
100*eval_cost_summary(p4, sp4)$cost/ cellStats(salt_pu_rescaled, sum, na.rm=TRUE)

