library(prioritizr)
library(prioritizrdata)

data(salt_pu)
data(salt_features)

set.seed(500)

#generic function for rescaling a raster between new_min and new_max
func_rescale <- function(x, new_min, new_max){
  rescaled_value <- ((x - cellStats(x, min, na.rm=TRUE)) * (new_max - new_min) / (cellStats(x, max, na.rm=TRUE) - cellStats(x, min, na.rm=TRUE))) + new_min
}


#setup cost stack with scaled options
costs <- stack(salt_pu)
costs <- salt_pu %>% 
  func_rescale(new_min = 0.01, new_max = 1000) %>% 
  addLayer(costs, .)

costs <- salt_pu %>% 
  func_rescale(new_min = 0.01, new_max = 100) %>% 
  addLayer(costs, .)

costs <- salt_pu %>% 
  func_rescale(new_min = 0.01, new_max = 10) %>% 
  addLayer(costs, .)

solutions <- stack()
problems <- list()

for (i in 1:nlayers(costs)) {
  p <- problem(costs[[i]], salt_features) %>%
    add_min_set_objective() %>%
    add_relative_targets(0.30) %>%
    add_binary_decisions() %>%
    add_boundary_penalties(0.001) %>% 
    add_gurobi_solver(gap = 0.1, verbose = FALSE)
  
  problems[[i]] <- p
  
  solutions <- addLayer(solutions, solve(p))
}

################################################

#plot costs on quantile scale
par(mfrow =c(1,nlayers(costs)))
for (i in 1:nlayers(costs)) {
  brks <- c(0, as.numeric(quantile(costs[[i]])))
  print(plot(costs[[i]], breaks = brks, lab.breaks = brks, col = hcl.colors(n = length(brks)-1, palette = "viridis")))
}

#plot all solutions
plot(solutions, main = c("Baseline scenario", "Rescaled cost (0.01 - 1000)", "Rescaled cost (0.01 - 100)", "Rescaled cost (0.01 - 10)"))

#number of planning units in solution as % of total
for (i in 1:nlayers(costs)) {
  print(eval_n_summary(problems[[i]], solutions[[i]])$cost/number_of_planning_units(problems[[i]]))
}

#costs of solutions as percent of total
for (i in 1:nlayers(costs)) {
  print(100*eval_cost_summary(problems[[i]], solutions[[i]])$cost/ cellStats(costs[[i]], sum, na.rm=TRUE))
}


