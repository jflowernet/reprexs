#reprex for tmap issue with maps not aligning in grid

library(tmap)

data(World)

#a world map
world_map <- tm_shape(World) +
  tm_borders()

#africa only
africa <- World |>
  dplyr::filter(continent == "Africa")

africa_map <- tm_shape(africa) +
  tm_fill(col = "income_grp", legend.show = FALSE)

#legend for Africa map
africa_map_legend <- tm_shape(africa) +
  tm_fill(col = "income_grp") +
  tm_layout(legend.only = TRUE)


#1: arrange maps and legend using grid package
library(grid)

grid.newpage()

pushViewport(viewport(layout = grid.layout(nrow = 2, ncol = 2)))

print(world_map, vp = viewport(layout.pos.row = 1, layout.pos.col = 1))
#NOTE: just = arguments do not seem to change the layout. Including to illustrate how the alignment should be
print(africa_map_legend, vp = viewport(layout.pos.row = 2, layout.pos.col = 1, just = "right"))
print(africa_map, vp = viewport(layout.pos.col = 2, just = "top"))

#2: arrange maps and legend using gridExtra
library(gridExtra)

africa_map_grob <- tmap_grob(africa_map)
africa_legend_grob <- tmap_grob(africa_map_legend)
world_map_grob <- tmap_grob(world_map)

grid.arrange(arrangeGrob(world_map_grob, africa_legend_grob, ncol = 1), africa_map_grob, ncol = 2)
#another method:
grid.arrange(world_map_grob, africa_legend_grob, africa_map_grob, layout_matrix = cbind(c(1,2), c(3,3)))

#3: arrange maps and legend using cowplot package
library(cowplot)

p1 <- plot_grid(world_map_grob, africa_legend_grob, ncol = 1)

plot_grid(p1, africa_map_grob, ncol = 2)
