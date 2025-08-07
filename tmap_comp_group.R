library(tmap)
packageVersion("tmap")

pal = cols4all::c4a("hcl.blues3", n = 7)
palna = cols4all::c4a_na("hcl.blues3")

tm_shape(World) + 
  tm_polygons("HPI", fill.legend = tm_legend_hide()) +
  tm_add_legend(labels = c("10 to 15", "15 to 20", "20 to 25", "25 to 30"), fill = pal[1:4], type = "polygons", title = "HPI") +
  tm_add_legend(labels = c("30 to 35", "35 to 40", "40 to 45", "Missing"), fill = c(pal[5:7], palna), type = "polygons", title = " ", stack = "horizontal")

tm_shape(World) + 
  tm_polygons("HPI", fill.legend = tm_legend_hide()) +
  tm_add_legend(labels = c("10 to 15", "15 to 20", "20 to 25", "25 to 30"), fill = pal[1:4], type = "polygons", title = "HPI", group_id = "A") +
  tm_add_legend(labels = c("30 to 35", "35 to 40", "40 to 45", "Missing"), fill = c(pal[5:7], palna), type = "polygons", title = " ", group_id = "A") +
  tm_comp_group(group_id = "A", stack = "horizontal", position = tm_pos_out("left", "bottom"))

#remotes::install_version("tmap", version = "4.0")

library(tmap)
packageVersion("tmap")

pal = cols4all::c4a("hcl.blues3", n = 7)
palna = cols4all::c4a_na("hcl.blues3")

tm_shape(World) + 
  tm_polygons("HPI", fill.legend = tm_legend_hide()) +
  tm_add_legend(labels = c("10 to 15", "15 to 20", "20 to 25", "25 to 30"), fill = pal[1:4], type = "polygons", title = "HPI") +
  tm_add_legend(labels = c("30 to 35", "35 to 40", "40 to 45", "Missing"), fill = c(pal[5:7], palna), type = "polygons", title = " ", stack = "horizontal")

