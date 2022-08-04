library(sf)

eu <- st_read("data/EU/NUTS2_RG_60M_2013_mortality.shp")

plot(eu["NUTS_ID"])

eu_shifted <- st_shift_longitude(eu)

#lines spreading across the new meridian
plot(eu_shifted["NUTS_ID"])

st_crs(eu)$proj4string

#set target crs which is WGS84 but centered on 180 degree longitude (Pacific centered) - change this to another centre if required
target_crs <- st_crs("+proj=longlat +datum=WGS84 +pm=180")

# define a long & slim polygon that overlaps the meridian line & set its CRS to match
# that of eu

# Centered in lon 180 - change to another value if required
new_long_center <- 180
offset <- 180 - new_long_center


polygon_overlap <- st_polygon(x = list(rbind(
  c(-0.0001 - offset, 90),
  c(0 - offset, 90),
  c(0 - offset, -90),
  c(-0.0001 - offset, -90),
  c(-0.0001 - offset, 90)
))) %>%
  st_sfc() %>%
  st_set_crs(4326)

# modify eu dataset to remove overlapping portions with eu's polygons
eu2 <- eu %>% st_difference(polygon_overlap) %>% 
  st_transform(crs = target_crs)

plot(eu2["NUTS_ID"])
