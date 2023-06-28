library(mregions2)

#taking apart the mrp_get function

#get an EEZ
layer <- "eez"

cql_filter <- "territory1 = 'Azores'"

filter <- NULL
count <- NULL

download_path <- "../../Downloads/test.zip"

namespace <- subset(mrp_list$namespace, mrp_list$layer == layer)

url <- httr2::url_parse("https://geo.vliz.be/geoserver/ows")

url$query <- list(service = "wfs", version = "2.0.0", request = "GetFeature", 
                  typeName = glue::glue("{namespace}:{layer}"), cql_filter = cql_filter, 
                  filter = filter, count = count, outputFormat = "SHAPE-ZIP")
url <- httr2::url_build(url)
hash <- digest::digest(url, algo = "crc32")
hash <- glue::glue("{layer}-{hash}")


resp <- httr2::request(url) %>% httr2::req_user_agent(mregions2:::mr_user_agent) %>% 
  httr2::req_error(is_error = function(resp) FALSE) %>% 
  httr2::req_perform(path = download_path)

azores <- mrp_get(layer, cql_filter = cql_filter)

plot(azores[1], axes = TRUE)

#rotate function: https://r-spatial.github.io/sf/articles/sf3.html#affine-transformations
rot = function(a) matrix(c(cos(a), sin(a), -sin(a), cos(a)), 2, 2)

azores_rot <- sf::st_geometry(azores) * rot(pi/2)

plot(azores_rot, axes=TRUE)


azores_gml <- sf::st_read("../../Downloads/eez.gml")

azores_json <- sf::st_read("../../Downloads/eez.json")

azores_shp_direct <- sf::st_read("../../Downloads/eez_azores_direct/eez.shp")

#try using the 

download_path <- "../../Downloads/eez_inR.json"

url <- httr2::url_parse("https://geo.vliz.be/geoserver/ows")
url$query <- list(service = "wfs", version = "1.1.0", request = "GetFeature", 
                  typeName = glue::glue("{namespace}:{layer}"), cql_filter = cql_filter, 
                  filter = filter, count = count, outputFormat = "JSON")
url <- httr2::url_build(url)


resp <- httr2::request(url) %>% httr2::req_user_agent(mregions2:::mr_user_agent) %>% 
  httr2::req_error(is_error = function(resp) FALSE) %>% 
  httr2::req_perform(path = download_path)

azores_R_json <- sf::st_read("../../Downloads/eez_inR.json")

plot(azores_R_json[1])




download_path <- "../../Downloads/eez_inRv2.json"

url <- httr2::url_parse("https://geo.vliz.be/geoserver/ows")
url$query <- list(service = "wfs", version = "2.0.0", request = "GetFeature", 
                  typeName = glue::glue("{namespace}:{layer}"), cql_filter = cql_filter, 
                  filter = filter, count = count, outputFormat = "JSON")
url <- httr2::url_build(url)


resp <- httr2::request(url) %>% httr2::req_user_agent(mregions2:::mr_user_agent) %>% 
  httr2::req_error(is_error = function(resp) FALSE) %>% 
  httr2::req_perform(path = download_path)

azores_Rv2_json <- sf::st_read("../../Downloads/eez_inRv2.json")

plot(azores_Rv2_json[1])
azores_R_json <- sf::st_read("../../Downloads/eez_inR.json")

plot(azores_R_json[1])