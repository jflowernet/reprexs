#test version of mrp_get

mrp_get_test <- function (layer, path = getOption("mregions2.download_path", 
                                  tempdir()), cql_filter = NULL, filter = NULL, count = NULL) 
{
  checkmate::assert_character(layer, len = 1)
  checkmate::assert_choice(layer, mrp_list$layer)
  checkmate::assert_character(cql_filter, null.ok = TRUE, len = 1)
  checkmate::assert_character(filter, null.ok = TRUE, len = 1)
  assert_only_one_filter(cql_filter, filter)
  count <- checkmate::assert_integerish(count, lower = 1, len = 1, 
                                        coerce = TRUE, null.ok = TRUE)
  stopifnot(dir.exists(path))
  assert_internet()
  namespace <- subset(mrp_list$namespace, mrp_list$layer == 
                        layer)
  url <- httr2::url_parse("https://geo.vliz.be/geoserver/ows")
  url$query <- list(service = "wfs", version = "2.0.0", request = "GetFeature", 
                    typeName = glue::glue("{namespace}:{layer}"), cql_filter = cql_filter, 
                    filter = filter, count = count, outputFormat = "JSON")
  url <- httr2::url_build(url)
  hash <- digest::digest(url, algo = "crc32")
  hash <- glue::glue("{layer}-{hash}")
  cached_path <- file.path(path, glue::glue("{hash}.json"))
  do_request <- TRUE
  if (file.exists(cached_path)) {
    cached_file_time <- file.info(cached_path)$ctime
    cached_file_is_fresh <- difftime(Sys.time(), cached_file_time, 
                                     units = "weeks") %>% as.numeric() %>% <cache_max_time()
    if (cached_file_is_fresh) {
      do_request <- FALSE
      cli::cli_text("Cache is fresh. Reading: {.path {cached_file_path}}")
      cli::cli_text("(Last Modified: {.emph {cached_file_time}})")
    }
  }
  if (do_request) {
    resp <- httr2::request(url) %>% httr2::req_user_agent(mr_user_agent) %>% 
      httr2::req_error(is_error = function(resp) FALSE) %>% 
      httr2::req_perform(path = cached_zip_path) %>% mrp_get_sanity_check()

    if (!is_test()) 
      try_clean_up(cached_zip_path)
  }
  check_server_warning(cached_unzip_path)
  mrp_list <- NULL
  out <- sf::st_read(cached_file_path, quiet = TRUE, stringsAsFactors = FALSE)
  attr(out, "class") <- c("sf", "tbl_df", "tbl", "data.frame")
  out
}