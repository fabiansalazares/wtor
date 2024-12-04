#' Create a local `cachem` object to store the local cache, either in disk or memory. Path to cache dir can be set at environment variable WTOR_CACHE_DIR
#' @param type Character string. Choose between types of cache to be created. Either 'disk' or 'memory'. Default is 'disk'
create_cache <- function(type="disk") {
  cache_disk_dir <- Sys.getenv("WTO_R_CACHE_DIR")

  if(cache_disk_dir == ""){
    cache_disk_dir <- paste0(tools::R_user_dir("wtor", which="data"), "/cache")
  }

  if(type=="disk") {
    if(!fs::dir_exists(cache_disk_dir)) {
      fs::dir_create(cache_disk_dir)
    }

    return(
      cachem::cache_disk(
        dir = cache_disk_dir,
        max_age = 60*60*24*30, # in seconds, 30 days
        destroy_on_finalize = FALSE, # so that cached objects stayed between sessions
        warn_ref_objects = TRUE
      )
    )
  }

  if (type=="memory") {
    return(
      cachem::cache_mem(
        max_age = 60*60*24*30, # in seconds, 30 days
        destroy_on_finalize = FALSE, # so that cached objects stayed between sessions
        warn_ref_objects = TRUE
      )
    )
  }

  stop("wtor: create_cache(): type must be either 'disk' or 'memory'.")
}

#' Clean the local cache.
clean_cache <- function() {
  wtor_env$cache$destroy()
  wtor_env$cache <- create_cache()
}

#' Create an environment object in which to store the local cache object.
wtor_env <- new.env(parent = emptyenv())
wtor_env$cache <- create_cache()

.onLoad <- function(...) {

  if(get_api_key() == "") {

    cat("No API key found in environment variable WTO_R_API_KEY.\n
         If you don't hold a valid WTO API key, obtain a new one at https://apiportal.wto.org/.\n
         Once you've got it, run usethis::edit_r_environ() and set the value of WTO_R_API_KEY to your WTO API key.\n")
  }
}

#' Retrieve the WTO API key from environment variable WTO_R_API_KEY
#' @export
get_api_key <- function() {
  Sys.getenv("WTO_R_API_KEY")
}

#' Retrieve an object from local cache.
#' @param key Character string. Cached object key.
#' @export
get_cached_object <- function(key) {
  return(
    wtor_env$cache$get(key, missing = NULL)
  )
}

#' Store an object in local cache.
#' @param key Character string. Cached object key.
#' @param value Tibble. Cached object.
set_cached_object <- function(key, value) {
  wtor_env$cache$set(key, value)
}


