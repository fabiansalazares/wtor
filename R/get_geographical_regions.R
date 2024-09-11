#' Retrieve the list of available geographical regions to be passed as reporting or partner economies parameters.
#' @param lang Integer. Set to 1 for English, 2 for French or 3 for Spanish
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @examples get_geographical_regions()
#' @return A tibble containing the list of geographical regions and their corresponding codes.
#' @export
get_geographical_regions <- function(
    lang="1",
    nocache=F) {

  cache_key <- paste0("timeseries_geographical_regions_", lang)

  cached_geographical_regions <-get_cached_object(cache_key)

  if(!is.null(cached_geographical_regions) & !nocache) {
    message("get_geographical_regions: returning from cache.")
    return(cached_geographical_regions)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/territory/regions?lang={lang}"
  )

  tryCatch(
    expr={
      response <- httr::GET(
        url = get_url,
        config = httr::add_headers(
          "Cache-Control"="no-cache",
          "Ocp-Apim-Subscription-Key"=get_api_key()
        )
      )
    },
    error = function(e) {
      stop("get_geographical_regions: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_geographical_regions: WTO API returned a ", response$status_code, " code")
  }


  geographical_regions_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        code = .x$code,
        name = .x$name,
        displayOrder = .x$displayOrder,
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key=cache_key,
                    value= geographical_regions_df)

  return(
    geographical_regions_df
  )

}
