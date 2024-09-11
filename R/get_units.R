#' Retrieve a list of valid for the units in which the data may be retrieved.
#' @param lang Numeric. Set to 1 for English, 2 for French or 3 for Spanish
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @export
get_units <- function(
    lang="1",
    nocache=F) {

  cached_units <-get_cached_object("timeseries_units")

  if(!is.null(cached_units) & !nocache) {
    message("get_units: returning from cache.")
   return(cached_units)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/units?lang={lang}"
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
      stop("get_units: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_units: WTO API returned a ", response$status_code, " code")
  }

  units_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        code = .x$code,
        name = .x$name,
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key="timeseries_units",
                    value= units_df)

  return(
   units_df
  )

}
