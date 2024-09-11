#' Retrive a list of valid codes for value flags. Value flags are predefined metadata for values, such as estimation, ad valorem, forecasted values...
#' @param lang Numeric. Set to 1 for English, 2 for French or 3 for Spanish
#' @param pc Character string. Product classifications: must be one of the following: 'all', 'none', or a list of comma separated codes.
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @export
get_value_flags <- function(
    lang="1",
    pc="all",
    nocache=F) {

  cached_value_flags <-get_cached_object("timeseries_value_flags")

  if(!is.null(cached_value_flags) & !nocache) {
    message("get_value_flags: returning from cache.")
   return(cached_value_flags)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/value_flags?lang={lang}"
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
      stop("get_value_flags: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_value_flags: WTO API returned a ", response$status_code, " code")
  }

  value_flags_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        code = .x$code,
        description = .x$description
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key="timeseries_value_flags",
                    value= value_flags_df)

  return(
   value_flags_df
  )

}
