#' @export
get_years <- function(
    language="1",
    nocache=F) {

  cached_years <-get_cached_object("timeseries_years")

  if(!is.null(cached_years) & !nocache) {
    message("get_years: returning from cache.")
   return(cached_years)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/years?lang={language}"
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
      stop("get_years: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_years: WTO API returned a ", response$status_code, " code")
  }

  years_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        year = .x$year,
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key="timeseries_years",
                    value= years_df)

  return(
   years_df
  )

}
