#' Retrieve available periods for the WTO Timeseries API
#' @param lang Numeric. Set to 1 for English, 2 for French or 3 for Spanish
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @returns A tibble containing the available periods.
#' @examples get_periods()
#' @export
get_periods <- function(
    lang="1",
    nocache=F) {

  cached_periods <-get_cached_object("timeseries_periods")

  if(!is.null(cached_periods) & !nocache) {
    message("get_periods: returning from cache.")
   return(cached_periods)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/periods?lang={lang}"
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
      stop("get_periods: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_periods: WTO API returned a ", response$status_code, " code")
  }


  periods_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        code = .x$code,
        name = .x$name,
        description = .x$description,
        frequencyCode = .x$frequencyCode,
        displayOrder= .x$displayOrder
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key="timeseries_periods",
                    value= periods_df)

  return(
   periods_df
  )

}
