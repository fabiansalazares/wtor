#' Retrieve the available data frequencies.
#' @param lang Integer. Set to 1 for English, 2 for French or 3 for Spanish
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @examples get_frequencies()
#' @export
get_frequencies <- function(
    lang="1",
    nocache=F) {

  cached_frequencies <-get_cached_object("timeseries_frequencies")

  if(!is.null(cached_frequencies) & !nocache) {
    message("get_frequencies: returning from cache.")
   return(cached_frequencies)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/frequencies?lang={lang}"
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
      stop("get_frequencies: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_frequencies: WTO API returned a ", response$status_code, " code")
  }

  frequencies_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        code = .x$code,
        name = .x$name
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key="timeseries_frequencies",
                    value= frequencies_df)

  return(
   frequencies_df
  )

}
