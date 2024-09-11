#' Retrieve a list of categories of available Timeseries API indicators.
#' @param lang Numeric. Set to 1 for English, 2 for French or 3 for Spanish
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @examples get_indicator_categories()
#' @return A tibble containing the codes of indicator categories and their description.
#' @export
get_indicator_categories <- function(
    lang="1",
    nocache=F) {

  cached_indicator_categories <-get_cached_object("timeseries_indicator_categories")

  if(!is.null(cached_indicator_categories) & !nocache) {
    message("get_indicator_categories: returning from cache.")
   return(cached_indicator_categories)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/indicator_categories?lang={lang}"
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
      stop("get_indicator_categories: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_indicator_categories: WTO API returned a ", response$status_code, " code")
  }

  indicator_categories_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        code = .x$code,
        name = .x$name,
        parentCode = .x$parentCode,
        sortOrder = .x$sortOrder
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key="timeseries_indicator_categories",
                    value= indicator_categories_df)

  return(
   indicator_categories_df
  )

}
