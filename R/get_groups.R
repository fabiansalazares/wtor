#' Retrieve the list of available groups of economies to be passed as reporting or partner economies parameters.
#' @param lang Numeric. Set to 1 for English, 2 for French or 3 for Spanish
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @examples get_groups()
#' @return A tibble containing the list of available groups of economies and their corresponding codes.
#' @export
get_groups <- function(
    lang="1",
    nocache=F) {

  cache_key <- paste0("timeseries_groups_", lang)

  cached_groups <-get_cached_object(cache_key)

  if(!is.null(cached_groups) & !nocache) {
    message("get_groups: returning from cache.")
   return(cached_groups)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/territory/groups?lang={lang}"
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
      stop("get_groups: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_groups: WTO API returned a ", response$status_code, " code")
  }


  groups_df <- lapply(
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
                    value= groups_df)

  return(
   groups_df
  )

}
