#' @export
get_groups <- function(
    language="1",
    nocache=F) {

  cache_key <- paste0("timeseries_groups_", language)

  cached_groups <-get_cached_object(cache_key)

  if(!is.null(cached_groups) & !nocache) {
    message("get_groups: returning from cache.")
   return(cached_groups)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/territory/groups?lang={language}"
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
