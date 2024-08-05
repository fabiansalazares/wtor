#' @export
get_topics <- function(
    language="1",
    nocache=F) {

  cached_topics <-get_cached_object("timeseries_topics")

  if(!is.null(cached_topics) & !nocache) {
    message("get_topics: returning from cache.")
   return(cached_topics)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/topics?lang={language}"
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
      stop("get_topics: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_topics: WTO API returned a ", response$status_code, " code")
  }

  topics_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        id = .x$id,
        name = .x$name,
        sortOrder = .x$sortOrder
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key="timeseries_topics",
                    value= topics_df)

  return(
   topics_df
  )

}
