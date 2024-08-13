#' @export
get_reporting_economies <- function(
    ig="all",
    reg="all",
    gp="all",
    language="1",
    nocache=F) {

  cache_key <- paste0("timeseries_reporting_economies_", ig, "_", reg, "_", gp, "_", language)

  cached_reporting_economies <-get_cached_object(cache_key)

  if(!is.null(cached_reporting_economies) & !nocache) {
    message("get_reporting_economies: returning from cache.")
    return(cached_reporting_economies)
  }

  get_url <- glue::glue("http://api.wto.org/timeseries/v1/reporters?ig={ig}&reg={reg}&gp={gp}&lang={language}")

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
      stop("get_reporting_economies: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_reporting_economies: WTO API returned a ", response$status_code, " code")
  }

  reporting_economies_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        code = .x$code,
        iso3A = .x$iso3A,
        name= .x$name,
        displayOrder= .x$displayOrder
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key=cache_key,
                    value= reporting_economies_df)

  return(
    reporting_economies_df
  )

}
