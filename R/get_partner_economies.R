#' @export
get_partner_economies <- function(
    ig="all",
    reg="all",
    gp="all",
    language="1",
    nocache=F) {

  cached_partner_economies <-get_cached_object("timeseries_partner_economies")

  if(!is.null(cached_partner_economies) & !nocache) {
    message("get_partner_economies: returning from cache.")
    return(cached_partner_economies)
  }

  get_url <- glue::glue("http://api.wto.org/timeseries/v1/partners?ig={ig}&reg={reg}&gp={gp}&lang={language}")

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
      stop("get_partner_economies: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_partner_economies: WTO API returned a ", response$status_code, " code")
  }

  partner_economies_df <- lapply(
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

  set_cached_object(key="timeseries_partner_economies",
                    value= partner_economies_df)

  return(
    partner_economies_df
  )

}
