#' Retrieve the list of partner economies and their codes.
#' @param ig Character string. Either 'all', 'group' (for groups of economies) or 'individual' (for individual economies).
#' @param reg Character string. Region to filter for. See `get_geographical_regions()`. Default is 'all'.
#' @param gp Character string. Group of economies to filter for. See `get_groups()`. Default is 'all'.
#' @param lang Integer. Set to 1 for English, 2 for French or 3 for Spanish
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @return A tibble containing the list of partner economies and their corresponding country codes.
#' @examples get_partner_economies()
#' @export
get_partner_economies <- function(
    ig="all",
    reg="all",
    gp="all",
    lang="1",
    nocache=F) {

  cache_key <- paste0("timeseries_partner_economies_", ig, "_", reg, "_", gp, "_", lang)

  cached_partner_economies <-get_cached_object(cache_key)

  if(!is.null(cached_partner_economies) & !nocache) {
    message("get_partner_economies: returning from cache.")
    return(cached_partner_economies)
  }

  get_url <- glue::glue("http://api.wto.org/timeseries/v1/partners?ig={ig}&reg={reg}&gp={gp}&lang={lang}")

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

  set_cached_object(key=cache_key,
                    value= partner_economies_df)

  return(
    partner_economies_df
  )

}
