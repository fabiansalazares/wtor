#' Retrieve the list of available HS versions for the Quantitative Restrictions database.
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @examples get_qr_hs_versions()
#' @return A tibble containing the list of valid codes for available HS version.
#' @export
get_qr_hs_versions <- function(nocache=F) {
  cached_qr_hs_versions <-get_cached_object("qr_hs_versions")

  if(!is.null(cached_qr_hs_versions) & !nocache) {
    message("wtor: get_qr_hs_versions(): returning from cache.")
   return(cached_qr_hs_versions)
  }

  get_url <- "https://api.wto.org/qrs/hs-versions"

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
      stop("get_qr_hs_versions: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_qr_hs_versions: WTO API returned a ", response$status_code, " code")
  }


  qr_hs_versions_df <- httr::content(response, type = "text") |>
    jsonlite::fromJSON() |>
    dplyr::as_tibble()

  set_cached_object(key="timeseries_qr_hs_versions",
                    value= qr_hs_versions_df)

  return(
   qr_hs_versions_df
  )

}
