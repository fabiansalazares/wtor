#' Retrieve the list of available indicators for the WTO Timeseries API
#' @param name Character string. Indicator name or part of it.
#' @param i Character string. Either 'all' or a string to filter indicators' description for.
#' @param t Character string. Topics to filter for, separated by commas.
#' @param pc Character string. Product classifications: must be one of the following: 'all', 'none', or a list of comma separated codes.
#' @param tp Character string. Filter for indicators containing a trade partner parameter. Either 'all', 'yes' or 'no'.
#' @param frq Character string. Filter for the frequency available. Either 'all' or 'A', 'S', 'Q', 'M' (anually, semi-anually, quarterly or monthly).
#' @param lang Integer. Set to 1 for English, 2 for French or 3 for Spanish
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @return A tibble containing the available indicators with the specified filters.
#' @examples get_timeseries_available_indicators()
#' @export
get_timeseries_available_indicators <- function(
    i="all", # indicators - either all or filter for a given indicator
    name="", # indicator name or part of it
    t="all", # topics - separated by commas
    pc="all",
    tp="all",
    frq="all",
    lang="1",
    nocache=F
    ) {

  cached_timeseries_available_indicators <- get_cached_object("timeseries_available_indicators")

  if(!is.null(cached_timeseries_available_indicators) & !nocache) {
    message("Retrieving from cache")
    return(cached_timeseries_available_indicators)
  }

  get_url <- paste0(
      "https://api.wto.org/timeseries/v1/indicators?",
      "i=", i,
      "&t=", t,
      "&pc=", pc,
      "&tp=", tp,
      "&frq=", frq,
      "&lang=", lang
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
      stop("get_available_indicators: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    message("get_available_indicators: WTO API returned a ", response$status_code, " code")
    message("No indicators to return.")
    return(NULL)
  }

  indicators_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        code = .x$code,
        name= .x$name,
        categoryCode = .x$categoryCode,
        categoryLabel = .x$categoryLabel,
        subcategoryCode = .x$subcategoryCode,
        subcategoryLabel= .x$subcategoryLabel,
        unitCode = .x$unitCode,
        unitLabel = .x$unitCode,
        startYear = .x$endYear,
        frequencyCode =.x$frequencyCode,
        frequencyLabel = .x$frequencyLabel,
        numberReporters = .x$numberReporters,
        numberPartners = .x$numberPartners,
        productSectorClassificationCode = .x$productSectorClassificationCode,
        productSectorClassificationLabel = .x$productSectorClassificationLabel,
        hasMetadata = .x$hasMetadata,
        numberDecimals = .x$numberDecimals,
        numberDatapoints = .x$numberDatapoints,
        updateFrequency = .x$updateFrequency,
        description = .x$description,
        sortOrder = .x$sortOrder
      )
    }
  ) |>
    dplyr::bind_rows()

  indicators_df_date <-  response$headers$date |>
    stringr::str_extract("\\d{2} [aA-zZ]{3} \\d{4}")  |>
    as.Date("%D %b %Y")

  set_cached_object(key="timeseries_available_indicators",
                    value= indicators_df)

  return(
    indicators_df
  )

}
