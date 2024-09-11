#' Retrieve metadata for the Timeseries API indicators.
#' @param code indicator code. Required.
#' @param reporting_economies Character string. A vector or a scalar containing the codes and/or names of the reporting economies.
#' @param partner_economies Character string. A vector or a scalar containing the codes and/or names of the partner economies. Not all indicators allow for this parameter.
#' @param product_classification_code Character string. Either TRUE or FALSE depending on whether to include or not subproducts and subsectors.
#' @param subproducts_subsectors Logical. Either TRUE or FALSE depending on whether to include or not subproducts and subsectors.
#' @param lang Integer. Set to 1 for English, 2 for French or 3 for Spanish
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @export
get_timeseries_metadata <- function(
    code,
    reporting_economies="all",
    partner_economies="all",
    product_classification_code="all", # can be all | default | the actual code
    subproducts_subsectors=TRUE,
    lang=1,
    nocache=F) {

  cached_timeseries_metadata <-get_cached_object("timeseries_timeseries_metadata")

  if(!is.null(cached_timeseries_metadata) & !nocache) {
    message("get_timeseries_metadata: returning from cache.")
    return(cached_timeseries_metadata)
  }

  subproducts_subsectors_string <- ifelse(subproducts_subsectors,
                                                 "true",
                                                 "false")

  get_url <- glue::glue('http://api.wto.org/timeseries/v1/metadata?i={code}&r={reporting_economies}&p={partner_economies}&pc={product_classification_code}&spc={subproducts_subsectors_string}&lang={lang}')

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
      stop("get_timeseries_metadata: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_timeseries_metadata: WTO API returned a ", response$status_code, " code")
  }

  timeseries_metadata_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        metadataCategoryCode = .x$metadataCategoryCode,
        indicatorCategoryCode = .x$indicatorCategoryCode,
        indicatorCode= .x$indicatorCode,
        reportingEconomyCode = .x$reportingEconomyCode,
        productOrSectorClassificationCode = .x$productOrSectorClassificationCode,
        productOrSectorCode= .x$productOrSectorCode,
        periodCode = .x$periodCode,
        frequencyCode = .x$frequencyCode,
        unitCode = .x$unitCode,
        value = .x$value
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key="timeseries_timeseries_metadata",
                    value= timeseries_metadata_df)

  return(
    timeseries_metadata_df
  )

}
