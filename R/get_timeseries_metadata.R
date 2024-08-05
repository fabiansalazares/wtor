#' @export
get_timeseries_metadata <- function(
    code,
    reporting_economy="all",
    partner_economy="all",
    product_classification_code="all", # can be all | default | the actual code
    include_sub_products_sectors=TRUE,
    period="all",
    frequency="all",
    language=1,
    nocache=F) {

  cached_timeseries_metadata <-get_cached_object("timeseries_timeseries_metadata")

  if(!is.null(cached_timeseries_metadata) & !nocache) {
    message("get_timeseries_metadata: returning from cache.")
    return(cached_timeseries_metadata)
  }

  include_sub_products_sectors_string <- ifelse(include_sub_products_sectors,
                                                 "true",
                                                 "false")

  get_url <- glue::glue('http://api.wto.org/timeseries/v1/metadata?i={code}&r={reporting_economy}&p={partner_economy}&pc={product_classification_code}&spc={include_sub_products_sectors_string}&lang={language}')

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
