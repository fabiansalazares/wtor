#' @export
get_timeseries_data_count <- function(
    code,
    reporting_economy="all",
    partner_economy="default",
    period="default", # ps - time period
    products_or_sectors="default", # can be all | default | the actual code
    include_sub_products_sectors=FALSE,
    frequency="all",
    nocache=F) {

  cached_timeseries_data_count <- get_cached_object("timeseries_timeseries_data_count")

  if(!is.null(cached_timeseries_data_count) & !nocache) {
    message("get_timeseries_data_count(): returning from cache.")
    return(cached_timeseries_data_count)
  }

  include_sub_products_sectors_string <- ifelse(include_sub_products_sectors,
                                                 "true",
                                                 "false")


  get_url <- glue::glue('http://api.wto.org/timeseries/v1/data_count?i={code}&r={reporting_economy}&p={partner_economy}&ps={period}&pc={products_or_sectors}&spc={include_sub_products_sectors_string}')

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
      stop("wtor: get_timeseries_data_count(): httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
      print(httr::content(response)$errors)
    stop("wtor: get_timeseries_data_count(): WTO API returned a ", response$status_code, " code")
  }


  timeseries_data_count_df <- dplyr::tibble(
    n=as.integer( httr::content(response))
  )

  set_cached_object(key="timeseries_timeseries_data_count",
                    value= timeseries_data_count_df)

  return(
    timeseries_data_count_df
  )

}
