#' @export
get_timeseries_data_count <- function(
    code,
    reporting_economies="all",
    partner_economies="default",
    time_period="default", # ps - time period
    products_or_sectors="default", # can be all | default | the actual code
    subproducts_subsectors=FALSE,
    frequency_output="all",
    nocache=F) {

  if(is.null(code)) {
    stop("wtor: get_timeseries_data_count(): 'code' argument is NULL")
  }

  reporting_economies_codes <- check_reporting_economies(reporting_economies)
  if(is.null(reporting_economies_codes)) {
    stop("wtor: get_timeseries_data: reporting economies contain invalid codes or names. For a list of valid codes and names, execute wtor::get_reporting_economies()")
  }
  reporting_economies_codes <- paste(check_reporting_economies(reporting_economies), collapse=",")
  partner_economies_codes <- paste(check_partner_economies(partner_economies), collapse=",")

  cache_key <- tolower(
    paste0(
      "timeseries_",
      code,
      "_",
      stringr::str_replace_all(reporting_economies_codes, ",", "_"),
      "_",
      stringr::str_replace_all(partner_economies_codes, ",", "_"),
      time_period,
      "_",
      products_or_sectors,
      "_",
      subproducts_subsectors
    )
  ) |>
    stringr::str_replace_all(" ", "_")


  cached_timeseries_data_count <- get_cached_object(cache_key)

  if(!is.null(cached_timeseries_data_count) & !nocache) {
    message("get_timeseries_data_count(): returning from cache.")
    return(cached_timeseries_data_count)
  }



  include_sub_products_sectors_string <- ifelse(subproducts_subsectors,
                                                 "true",
                                                 "false")

  get_url <- glue::glue('http://api.wto.org/timeseries/v1/data_count?i={code}&r={reporting_economies_codes}&p={partner_economies_codes}&ps={time_period}&pc={products_or_sectors}&spc={subproducts_subsectors}')

  message("get_url: ", get_url)

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
    stop("wtor: get_timeseries_data_count(): WTO API returned a ", response$status_code, " code: ", httr::content(response)$errors)
  }


  timeseries_data_count_df <- dplyr::tibble(
    n=as.integer( httr::content(response))
  )

  set_cached_object(key=cache_key,
                    value= timeseries_data_count_df)

  return(
    timeseries_data_count_df
  )

}
