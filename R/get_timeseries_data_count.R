#' Retrieve the amount of data points to be retrieved, for any given query to the `get_timeseries_data()`.
#' @param code Character string. indicator code. Required.
#' @param reporting_economies Character string. A vector or a scalar containing the codes and/or names of the reporting economies.
#' @param partner_economies Character string. A vector or a scalar containing the codes and/or names of the partner economies. Not all indicators allow for this parameter.
#' @param time_period Character string. A string containing either "default", "all", or specific periods according to the format described in https://apiportal.wto.org/api-details#api=version1&operation=post-data
#' @param products_or_sectors Character string. A string containing either "default", "all", a specific product classification such as HS2, HS4, HS6, or a comma separated list of product codes belonging to AG,AGFOFI,MAIS,...
#' @param subproducts_subsectors Logical. Either TRUE or FALSE depending on whether to include or not subproducts and subsectors.
#' @param decimals Character string. Either "default" or a string containing the number of decimals that the output should contain.
#' @param heading_style Character string. Either "H" for human-readable headers and "M" for machine-readable codes.
#' @param meta Logical. TRUE to include metadata.
#' @param nocache Logical. TRUE to disable caching of results.
#' @return An integer that contains the number of data points.
#' @export
get_timeseries_data_count <- function(
    code, # i,
    reporting_economies, # r
    partner_economies=NULL, # p
    time_period="default", # ps
    products_or_sectors="default", # pc
    subproducts_subsectors=FALSE, # spc
    decimals="default", # dec number of decimals
    heading_style="M", # head heading style
    meta=FALSE, # include metadata,
    nocache=F
    ) {

  if(is.null(code)) {
    stop("wtor: get_timeseries_data_count(): 'code' argument is NULL")
  }

  reporting_economies_codes <- paste(check_reporting_economies(reporting_economies), collapse=",")
  if(is.null(reporting_economies_codes)) {
    stop("wtor: get_timeseries_data: reporting economies contain invalid codes or names. For a list of valid codes and names, execute wtor::get_reporting_economies()")
  }

  if (is.null(partner_economies)) {
    partner_economies_codes <- partner_economies
  } else {
    partner_economies_codes <- ifelse(partner_economies=="all", "all", paste(check_partner_economies(partner_economies), collapse=","))
  }

  cache_key <- tolower(
    paste0(
      "timeseriesdatacount_",
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
    digest::digest(algo="md5")

  include_sub_products_sectors_string <- ifelse(subproducts_subsectors,
                                                 "true",
                                                 "false")

  get_url <- glue::glue('http://api.wto.org/timeseries/v1/data_count?i={code}&r={reporting_economies_codes}&ps={time_period}&pc={products_or_sectors}&spc={subproducts_subsectors}')

  if(!is.null(partner_economies)) {
    get_url <- sprintf(paste0(get_url,"&p=%s"), partner_economies_codes)
  }

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
