#' Auxiliary function to check that a vector of country names contains valid country codes in the list of reporting economies. Wraps `check_economies_names_codes()`.
#' @param .countries Vector of character strings containing the countries codes' to be checked.
check_reporting_economies <- function(.countries) {
  check_economies_names_codes(.countries = .countries, .economies = get_reporting_economies())
}

#' Auxiliary function to check that a vector of country names contains valid country codes in the list of partner economies. Wraps `check_economies_names_codes()`
#' @param .countries Vector of character strings containing the countries codes' to be checked.
check_partner_economies <- function(.countries) {
  check_economies_names_codes(
    .countries = .countries,
    .economies = get_partner_economies()
  )
}

#' Auxiliary function to check that a vector of country names contains valid country codes
#' @param .countries Vector of character strings containing the countries codes' to be checked.
#' @param .economies Vector of character strings containing the economies codes' to be checked.
check_economies_names_codes <- function(
    .countries,
    .economies
    ) {

  reporting_economies_df <- .economies

  if(!"name" %in% names(reporting_economies_df)) {
    stop("wtor: check_economies_names_codes: .economies does not have a 'name' column")
  }

  codes_to_return <- c()
  invalid_countries <- NULL

  # if all the reporting economies are country codes
  if(all(.countries %in% reporting_economies_df$code)) {
    return(.countries)
  }

  # if all the reporting economies are names of countries
  if(all(.countries %in% reporting_economies_df$name)) {
    return(
      reporting_economies_df |>
        dplyr::filter(any(stringr::str_detect(.countries, stringr::fixed(name)))) |>
        # dplyr::filter(name %in% .countries) |>
        _$code
    )
  }

  # all the countries that are not names, must be codes. Otherwise, print message and  return NULL
  .countries_not_names <- .countries[!.countries %in% reporting_economies_df$name]
  if(!all(.countries_not_names %in% reporting_economies_df$code)) {
    invalid_countries <- .countries_not_names[!.countries_not_names %in% reporting_economies_df$code]

    message("The following countries are neither valid country names nor valid ISO country codes.")
    lapply(
      X=invalid_countries,
      FUN=function(.x) {
        message(.x)
      }
    )
  } |> invisible()

  codes_to_return <- c(codes_to_return, .countries_not_names)

  # all the countries that are not codes, must be names. Otherwise, print message and  return NULL
  .countries_not_codes <- .countries[!.countries %in% reporting_economies_df$code]
  if(!all(.countries_not_names %in% reporting_economies_df$name)) {
    invalid_countries <- .countries_not_codes[!.countries_not_codes %in% reporting_economies_df$name]
  }

  codes_to_return <- c(
    codes_to_return,
    reporting_economies_df |> dplyr::filter(name %in% .countries_not_codes) |> _$code
    ) |> unique()

  # if there were any invalid codes, NULL is returned
  if(!is.null(invalid_countries) & !length(invalid_countries) == 0) {
    return(NULL)
  }

  return(codes_to_return)
}

#' Retrieve timeseries data.
#' @param code indicator code. Required.
#' @param reporting_economies A vector or a scalar containing the codes and/or names of the reporting economies.
#' @param partner_economies A vector or a scalar containing the codes and/or names of the partner economies. Not all indicators allow for this parameter.
#' @param time_period A string containing either "default", "all", or specific periods according to the format described in https://apiportal.wto.org/api-details#api=version1&operation=post-data
#' @param products_or_sectors A string containing either "default", "all", a specific product classification such as HS2, HS4, HS6, or a comma separated list of product codes belonging to AG,AGFOFI,MAIS,...
#' @param subproducts_subsectors Either TRUE or FALSE depending on whether to include or not subproducts and subsectors.
#' @param format_output Either "csv" or "json", depending on the output format in which to obtain the response to the POST request. It does not have any impact on the function returned dataframe.
#' @param mode_output Either "codes" (by default) or "full", depending on whether the columns in the returned dataframe will contain
#' @param decimals Either "default" or a string containing the number of decimals that the output should contain.
#' @param heading_style Either "H" for human-readable headers and "M" for machine-readable codes.
#' @param offset Number of datapoints to offset in the request. Usefull if manual pagination is to be implemented.
#' @param max_records Maximum number of rows to return. Default is NULL. If NULL, the maximum number of rows will equal the number of datapoints returned by get_timeseries_data_count()
#' @param heading_style Either "H" for human-readable headers and "M" for machine-readable codes.
#' @param meta TRUE to include metadata.
#' @param nocache TRUE to disable caching of results.
#' @param nopagination TRUE to disable pagination of requests.
#' @param pageitems Number of rows per paginated request. By default 10.000
#' @param request_max_attempts Maximum number of request attempts.
#' @return A tibble containing the request data.
#' @export
get_timeseries_data <- function(
    code, # i,
    reporting_economies, # r a vector containing the names of reporting economies
    partner_economies=NULL, # p
    time_period="default", # ps
    products_or_sectors="default", # pc
    subproducts_subsectors=FALSE, # spc
    format_output="csv", # output format: json or csv . if csv is chosen a compressed csv is returned
    mode_output="codes", # output mode
    decimals="default", # dec number of decimals
    heading_style="M", # head heading style
    offset=0, # off - records to offset
    max_records=NULL, # max maximum number of records returned heading_style="M",
    meta=FALSE, # include metadata,
    nocache=F,
    nopagination=F,
    pageitems = 10000,
    request_max_attempts = 10
    ) {

  # check that an indicator code has been passed as argument -----
  if(is.null(code)) {
    stop("wtor: get_timeseries: no code was passed as argument, but it is required.")
  }

  reporting_economies_codes <- ifelse(reporting_economies=="all", "all", check_reporting_economies(reporting_economies))
  if(is.null(reporting_economies_codes)) {
    stop("wtor: get_timeseries_data: reporting economies contain invalid codes or names. For a list of valid codes and names, execute wtor::get_reporting_economies()")
  }

  if (is.null(partner_economies)) {
    partner_economies_codes <- partner_economies
  } else {
    partner_economies_codes <- ifelse(partner_economies=="all", "all", check_partner_economies(partner_economies))
  }

  # generate a cache key and retrieve from cache if it does exist ----
  cache_key <- tolower(
    paste0(
      "timeseries_",
      code,
      "_",
      paste(reporting_economies_codes, collapse="_"),
      "_",
      paste(partner_economies_codes, collapse="_"),
      "_",
      time_period,
      "_",
      products_or_sectors |> tolower(),
      "_",
      subproducts_subsectors |> tolower(),
      "_"
    )
  ) |>
    digest::digest(algo="md5")


  message("cache_key: ", cache_key)

  # retrieve from cache if existing -----
  cached_timeseries <- get_cached_object(key=cache_key)

  if(!is.null(cached_timeseries) & !nocache) {
    message("Retrieving from local cache...")
    return(cached_timeseries)
  }


  # perform pagination, f allowed and needed
  # if pagination is not disabled, and the number of datapoints is greater than the maximum number of items per page,
  # the request will be split in n=datapoints/maxitems, where datapoints is the expected number of datapoints, and
  # maxitems is the maximum number allowed to each request. This parameter can be set as an argument
  datapoints <- get_timeseries_data_count(
    code,
    reporting_economies = reporting_economies,
    partner_economies = partner_economies,
    time_period=time_period,
    products_or_sectors = products_or_sectors,
    subproducts_subsectors= subproducts_subsectors
  ) |> _$n

  message("Datapoints to be retrieved: ", datapoints)

  # retrieve data from WTO API -----
  request_url <- "http://api.wto.org/timeseries/v1/data"

  # generate body of POST request - line by line of the JSON object -----
  ## indicator
  # indicator_line <- glue::glue('"i": "{code}"')
  indicator_line <- sprintf('"i": "%s"', code)

  ## reporting economies -----
  if(is.null(reporting_economies_codes)) {
    reporting_economies_line <- NULL
  } else {
    if(length(reporting_economies_codes) == 1) {
      reporting_economies_line <- sprintf('"r": "%s"', reporting_economies_codes)
    } else {
      reporting_economies_list <- paste(reporting_economies_codes, collapse=",")
      reporting_economies_line <- sprintf('"r": "%s"', reporting_economies_list)
    }
  }

  ## partner economies ----
  if(is.null(partner_economies_codes)) {
    partner_economies_line <- NULL
  } else {
    if(length(partner_economies_codes) == 1) {
      partner_economies_line <- glue::glue('"p": "{partner_economies_codes}"')
    } else {
      partner_economies_list <- paste(partner_economies_codes, collapse=",")
      partner_economies_line <- glue::glue('"p": "{partner_economies_list}"')
    }
  }

  ## time period -----
  time_period_line <- glue::glue('"ps": "{time_period}"')

  ## products or sectors
  if(is.null(products_or_sectors)) {
    products_or_sectors_line <- NULL
  } else {
    if(length(products_or_sectors) == 1) {
      products_or_sectors_line <- glue::glue('"pc": "{products_or_sectors}"')
    } else {
      products_or_sectors_list <- paste(partner_economies, collapse=",")
      products_or_sectors_line <- glue::glue('"pc": "{products_or_sectors_list}"')
    }
  }

  ## include subproducts or subsectors ------
  subproducts_or_subsectors <- ifelse(
    subproducts_subsectors,
    "true",
    "false"
  )
  subproducts_or_subsectors_line <- glue::glue('"spc": {subproducts_or_subsectors}')

  ## format ------
  if(!format_output %in% c("json", "csv")) {
    stop("wtor: get_timeseries_data: format_output must be either json or csv")
  }
  format_output_line <- glue::glue('"fmt": "{format_output}"')

  ## mode ------
  if(!mode_output %in% c("full", "codes")) {
    stop("wtor: get_timeseries_data: mode argument must be either 'full' or 'codes'.")
  }
  # mode_output_line <- glue::glue('"mode": "{mode_output}"')
  mode_output_line <- sprintf('"mode": "%s"', mode_output)

  ## decimals ------
  decimals_line <- as.character(decimals)
  # decimals_line <- glue::glue('"dec": "{decimals_line}"')
  decimals_line <- sprintf('"dec": "%s"', decimals_line)


  # by default, .max_records will be set to the number of expected datapoints ------
  .max_records <- datapoints

  # if argument max_records has been set, .max_records will be set to its value ------
  if(!is.null(max_records)) {
    .max_records <- max_records
  }

  # .max_records must not be greater than 1M in any case ------
  if(.max_records > 999999) {
    .max_records <- 999999
  }

  .max_records <- format(.max_records, scientific=FALSE)
  max_records_line <- sprintf('"max": %s', .max_records)

  ## head ------
  if(!heading_style %in% c("H", "M")) {
    stop("wtor: get_timeseries_data: heading_style must be either 'H' or 'M'")
  }
  heading_style_line <- glue::glue('"head": "{heading_style}"')

  ## lang
  lang_line <- glue::glue('"lang": 1')

  ## metadata to include ------
  meta <- ifelse(
    meta,
    "true",
    "false"
  )

  meta_line<- glue::glue('"meta": {meta}')

  lines_list <- c(
    indicator_line,
    reporting_economies_line,
    partner_economies_line,
    time_period_line,
    products_or_sectors_line,
    subproducts_or_subsectors_line,
    format_output_line,
    mode_output_line,
    decimals_line,
    max_records_line,
    heading_style_line,
    lang_line,
    meta_line
  )

  lines_list <- lines_list[!is.null(lines_list)]

  ## put together all the lines of POST request JSON object
  request_post_body <- paste(
    lines_list,
    collapse = ",\n"
  )


  # pagination -----
  if(nopagination) {
    offset_vector <- c(0)
    message("Pagination has been disabled.")
    if (datapoints > 1e6) {
      stop("wtor: get_timeseries_data(): Number of datapoints to retrieve is greather than 1M.")
    }
  } else {
    if(datapoints < pageitems) {
      offset_vector <- c(0)
    } else {
      # we calculate the number of items per request by dividing the total datapoints
      offset_vector <- seq(from=0, to = datapoints, by=pageitems)
      message("Requests: ", length(offset_vector))
      message("Items by request: ", pageitems)
    }
  }

  # post query -----
  timeseries_data_df <- lapply(
    X=offset_vector,
    FUN=function(.offset) {
      message("Offset: ", .offset, "\t", round(.offset/datapoints*1e2,1), "%")

      offset_line <- glue::glue('"off": {as.character(.offset)}')

      request_post_body <- glue::glue("{{\n{request_post_body},\n{offset_line}\n}}")

      request_completed <- FALSE
      request_attempts <- 0
      while(!request_completed & request_attempts < request_max_attempts) {
        request_attempts <- request_attempts + 1

        tryCatch(
          expr={
            response <- httr::POST(
              url=request_url,
              body=request_post_body,
              config = httr::add_headers(
                "Content-Type"= "application/json",
                "Cache-Control"="no-cache",
                "Ocp-Apim-Subscription-Key"=get_api_key()
              )
            )

            request_completed <- TRUE
          },
          error = function(e) {
            message("get_timeseries_data: httr::POST error: ", e)
            message("Attempting again ", request_attempts, "/", request_max_attempts)
          }
        )
      }

      if(!request_completed) {
        message("Request was not completed. Stopping with last status code")
        stop(response$status_code)
      }

      if(response$status_code != 200) {
        stop("wtor: get_timeseries_data: HTTP code returned: ", response$status_code, "\n", httr::content(response)$errors$SQL[1])
        # stop(response$status_code, "\n", httr::content(response)$errors$SQL[1])
      }

      if(format_output=="csv") {
        # zipped csv output
        # message("Unpacking zipped csv")
        # Creating a connection object using mode "wb
        tmp_file <- tempfile()
        con = file(tmp_file, "wb")
        writeBin(response$content, con)
        # Close the connection object
        close(con)

        .timeseries_data_df <- readr::read_csv(
          archive::archive_read(tmp_file, 1),
          col_types = readr::cols(
            readr::col_character(), # indicatorcategorycode
            readr::col_character(), # indicatorcode
            readr::col_factor(), # reportingeconomycode
            readr::col_factor(), # partnereconomycode
            readr::col_factor(), # productsectorclassificationcode
            readr::col_character(), # productsectorcode
            readr::col_factor(), # periodcode
            readr::col_factor(), # frequencycode
            readr::col_factor(), # unitcode
            readr::col_character(), # year
            readr::col_character(), # valueflagcode
            readr::col_double() # value
          )
        ) |>
          dplyr::as_tibble() |>
          dplyr::rename_all(tolower)

      } else if(format_output=="json") {
        # json output

        # message("Unpacking json")

        .timeseries_data_df <- jsonlite::fromJSON(httr::content(response, as="text")) |>
          _$Dataset |>
          dplyr::as_tibble() |>
          dplyr::rename_all(tolower)
      } else {
        stop("wtor: get_timeseries_data(): format_output must be either 'json' or 'csv'.")
      }

      return(.timeseries_data_df)

    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(
    key=cache_key,
    value= timeseries_data_df
    )

  return(timeseries_data_df)

}

