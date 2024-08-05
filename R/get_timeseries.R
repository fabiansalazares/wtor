check_reporting_economies <- function(.countries) {
  check_economies_names_codes(.countries = .countries, .economies = get_reporting_economies())
}

check_partner_economies <- function(.countries) {
  check_economies_names_codes(.countries = .countries, .economies = get_partner_economies())
}

check_economies_names_codes <- function(.countries, .economies) {
  reporting_economies_df <- .economies

  codes_to_return <- c()
  invalid_countries <- NULL


  # if all the reporting economies are country codes
  if(all(.countries %in% reporting_economies_df$code)) {
    return(.countries)
  }

  # if all the reporting economies are names of countries
  if(all(.countries %in% reporting_economies_df$name)) {
    return(reporting_economies_df |> dplyr::filter(name %in% .countries) |> _$code)
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
#' @param code indicator code.
#' @param code indicator code.
#' @param code indicator code.
#' @export
get_timeseries_data <- function(
    code, # i,
    reporting_economies, # r a vector containing the names of reporting economies
    partner_economies=NULL, # p
    time_period="default", # ps
    products_or_sectors=NULL, # pc
    subproducts_subsectors=FALSE, # spc
    format_output="csv", # output format: json or csv . if csv is chosen a compressed csv is returned
    mode_output="codes", # output mode
    decimals="default", # dec number of decimals
    offset=0, # off - records to offset
    max_records=10000, # max maximum number of records returned heading_style="M",
    heading_style="M", # head heading style
    meta=FALSE, # include metadata,
    nocache=T
    ) {

  if(is.null(code)) {
    stop("wtor: get_timeseries: no code was passed as argument, but it is required.")
  }

  reporting_economies_codes <- check_reporting_economies(reporting_economies)
  if(is.null(reporting_economies_codes)) {
    stop("wtor: get_timeseries_data: reporting economies contain invalid codes or names. For a list of valid codes and names, execute wtor::get_reporting_economies()")
  }

  partner_economies_codes <- check_partner_economies(partner_economies)


  cache_key <- tolower(
    paste0(
      "timeseries_",
      code,
      "_",
      paste(reporting_economies_codes, collapse="_"),
      "_",
      paste(partner_economies, collapse="_"),
      time_period,
      "_",
      products_or_sectors,
      "_",
      subproducts_subsectors,
      "_",
      offset,
      "_",
      max_records
    )
  ) |>
    stringr::str_replace_all(" ", "_")

  message("cache_key: ", cache_key)

  # retrieve from cache if possible
  cached_timeseries <- get_cached_object(key=cache_key)

  if(!is.null(cached_timeseries) & !nocache) {
    message("Retrieving from local cache...")
    return(cached_timeseries)
  }

  # retrieve from WTO api
  request_url <- "http://api.wto.org/timeseries/v1/data"

  message(request_url)

  # indicator
  indicator_line <- glue::glue('"i": "{code}"')

  # reporting economies
  if(is.null(reporting_economies_codes)) {
    reporting_economies_line <- NULL
  } else {
    if(length(reporting_economies_codes) == 1) {
      reporting_economies_line <- glue::glue('"r": "{reporting_economies_codes}"')
    } else {
      reporting_economies_list <- paste(reporting_economies_codes, collapse=",")
      reporting_economies_line <- glue::glue('"r": "{reporting_economies_list}"')
    }
  }

  # partner economies
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

  # time period
  time_period_line <- glue::glue('"ps": "{time_period}"')

  # products or sectors
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

  # include subproducts or subsectors
  subproducts_or_subsectors <- ifelse(
    subproducts_subsectors,
    "true",
    "false"
  )
  subproducts_or_subsectors_line <- glue::glue('"spc": {subproducts_or_subsectors}')

  # format
  if(!format_output %in% c("json", "csv")) {
    stop("wtor: get_timeseries_data: format_output must be either json or csv")
  }
  format_output_line <- glue::glue('"fmt": "{format_output}"')

  # mode
  if(!mode_output %in% c("full", "codes")) {
    stop("wtor: get_timeseries_data: mode argument must be either 'full' or 'codes'.")
  }
  mode_output_line <- glue::glue('"mode": "{mode_output}"')

  # decimals
  decimals_line <- as.character(decimals)
  decimals_line <- glue::glue('"dec": "{decimals_line}"')

  # offset
  offset <- as.character(offset)
  offset_line <- glue::glue('"off": {offset}')

  max_records <- format(max_records, scientific=FALSE)
  max_records_line <- glue::glue('"max": {max_records}')

  # head
  if(!heading_style %in% c("H", "M")) {
    stop("wtor: get_timeseries_data: heading_style must be either 'H' or 'M'")
  }
  heading_style_line <- glue::glue('"head": "{heading_style}"')

  # lang
  lang_line <- glue::glue('"lang": 1')

  # metadata to include
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
    offset_line,
    max_records_line,
    heading_style_line,
    lang_line,
    meta_line
  )

  lines_list <- lines_list[!is.null(lines_list)]

  # put together the POST request
  request_post_body <- paste(
    lines_list,
    collapse = ",\n"
  )

  request_post_body <- glue::glue("{{\n{request_post_body}\n}}")


  message(request_post_body)

  # browser()

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

    },
    error = function(e) {
      stop("get_timeseries_data: httr::POST error: ", e)
    }
  )

  if(format_output=="csv") {
    # zipped csv output
    message("Unpacking zipped csv")
    # Creating a connection object using mode "wb
    tmp_file <- tempfile()
    con = file(tmp_file, "wb")
    writeBin(response$content, con)
    # Close the connection object
    close(con)

    timeseries_data_df <- readr::read_csv(archive::archive_read(tmp_file))

  } else if(format_output=="json") {
    # json output

    message("Unpacking json")

    timeseries_data_df <- jsonlite::fromJSON(httr::content(response, as="text")) |>
      _$Dataset |>
      dplyr::as_tibble() |>
      dplyr::rename_all(tolower)
  } else {
    stop("wtor: get_timeseries_data(): format_output must be either 'json' or 'csv'.")
  }

  timeseries_data_df <- timeseries_data_df |>
    dplyr::as_tibble() |>
    dplyr::rename_all(tolower)

  set_cached_object(key=cache_key,
                    value= timeseries_data_df)

  return(timeseries_data_df)

}

