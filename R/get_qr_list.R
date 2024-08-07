#' Retrieve a list of quantitative restrictions
#' @param page Page number
#' @param reporter_member_code Country code of the reporting member
#' @param in_force_only TRUE to restrict to quantitative restrictions currently in force
#' @param year_of_entry_into_force Year in which the quantitative restrictions went into force
#' @param product_codes A vector or a scalar containing the product code(s) that partially or fully match the codes in any HS version
#' @param product_ids A vector or a scalar containing the product of product ids in format such as hx-yyyyyy, where x is the HS version and yyyyyy the product id.
#' @param nocache TRUE to disable retrieval of cached results
#' @examples get_list()
#' @return A tibble containing the list of matching quantitative restrictions.
get_qr_list <- function(
    page=NULL,
    reporter_member_code=NULL,
    in_force_only=NULL,
    year_of_entry_into_force=NULL,
    product_codes=NULL,
    product_ids=NULL,
    nocache=F
    ) {

  cache_key <- paste("qr_list",
                     page,
                     reporter_member_code,
                     in_force_only,
                     year_of_entry_into_force,
                     product_codes,
                     product_ids,
                     nocache,
                     collapse="_") |>
    tolower() |>
    stringr::str_replace_all(" ", "_")


  cached_qr_list  <- get_cached_object(cache_key)

  if(!is.null(cached_qr_list) & !nocache) {
    message("wtor: get_qr_list(): returning from cache.")
    return(cached_qr_list)
  }

  get_url <- "https://api.wto.org/qrs/qrs?"

  if(!is.null(page)) {
    get_url <- glue::glue("{get_url}page={page}")
  }

  if(!is.null(reporter_member_code)) {
    get_url <- glue::glue("{get_url}reporter_member_code={reporter_member_code}&")
  }

  if(!is.null(in_force_only)) {
    get_url <- glue::glue("{get_url}in_force_only={in_force_only}&")
  }

  if(!is.null(year_of_entry_into_force)) {
    get_url <- glue::glue("{get_url}year_of_entry_into_force={year_of_entry_into_force}&")
  }

  if(!is.null(product_codes)) {
    get_url <- glue::glue("{get_url}product_codes={product_codes}&")
  }

  if(!is.null(product_ids)) {
    get_url <- glue::glue("{get_url}product_ids={product_ids}&")
  }

  message("Url: ", get_url)

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
      stop("get_qr_list: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("wtor: get_qr_list(): WTO API returned a ", response$status_code, " code with the following information", httr::content(response, type="text"))
  }

  qr_list_df <- lapply(
    X=httr::content(response)$data,
    FUN=function(.x) {
      id <- .x$id
      # if(id == "9025") { browser()}
      in_force_from <- .x$in_force_from
      termination_dt <- .x$termination_dt
      reporter_member_code <- .x$reporter_member$code
      reporter_member_name_en <- .x$reporter_member$name$en
      reporter_member_name_es <- .x$reporter_member$name$es
      reporter_member_name_fr <- .x$reporter_member$name$fr
      general_description <- .x$general_description
      national_legal_bases <- .x$national_legal_bases
      administrative_mechanisms <- list(.x$administrative_mechanisms)
      restrictions <- list(.x$restrictions)
      responsible_entities <- .x$responsible_entities # empty list
      notified_in_qr_sn <- lapply(.x$notified_in, function(.y) {.y$qr_sn})
      notified_in_notification_dt <- lapply(.x$notified_in, function(.y) {.y$notification_dt})
      notified_in_document_symbol <- lapply(.x$notified_in, function(.y) {.y$notification_symbol})
      notified_in_document_url<- lapply(.x$notified_in, function(.y) {.y$document_url})
      notified_in_original_language <- lapply(.x$notified_in, function(.y) {.y$original_language})
      notified_in_type <- lapply(.x$notified_in, function(.y) {.y$type})
      notified_covered_periods <- lapply(.x$notified_in, function(.y) {.y$covered_periods})
      details <- .x$details

      dplyr::tibble(
        id = .x$id,
        in_force_from = as.Date(ifelse(is.null(in_force_from), NA, in_force_from)),
        termination_dt = as.character(ifelse(length(termination_dt)==0 | is.null(termination_dt), NA, termination_dt)),
        reporter_member_code = ifelse(is.null(reporter_member_code), NA, reporter_member_code),
        reporter_member_name_en = .x$reporter_member$name$en,
        reporter_member_name_es = .x$reporter_member$name$es,
        reporter_member_name_fr = .x$reporter_member$name$fr,
        general_description= ifelse(is.null(general_description), NA, general_description),
        national_legal_bases= ifelse(is.null(national_legal_bases), NA, national_legal_bases),
        administrative_mechanisms = ifelse(is.null(administrative_mechanisms), NA, administrative_mechanisms),
        restrictions = ifelse(is.null(restrictions), NA, restrictions),
        measures = list(
          lapply(
          X=.x$measures,
          function(.y) {
           if("description" %in% names(.y))  {
              flow <-.y$flow
              symbol <- .y$symbol
              group_namel <- .y$group_name
              mast_codes <- list(.y$mast_codes)
              description <- .y$description
              interpreted <- .y$interpreted

            return(
              dplyr::tibble(
                flow=.y$flow,
                symbol=.y$symbol,
                group_namel=.y$group_name,
                mast_codes=list(.y$mast_codes),
                description = ifelse(is.null(description), NA,description),
                interpreted = ifelse(is.null(interpreted), NA,interpreted)
              )
            )
           }

          if(all(c("description_en", "description_es", "description_fr") %in% names(.y))) {
              flow <-.y$flow
              symbol <- .y$symbol
              group_namel <- .y$group_name
              mast_codes <- list(.y$mast_codes)
              description_en <- .y$description$en
              description_es <- .y$description$es
              description_fr <- .y$description$fr
              interpreted <- .y$interpreted

            return(
              dplyr::tibble(
                flow=.y$flow,
                symbol=.y$symbol,
                group_namel=.y$group_name,
                mast_codes=list(.y$mast_codes),
                description_en = ifelse(is.null(description_en), NA,description_en),
                description_es = ifelse(is.null(description_es), NA,description_es),
                description_fr = ifelse(is.null(description_fr), NA,description_fr),
                interpreted = ifelse(is.null(interpreted), NA,interpreted)
              )
            )
          }

          stop("wtor: get_qr_list(): no description found on element with id: ", .x$id)

          }) |>
            dplyr::bind_rows()
        ),

        responsible_entities = ifelse(length(responsible_entities)==0, NA, responsible_entities),
        notified_in_qr_sn = lapply(.x$notified_in, function(.y) {.y$qr_sn}),
        notified_in_notification_dt = lapply(.x$notified_in, function(.y) {.y$notification_dt}),
        notified_in_document_symbol = lapply(.x$notified_in, function(.y) {.y$notification_symbol}),
        notified_in_document_url= lapply(.x$notified_in, function(.y) {.y$document_url}),
        notified_in_original_language = lapply(.x$notified_in, function(.y) {.y$original_language}),
        notified_in_type = lapply(.x$notified_in, function(.y) {.y$type}),
        notified_covered_periods = lapply(.x$notified_in, function(.y) {.y$covered_periods}),
        details = ifelse(is.null(details), NA, details)
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key=cache_key,
                    value=qr_list_df)

  return(
   qr_list_df
  )

}
