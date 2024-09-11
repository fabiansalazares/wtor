#' Retrieve quantitative restrictions by id
#' @param qr_id Id of the QR
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @examples get_qr_id(qr_id="")
#' @return A tibble containing the details of the quantitative restriction whose id matches the argument.
get_qr_id <- function(
    qr_id,
    nocache=F
    ) {

  cache_key <- paste0("timeseries_qr_id_", qr_id)
  cached_qr_id  <-get_cached_object(cache_key)

  if(!is.null(cached_qr_id) & !nocache) {
    message("wtor: get_qr_id(): returning from cache.")
    return(cached_qr_id)
  }

  get_url <- glue::glue("https://api.wto.org/qrs/qrs/{qr_id}")

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
      stop("get_qr_id: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("wtor: get_qr_id(): WTO API returned a ", response$status_code, " code with the following information", httr::content(response, type="text"))
  }

  .x <- httr::content(response)$data

  qr_id_df <- dplyr::tibble(
    id = .x$id,
    in_force_from  = .x$in_force_from,
    termination_dt = .x$termination_dt,
    reporter_member_code = .x$reporter_member$code,
    reporter_member_name_en = .x$reporter_member$name$en,
    reporter_member_name_es = .x$reporter_member$name$es,
    reporter_member_name_fr = .x$reporter_member$name$fr,
    general_description = .x$general_description,
    nation_legal_bases = .x$national_legal_bases,
    administrative_mechanisms = .x$administrative_mechanisms,
    restrictions = .x$restrictions,
    measures_flow = .x$measures$flow,
    measures_symbol= .x$measures$symbol,
    group_name = .x$measures$group_name,
    mast_codes = .x$measures$mast_codes[[1]],
    description_en = .x$measures$description$en,
    description_es = .x$measures$description$es,
    description_fr = .x$measures$description$fr,
    interpreted = .x$interpreted,
    affected_products_id = lapply(.x$affected_products, function(.y) {.y$id}),
    affected_products_code = lapply(.x$affected_products, function(.y) {.y$code}),
    affected_products_description_en = lapply(.x$affected_products, function(.y) {.y$description$en}),
    affected_products_description_es = lapply(.x$affected_products, function(.y) {.y$description$es}),
    affected_products_description_fr = lapply(.x$affected_products, function(.y) {.y$description$fr}),
    notified_in_qr_sn = .x$notified_in$qr_sn,
    notified_in_notification_dt = .x$notified_in$notification_dt,
    notified_in_document_symbol = .x$notified_in$document_symbol,
    notified_in_document_url = .x$notified_in$document_url,
    notified_in_original_language = .x$notified_in$original_language,
    notified_in_type= .x$notified_in$type,
    notified_in_covered_periods = .x$notified_in$covered_periods
  )

  set_cached_object(key=cache_key,
                    value= qr_id_df)

  return(
   qr_id_df
  )

}
