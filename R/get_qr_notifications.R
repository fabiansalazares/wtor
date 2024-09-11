#' Retrieve a the notified quantitative restrictions for a given year and current reporting member, from the Quantitative Restrictions API
#' @param page Page to be retrieved
#' @param reporter_member_code Member code
#' @param notification_year Year of the notification
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @examples get_qr_notifications(reporter_member_code="C554", notification_year="2022")
get_qr_notifications <- function(
    page=NULL,
    reporter_member_code=NULL,
    notification_year=NULL,
    nocache=T) {

  cached_qr_notifications <-get_cached_object("timeseries_qr_notifications")

  if(!is.null(cached_qr_notifications) & !nocache) {
    message("wtor: get_qr_notifications(): returning from cache.")
    return(cached_qr_notifications)
  }

  get_url <- "https://api.wto.org/qrs/notifications?"

  if(!is.null(notification_year) & length(notification_year) == 1) {
    get_url <- paste0(get_url,glue::glue("notification_year={notification_year}"))
  }

  if (!is.null(reporter_member_code) & length(reporter_member_code) == 1) {
    get_url <- paste0(get_url,glue::glue("reporter_member_code={reporter_member_code}"))
  }

  if(!is.null(page)) {
    get_url <- paste0(get_url, glue::glue("&page={page}"))
  }

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
      stop("get_qr_notifications: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("wtor: get_qr_notifications(): WTO API returned a ", response$status_code, " code with the following information", httr::content(response, type="text"))
  }


  qr_notifications_df <- lapply(
    X=httr::content(response)$data,
    FUN=function(.x) {
      dplyr::tibble(
        en = .x$reporter_member$name$en,
        es = .x$reporter_member$name$es,
        fr = .x$reporter_member$name$fr,
        notification_dt = .x$notification_dt,
        document_symbol= .x$document_symbol,
        document_url = .x$document_url,
        original_language = .x$original_language,
        type = .x$type,
        covered_periods = .x$covered_periods[[1]]
      )
    }
  ) |>
    dplyr::bind_rows()



  set_cached_object(key="timeseries_qr_notifications",
                    value= qr_notifications_df)

  return(
   qr_notifications_df
  )


}
