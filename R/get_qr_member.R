#' Retrieve a list of member countries names and codes that can be used as arguments to other QR functions.
#' @param member_code Code of the member country.
#' @param name Name of the member country.
#' @param page Page to be retrieved
#' @examples get_qr_member(name="Afghanistan")
#' @return A tibble containing the name and code information for the requested country.
get_qr_members <- function(
    member_code=NULL,
    name=NULL,
    page=NULL,
    nocache=F
    ) {

  cached_qr_members <-get_cached_object("timeseries_qr_members")

  if(!is.null(cached_qr_members) & !nocache) {
    message("get_qr_members: returning from cache.")
   return(cached_qr_members)
  }

  if(!is.null(member_code) & length(member_code) == 1) {
    get_url <- glue::glue(
      "https://api.wto.org/qrs/members?member_code={member_code}"
    )
  } else if (!is.null(name) & length(name) == 1) {
    get_url <- glue::glue(
      "https://api.wto.org/qrs/members?name={name}"
    )
  } else {
    stop("wtor: get_qr_members: either member_code or name arguments are required.")
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
      stop("get_qr_members: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("wtor: get_qr_members(): WTO API returned a ", response$status_code, " code with the following information", httr::content(response, type="text"))
  }

  qr_members_df <- httr::content(response)$data[[1]]$name |> dplyr::as_tibble()



  set_cached_object(key="timeseries_qr_members",
                    value= qr_members_df)

  return(
   qr_members_df
  )

}
