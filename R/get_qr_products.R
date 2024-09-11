#' Retrieve product lists from the Quantitative Restrictions database
#' @param hs_version Character string. One of the following strings: "h1", "h2", "h3", "h4", "h5", "h6"
#' @param code Character string. The product code to fully or partially match
#' @param page Integer. Page number
#' @param description Character string. String to be searched for in the product description
#' @param nocache Logical. If TRUE, disables retrieval of results from local cache.
#' @examples get_qr_products(hs_version="h1", code="65")
get_qr_products <- function(
    hs_version,
    code=NULL,
    page=NULL,
    description=NULL,
    nocache=F
) {

  cache_key <- paste0("timeseries_qr_products_",
                      hs_version,
                      "_",
                      code,
                      "_",
                      page,
                      "_",
                      description)

  cached_qr_products  <-get_cached_object(cache_key)

  if(!is.null(cached_qr_products) & !nocache) {
    message("wtor: get_qr_products(): returning from cache.")
    return(cached_qr_products)
  }

  get_url <- "https://api.wto.org/qrs/products?"

  get_url <- sprintf(paste0(get_url, "hs_version=%s"),hs_version )

  if(!is.null(code)) {
    get_url <- sprintf(paste0(get_url, "&code=%s"), code)
  }

  if(!is.null(page)) {
    get_url <- sprintf(paste0(get_url, "&page=%s"), page)
  }

  if(!is.null(description)) {
    get_url <- sprintf(paste0(get_url, "&description=%s"), description)
  }

  get_url <- sprintf(paste0(get_url, "&hs_version=%s"),hs_version )


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
      stop("get_qr_products: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("wtor: get_qr_products(): WTO API returned a ", response$status_code, " code with the following information", httr::content(response, type="text"))
  }


  qr_products_df <- lapply(
    X=httr::content(response)$data,
    FUN=function(.x) {
      # browser()
      dplyr::tibble(
        id=.x$id,
        code=.x$code,
        tier=.x$tier,
        leaf=.x$leaf,
        description_en = .x$description$en,
        description_es = .x$description$es,
        description_fr = .x$description$fr,
        hs_version = .x$hs_version
      )
    }
  ) |> dplyr::bind_rows()

  set_cached_object(key=cache_key,
                    value= qr_products_df)

  return(
   qr_products_df
  )

}
