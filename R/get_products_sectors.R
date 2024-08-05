#' @export
get_products_sectors <- function(
    language="1",
    pc="all",
    nocache=F) {

  cached_products_sectors <-get_cached_object("timeseries_products_sectors")

  if(!is.null(cached_products_sectors) & !nocache) {
    message("get_products_sectors: returning from cache.")
   return(cached_products_sectors)
  }

  if(pc == "all") {
    message("Retrieving all products and sectors in ALL product classifications.")
    message("Make sure to specify a 'pc' argument if you want to limit it to a given product classification")
    message("Available product classifications can be retrieved with wtor::get_product_classifications")
  }

  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/products?pc={pc}&lang={language}"
    )

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
      stop("get_products_sectors: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_products_sectors: WTO API returned a ", response$status_code, " code")
  }

  products_sectors_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        code = .x$code,
        name= .x$name,
        note = .x$note,
        productClassification = .x$productClassification,
        codeUnique = .x$codeUnique,
        displayOrder = .x$displayOrder,
        hierarchy = .x$hierarchy,
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key="timeseries_products_sectors",
                    value= products_sectors_df)

  return(
   products_sectors_df
  )

}
