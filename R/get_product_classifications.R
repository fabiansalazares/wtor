#' Retrieve a list of product classifications that are allowed as arguments to other functions.
#' @param language 1 for English, 2 for French or 3 for Spanish
#' @examples get_periods()
#' @export
get_product_classifications <- function(
    language="1",
    nocache=F) {

  cached_product_classifications <-get_cached_object("timeseries_product_classifications")

  if(!is.null(cached_product_classifications) & !nocache) {
    message("get_product_classifications: returning from cache.")
   return(cached_product_classifications)
  }


  get_url <- glue::glue(
    "https://api.wto.org/timeseries/v1/product_classifications?lang={language}"
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
      stop("get_product_classifications: httr::GET error: ", e)
    }
  )

  if(response$status_code != "200") {
    stop("get_product_classifications: WTO API returned a ", response$status_code, " code")
  }

  product_classifications_df <- lapply(
    X=httr::content(response),
    FUN=function(.x) {
      dplyr::tibble(
        code = .x$code,
        name= .x$name
      )
    }
  ) |>
    dplyr::bind_rows()

  set_cached_object(key="timeseries_product_classifications",
                    value= product_classifications_df)

  return(
   product_classifications_df
  )

}
