#' Helper function for `get_timeseries_data()`: retrieve all NMF tariffs for a given reporting economy.
#' @param  .economy Character string. Reporting economy code or name.
#' @param  .full_names Logical. Include a column called "full_name" containing the description for the HS6 codes.
#' @return A tibble containing the full list of NMF tariffs applied.
#' @export
get_tariffs_nmf <- function(
    .economy,
    .full_names = T
) {


  # retrieve the code corresponding to the economy passed as argument in .economy, if necessary, or return if error
  .economy_code <- .economy

  if(.economy %in% (wtor::get_reporting_economies() |> _$name)) {
    .economy_code <- wtor::get_reporting_economies() |> filter(name == .economy) |> _$code
  }

  if(!.economy_code %in% (wtor::get_reporting_economies() |> _$code)) {
    stop(sprintf("Economy %s is not a valid reporting economy code or name", .economy_code))
  }

  # retrieve NMF tariffs data
  .tariffs_nmf_df <- wtor::get_timeseries_data(
    code = "HS_A_0015",
    reporting_economies = .economy_code,
    products_or_sectors = "all",
    pageitems=999999
  )


  # include a column called "full_name" containing the full description of the HS6 code
  if(.full_names) {

    # claude 3.5 code
    hs6_code_names_df <- wtor::get_products_sectors("HS") |>
      mutate(
        level1 = substr(code, 1, 2),
        level2 = substr(code, 1, 4),
        level3 = code
      ) %>%
      # Join the tibble with itself to get descriptions for each level
      left_join(select(., code, name), by = c("level1" = "code"), suffix = c("", "_level1")) %>%
      left_join(select(., code, name), by = c("level2" = "code"), suffix = c("", "_level2")) %>%
      # Combine descriptions for rows with 6-character codes
      mutate(
        full_name= case_when(
          nchar(code) == 6 ~ paste(name_level1, name_level2, name, sep = " - "),
          TRUE ~ name
        )
      ) |>
      select(code, full_name)

    .tariffs_nmf_df <- .tariffs_nmf_df |>
      left_join(
        hs6_code_names_df |> rename(productorsectorcode= code), by="productorsectorcode"
      )


  }

  return(.tariffs_nmf_df)
}
