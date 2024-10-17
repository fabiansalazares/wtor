#' Helper function for `get_timeseries_data()`: retrieve all NMF tariffs for a given reporting economy.
#' @param  .economy Character string. Reporting economy code or name.
#' @param  .full_names Logical. Include a column called "full_name" containing the description for the HS6 codes.
#' @param  .last_period Logical. Keep only values from the most recent period available. Default is TRUE.
#' @param  .nocache Logical. TRUE to disable caching of results.
#' @return A tibble containing the full list of NMF tariffs applied.
#' @export
get_tariff_nmf <- function(
    .economy,
    .full_names = T,
    .last_period = T,
    .nocache = F
) {

  # retrieve the code corresponding to the economy passed as argument in .economy, if necessary, or return if error
  .economy_code <- .economy

  if(.economy %in% (get_reporting_economies() |> _$name)) {
    .economy_code <- get_reporting_economies() |> dplyr::filter(name == .economy) |> _$code
  }

  if(!.economy_code %in% (get_reporting_economies() |> _$code)) {
    stop(sprintf("Economy %s is not a valid reporting economy code or name", .economy_code))
  }

  # retrieve NMF tariffs data
  .tariffs_nmf_df <- get_timeseries_data(
    code = "HS_A_0015",
    reporting_economies = .economy_code,
    products_or_sectors = "all",
    pageitems=999999,
    nocache = .nocache
  )


  # include a column called "full_name" containing the full description of the HS6 code
  if(.full_names) {

    # claude 3.5 code
    hs6_code_names_df <- get_products_sectors("HS") |>
      dplyr::mutate(
        level1 = substr(code, 1, 2),
        level2 = substr(code, 1, 4),
        level3 = code
      )

    hs6_code_names_df |>
      # Join the tibble with itself to get descriptions for each level
      dplyr::left_join(dplyr::select(hs6_code_names_df, code, name), by = c("level1" = "code"), suffix = c("", "_level1")) |>
      dplyr::left_join(dplyr::select(hs6_code_names_df, code, name), by = c("level2" = "code"), suffix = c("", "_level2")) |>
      # Combine descriptions for rows with 6-character codes
      dplyr::mutate(
        full_name= dplyr::case_when(
          nchar(code) == 6 ~ paste(name_level1, name_level2, name, sep = " - "),
          TRUE ~ name
        )
      ) |>
      dplyr::select(code, full_name)

    .tariffs_nmf_df <- .tariffs_nmf_df |>
      dplyr::left_join(
        hs6_code_names_df |> dplyr::rename(productorsectorcode= code), by="productorsectorcode"
      )


  }

  if(.last_period) {
    .tariffs_nmf_df <- .tariffs_nmf_df |>
      dplyr::filter(as.integer(year) == max(as.integer(year)))
  }

  return(.tariffs_nmf_df)
}


#' Helper function for `get_timeseries_data()`: retrieve bilateral preferential tariffs for a given reporting economy and a partner economy.
#' @param .economy Character string. Reporting economy code or name.
#' @param .partner Character string. Partner economy code or name.
#' @param .full_names Logical. Include a column called "full_name" containing the description for the HS6 codes.
#' @param .last_period Logical. Keep only values from the most recent period available. Default is TRUE.
#' @param  .nocache Logical. TRUE to disable caching of results.
#' @return A tibble containing the full list of NMF tariffs applied.
#' @export
get_tariff_preferential <- function(
    .economy,
    .partner,
    .full_names = T,
    .last_period = T,
    .nocache = F
) {

  # retrieve the code corresponding to the economy passed as argument in .economy, if necessary, or return if error
  .economy_code <- .economy

  if(.economy %in% (get_reporting_economies() |> _$name)) {
    .economy_code <- get_reporting_economies() |> dplyr::filter(name == .economy) |> _$code
  }

  if(!.economy_code %in% (get_reporting_economies() |> _$code)) {
    stop(sprintf("Economy %s is not a valid reporting economy code or name", .economy_code))
  }

  # retrieve the code corresponding to the partner economy passed as argument in .economy, if necessary, or return if error
  .partner_code <- .partner

  if(.partner %in% (get_partner_economies() |> _$name)) {
    .partner_code <- get_reporting_economies() |> dplyr::filter(name == .partner) |> _$code
  }

  if(!.partner_code %in% (get_reporting_economies() |> _$code)) {
    stop(sprintf("Partner %s is not a valid reporting partner code or name", .partner_code))
  }

  # retrieve bilateral preferential  tariffs data
  .tariffs_preferential_df <- get_timeseries_data(
    code = "HS_P_0070",
    reporting_economies = .economy_code,
    partner_economies = .partner_code,
    products_or_sectors = "all",
    pageitems = 999999,
    nocache = .nocache
  )

  # include a column called "full_name" containing the full description of the HS6 code
  if(.full_names) {
    # claude 3.5 code
    hs6_code_names_df <- get_products_sectors("HS") |>
      dplyr::mutate(
        level1 = substr(code, 1, 2),
        level2 = substr(code, 1, 4),
        level3 = code
      )

    hs6_code_names_df |>
      # Join the tibble with itself to get descriptions for each level
      dplyr::left_join(dplyr::select(hs6_code_names_df, code, name), by = c("level1" = "code"), suffix = c("", "_level1")) |>
      dplyr::left_join(dplyr::select(hs6_code_names_df, code, name), by = c("level2" = "code"), suffix = c("", "_level2")) |>
      # Combine descriptions for rows with 6-character codes
      dplyr::mutate(
        full_name= dplyr::case_when(
          nchar(code) == 6 ~ paste(name_level1, name_level2, name, sep = " - "),
          TRUE ~ name
        )
      ) |>
      dplyr::select(code, full_name)

    .tariffs_preferential_df <- .tariffs_preferential_df |>
      dplyr::left_join(
        hs6_code_names_df |> dplyr::rename(productorsectorcode= code), by="productorsectorcode"
      )
  }

  if(.last_period) {
    .tariffs_preferential_df <- .tariffs_preferential_df |>
      dplyr::filter(as.integer(year) == max(as.integer(year)))
  }

  return(.tariffs_preferential_df)
}


#' Helper function for `get_timeseries_data()`: retrieve bilateral goods trade between two reporting members
#' @param .economy Character string. Reporting economy code or name.
#' @param .partner Character string. Partner economy code or name.
#' @param .full_names Logical. Include a column called "full_name" containing the description for the HS6 codes.
#' @param .last_period Logical. Keep only values from the most recent period available. Default is TRUE.
#' @param .mtn_sectors Character string. MTN sectors to filter for. Default is "all".
#' @param  .nocache Logical. TRUE to disable caching of results.
#' @return A tibble containing the full list of NMF tariffs applied.
#' @export
get_bilateral_goods_trade <- function(
    .economy,
    .partner,
    .full_names = T,
    .last_period = T,
    .mtn_sectors = "all",
    .nocache = F
) {

  # retrieve the code corresponding to the economy passed as argument in .economy, if necessary, or return if error
  .economy_code <- .economy

  if(.economy %in% (get_reporting_economies() |> _$name)) {
    .economy_code <- get_reporting_economies() |> dplyr::filter(name == .economy) |> _$code
  }

  if(!.economy_code %in% (get_reporting_economies() |> _$code)) {
    stop(sprintf("Economy %s is not a valid reporting economy code or name", .economy_code))
  }

  # retrieve the code corresponding to the partner economy passed as argument in .economy, if necessary, or return if error
  .partner_code <- .partner

  if(.partner %in% (get_partner_economies() |> _$name)) {
    .partner_code <- get_reporting_economies() |> dplyr::filter(name == .partner) |> _$code
  }

  if(!.partner_code %in% (get_reporting_economies() |> _$code)) {
    stop(sprintf("Partner %s is not a valid reporting partner code or name", .partner_code))
  }


  # retrieve bilateral preferential  tariffs data
  .bilateral_goods_trade_df <- get_timeseries_data(
    # code = "HS_P_0070",
    code = "HS_M_0020",
    reporting_economies = .economy_code, # "all",
    partner_economies = .partner_code,
    products_or_sectors = .mtn_sectors,
    time_period = "all",
    pageitems = 999999,
    nocache = .nocache
  )

  if(.full_names) {
    mt2_code_names_df <- get_products_sectors("MT2")

    .bilateral_goods_trade_df <- .bilateral_goods_trade_df |>
      dplyr::left_join(
        mt2_code_names_df |>
          dplyr::rename(productorsectorcode= code),
        by="productorsectorcode"
      ) |>
      dplyr::rename(
        product_name = name
      )
  }

  if(.last_period) {
    .bilateral_goods_trade_df <- .bilateral_goods_trade_df |>
      dplyr::filter(as.integer(year) == max(as.integer(year)))
  }

  return(.bilateral_goods_trade_df)
}


# bind_rows(get_tariff_nmf("Japan") |>
# dplyr::mutate(tipo="nmf"), get_tariff_preferential("Japan", "Spain") |>
# dplyr::mutate(tipo="pref")) |>
# dplyr::mutate(code = productorsectorcode |> as.integer()) |>
# ggplot2::ggplot(aes(x=code, y=value, color=tipo))  + geom_point()
