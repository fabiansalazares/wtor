

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

  # if the requested economy is an EU-member state, return NULL and display warning: the requested economy should be European Union instead.
  if(.economy_code %in% (wtor::get_reporting_economies(gp="918") |> _$code)) {
    message(
      sprintf("%s is a EU member. To retrieve the NMF schedule of a EU-member state, please request instead the NMF tariff schedule for European Union.",
              .economy))
    message("Returning NULL")
    return(NULL)
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

    hs6_code_names_df <- hs6_code_names_df |>
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
        hs6_code_names_df |> dplyr::rename(productorsectorcode=code), by="productorsectorcode"
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

  # if the .economy is a valid name, then set .economy_code to the corresponding code
  if(.economy %in% (get_reporting_economies() |> _$name)) {
    .economy_code <- get_reporting_economies() |> dplyr::filter(name == .economy) |> _$code
  }

  # if the .economy_code is still not a valid name, stop execution
  if(!.economy_code %in% (get_reporting_economies() |> _$code)) {
    stop(sprintf("Economy %s is not a valid reporting economy code or name", .economy_code))
  }

  # retrieve the code corresponding to the partner economy passed as argument in .economy, if necessary, or return if error
  .partner_code <- .partner

  if(.partner %in% (get_partner_economies() |> _$name)) {
    .partner_code <- get_reporting_economies() |> dplyr::filter(name == .partner) |> _$code
  }

  # .partner_code can be "all" or "default", in addition to a valid partner economy code
  if(!.partner_code %in% (get_reporting_economies() |> _$code) & !.partner_code %in% c("all", "default")) {
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

    hs6_code_names_df <- hs6_code_names_df |>
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
#' @param .hs6_products Logical. Whether to return values by HS6 code Default is MTN products
#' @param .products Character string. Products to filter for. Default is "all", which results in no filtering.
#' @param  .nocache Logical. TRUE to disable caching of results.
#' @return A tibble containing the full list of NMF tariffs applied.
#' @export
get_bilateral_goods_trade <- function(
    .economy,
    .partner,
    .full_names = T,
    .last_period = T,
    .hs6_products = F,
    .products = "all",
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

  if(!.partner_code %in% (get_reporting_economies() |> _$code) & !.partner_code %in% c("all", "default")) {
    stop(sprintf("Partner %s is not a valid reporting partner code or name", .partner_code))
  }


  .indicator_code <- "HS_M_0020"
  if(.hs6_products) {
    .indicator_code <- "HS_M_0010"
  }

  # retrieve bilateral preferential  tariffs data
  .bilateral_goods_trade_df <- get_timeseries_data(
    code = .indicator_code,
    reporting_economies = .economy_code, # "all",
    partner_economies = .partner_code,
    products_or_sectors = .products,
    time_period = "all",
    pageitems = 999999,
    nocache = .nocache
  )

  if(.full_names) {
    if(.hs6_products) {
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

      .bilateral_goods_trade_df <- .bilateral_goods_trade_df |>
        dplyr::left_join(
          hs6_code_names_df |> dplyr::rename(productorsectorcode= code), by="productorsectorcode"
        )

    } else {
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
  }

  if(.last_period) {
    .bilateral_goods_trade_df <- .bilateral_goods_trade_df |>
      dplyr::filter(as.integer(year) == max(as.integer(year)))
  }

  return(.bilateral_goods_trade_df)
}


#' Helper function for `get_timeseries_data()`: retrieve services imports of a given economy
#' @param .economy Character string. Reporting economy code or name.
#' @param .partner Character string. Partner economy code or name.
#' @param .full_names Logical. Include a column called "full_name" containing the description for the HS6 codes.
#' @param .last_period Logical. Keep only values from the most recent period available. Default is TRUE.
#' @param  .nocache Logical. TRUE to disable caching of results.
#' @return A tibble containing the full list of NMF tariffs applied.
#' @export
get_services_imports <- function(
    .economy,
    .partner=NULL,
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

  if(is.null(.partner_code)) {
    .partner_code = "all"
  } else {
    if(.partner %in% (get_partner_economies() |> _$name)) {
      .partner_code <- get_reporting_economies() |> dplyr::filter(name == .partner) |> _$code
    }

    if(!.partner_code %in% (get_reporting_economies() |> _$code)) {
      stop(sprintf("Partner %s is not a valid reporting partner code or name", .partner_code))
    }
  }

  .indicator_code <- "BAT_BV_M"

  # retrieve bilateral preferential  tariffs data
  .services_imports_df <- get_timeseries_data(
    code = .indicator_code,
    reporting_economies = .economy_code, # "all",
    partner_economies = .partner_code,
    time_period = "all",
    pageitems = 999999,
    nocache = .nocache
  )

  if(.full_names) {
    # claude 3.5 code
    bop6_code_names_df <- get_products_sectors("BOP6") |>
      dplyr::mutate(
        level1 = substr(hierarchy, 1, 2),
        level2 = substr(hierarchy, 1, 4),
        level3 = substr(hierarchy, 1, 6)
      )

    bop6_code_names_df |>
      # Join the tibble with itself to get descriptions for each level
      dplyr::left_join(dplyr::select(bop6_code_names_df, code, name), by = c("level1" = "code"), suffix = c("", "_level1")) |>
      dplyr::left_join(dplyr::select(bop6_code_names_df, code, name), by = c("level2" = "code"), suffix = c("", "_level2")) |>
      # Combine descriptions for rows with 6-character codes
      dplyr::mutate(
        full_name= dplyr::case_when(
          nchar(code) == 6 ~ paste(name_level1, name_level2, name, sep = " - "),
          TRUE ~ name
        )
      ) |>
      dplyr::select(code, full_name)

    .services_imports_df <- .services_imports_df |>
      dplyr::left_join(
        bop6_code_names_df |> dplyr::rename(
          productorsectorcode = code),
        by="productorsectorcode"
      ) |>
      dplyr::select(-level1, -level2, -level3)
  }

  if(.last_period) {
    .services_imports_df <- .services_imports_df |>
      dplyr::filter(as.integer(year) == max(as.integer(year)))
  }

  return(.services_imports_df)

}

#' Helper function for `get_timeseries_data()`: retrieve services exports of a given economy
#' @param .economy Character string. Reporting economy code or name.
#' @param .partner Character string. Partner economy code or name.
#' @param .full_names Logical. Include a column called "full_name" containing the description for the HS6 codes.
#' @param .last_period Logical. Keep only values from the most recent period available. Default is TRUE.
#' @param  .nocache Logical. TRUE to disable caching of results.
#' @return A tibble containing the full list of NMF tariffs applied.
#' @export
get_services_exports <- function(
    .economy,
    .partner=NULL,
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

  if(is.null(.partner_code)) {
    .partner_code = "all"
  } else {
    if(.partner %in% (get_partner_economies() |> _$name)) {
      .partner_code <- get_reporting_economies() |> dplyr::filter(name == .partner) |> _$code
    }

    if(!.partner_code %in% (get_reporting_economies() |> _$code)) {
      stop(sprintf("Partner %s is not a valid reporting partner code or name", .partner_code))
    }
  }

  .indicator_code <- "BAT_BV_X"

  # retrieve bilateral preferential  tariffs data
  .services_exports_df <- get_timeseries_data(
    code = .indicator_code,
    reporting_economies = .economy_code, # "all",
    partner_economies = .partner_code,
    time_period = "all",
    pageitems = 999999,
    nocache = .nocache
  )

  if(.full_names) {
    # claude 3.5 code
    bop6_code_names_df <- get_products_sectors("BOP6") |>
      dplyr::mutate(
        level1 = substr(hierarchy, 1, 2),
        level2 = substr(hierarchy, 1, 4),
        level3 = substr(hierarchy, 1, 6)
      )

    bop6_code_names_df |>
      # Join the tibble with itself to get descriptions for each level
      dplyr::left_join(dplyr::select(bop6_code_names_df, code, name), by = c("level1" = "code"), suffix = c("", "_level1")) |>
      dplyr::left_join(dplyr::select(bop6_code_names_df, code, name), by = c("level2" = "code"), suffix = c("", "_level2")) |>
      # Combine descriptions for rows with 6-character codes
      dplyr::mutate(
        full_name= dplyr::case_when(
          nchar(code) == 6 ~ paste(name_level1, name_level2, name, sep = " - "),
          TRUE ~ name
        )
      ) |>
      dplyr::select(code, full_name)

    .services_exports_df <- .services_exports_df |>
      dplyr::left_join(
        bop6_code_names_df |> dplyr::rename(
          productorsectorcode = code),
        by="productorsectorcode"
      ) |>
      dplyr::select(-level1, -level2, -level3)
  }

  if(.last_period) {
    .services_exports_df <- .services_exports_df |>
      dplyr::filter(as.integer(year) == max(as.integer(year)))
  }

  return(.services_exports_df)

}


#' Helper function for `get_products_sectors()`: retrieve full names for HS products and sectores
#' @param .hs_level Numeric. HS Level. Default is 6
#' @param  .nocache Logical. TRUE to disable caching of results.
#' @param .lang Character string. Language of the returned output. Default is "1". "1" for English, "2" for French and "3" for Spanish.
#' @return A tibble containing the full list of NMF tariffs applied.
#' @export
get_names_hs_products <- function(
    .hs_level=6,
    .nocache=F,
    .lang="1"
    ) {

  hs_code_names_df <- get_products_sectors("HS", lang=.lang) |>
    dplyr::mutate(
      level1 = substr(code, 1, 2),
      level2 = substr(code, 1, 4),
      level3 = code
    )

  hs_code_names_df <- hs_code_names_df |>
    # Join the tibble with itself to get descriptions for each level
    dplyr::left_join(dplyr::select(hs_code_names_df, code, name), by = c("level1" = "code"), suffix = c("", "_level1")) |>
    dplyr::left_join(dplyr::select(hs_code_names_df, code, name), by = c("level2" = "code"), suffix = c("", "_level2")) |>
    # Combine descriptions for rows with n-character codes
    dplyr::mutate(
      full_name= dplyr::case_when(
        nchar(code) == .hs_level ~ paste(name_level1, name_level2, name, sep = " - "),
        TRUE ~ name
      )
    ) |>
    dplyr::filter(stringr::str_length(hierarchy) == .hs_level) |>
    dplyr::select(code, full_name) |>
      dplyr::mutate(full_name = htmltools::htmlEscape(full_name, attribute = FALSE)) |>
      dplyr::mutate(full_name = stringr::str_remove_all(full_name,"'"))



  hs_code_names_df

}

#' Get a list of the economies belonging to a given group
#' @param .type Character string. Either "reporting" or "partner". Default is "reporting".
#' @param .group Character string. Code of the group whose members are to be retrieved. Default is NULL. Retrieve a list of codes with `get_groups()`
#' @param .lang Character string. Language of the returned output. Default is "1". "1" for English, "2" for French and "3" for Spanish.
#' @param  .nocache Logical. TRUE to disable caching of results. Default is FALSE.
#' @export
get_group_members <- function(
    .type="reporting",
    .group = NULL,
    .lang="1",
    .nocache=F
) {

  if (.type == "reporting") {
    get_reporting_economies(
      gp=.group
    )
  } else {
    get_partner_economies(
      gp=.group
    )
  }
}

#' Check whether a given economy is a member of the WTO
#' @param .code Character string. ISO code of the economy whose WTO member status is to be checked.
#' @param  .nocache Logical. TRUE to disable caching of results. Default is FALSE.
#' @returns TRUE is the economy is a member of the WTO
#' @export
is_wto_member <- function(
    .code,
    .nocache="F"
) {

  if (.code %in% (get_group_members(.group="900") |> _$code)){
    return(TRUE)
  }

  return(FALSE)

}

# dplyr::bind_rows(get_tariff_nmf("Japan") |>
# dplyr::mutate(tipo="nmf"), get_tariff_preferential("Japan", "Spain") |>
# dplyr::mutate(tipo="pref")) |>
# dplyr::mutate(code = productorsectorcode |> as.integer()) |>
# ggplot2::ggplot(aes(x=code, y=value, color=tipo))  + geom_point()


# wtor::get_services_exports("Japan", "Spain")
