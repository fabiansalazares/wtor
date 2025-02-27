% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_timeseries_helper_functions.R
\name{get_tariff_nmf}
\alias{get_tariff_nmf}
\title{Helper function for `get_timeseries_data()`: retrieve all MFN tariffs for a given reporting economy.}
\usage{
get_tariff_nmf(
  .economy,
  .full_names = T,
  .last_period = T,
  .year = NULL,
  .products_or_sectors = "all",
  .ad_valorem_equivalents = FALSE,
  .nocache = F
)
}
\arguments{
\item{.economy}{Character string. Reporting economy code or name.}

\item{.full_names}{Logical. Include a column called "full_name" containing the description for the HS6 codes. Default is FALSE.}

\item{.last_period}{Logical. Keep only values from the most recent period available. Default is TRUE.}

\item{.year}{Integer. Select year to download. If `NULL` it retrieves all the periods available.}

\item{.products_or_sectors}{Character string. Products or sectors to download. By default `"all"`.}

\item{.ad_valorem_equivalents}{Logical. If TRUE, request the series ('HS_A_0015') that includes ad-valorem equivalent tariff rates. Default is FALSE.}

\item{.nocache}{Logical. TRUE to disable caching of results.}
}
\value{
A tibble containing the full list of MFN tariffs applied.
}
\description{
Helper function for `get_timeseries_data()`: retrieve all MFN tariffs for a given reporting economy.
}
