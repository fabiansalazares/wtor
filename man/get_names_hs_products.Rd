% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_timeseries_helper_functions.R
\name{get_names_hs_products}
\alias{get_names_hs_products}
\title{Helper function for `get_products_sectors()`: retrieve full names for HS products and sectores}
\usage{
get_names_hs_products(.hs_level = 6, .nocache = F, .lang = "1")
}
\arguments{
\item{.hs_level}{Numeric. HS Level. Default is 6}

\item{.nocache}{Logical. TRUE to disable caching of results.}

\item{.lang}{Character string. Language of the returned output. Default is "1". "1" for English, "2" for French and "3" for Spanish.}
}
\value{
A tibble containing the full list of MFN tariffs applied.
}
\description{
Helper function for `get_products_sectors()`: retrieve full names for HS products and sectores
}
