% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_timeseries.R
\name{get_timeseries_data}
\alias{get_timeseries_data}
\title{Retrieve timeseries data.}
\usage{
get_timeseries_data(
  code,
  reporting_economies,
  partner_economies = NULL,
  time_period = "default",
  products_or_sectors = "default",
  subproducts_subsectors = FALSE,
  format_output = "csv",
  mode_output = "codes",
  decimals = "default",
  heading_style = "M",
  offset = 0,
  max_records = 10000,
  meta = FALSE,
  nocache = F,
  nopagination = F,
  pageitems = 10000,
  request_max_attempts = 10
)
}
\arguments{
\item{code}{indicator code. Required.}

\item{reporting_economies}{A vector or a scalar containing the codes and/or names of the reporting economies.}

\item{partner_economies}{A vector or a scalar containing the codes and/or names of the partner economies. Not all indicators allow for this parameter.}

\item{time_period}{A string containing either "default", "all", or specific periods according to the format described in https://apiportal.wto.org/api-details#api=version1&operation=post-data}

\item{products_or_sectors}{A string containing either "default", "all", a specific product classification such as HS2, HS4, HS6, or a comma separated list of product codes belonging to AG,AGFOFI,MAIS,...}

\item{subproducts_subsectors}{Either TRUE or FALSE depending on whether to include or not subproducts and subsectors.}

\item{format_output}{Either "csv" or "json", depending on the output format in which to obtain the response to the POST request. It does not have any impact on the function returned dataframe.}

\item{mode_output}{Either "codes" (by default) or "full", depending on whether the columns in the returned dataframe will contain}

\item{decimals}{Either "default" or a string containing the number of decimals that the output should contain.}

\item{heading_style}{Either "H" for human-readable headers and "M" for machine-readable codes.}

\item{offset}{Number of datapoints to offset in the request. Usefull if manual pagination is to be implemented.}

\item{max_records}{Maximum number of rows to return.}

\item{meta}{TRUE to include metadata.}

\item{nocache}{TRUE to disable caching of results.}

\item{nopagination}{TRUE to disable pagination of requests.}

\item{pageitems}{Number of rows per paginated request. By default 10.000}

\item{request_max_attempts}{Maximum number of request attempts.}
}
\value{
A tibble containing the request data.
}
\description{
Retrieve timeseries data.
}
