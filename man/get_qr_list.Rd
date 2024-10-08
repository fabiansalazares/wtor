% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_qr_list.R
\name{get_qr_list}
\alias{get_qr_list}
\title{Retrieve a list of quantitative restrictions}
\usage{
get_qr_list(
  page = NULL,
  reporter_member_code = NULL,
  in_force_only = NULL,
  year_of_entry_into_force = NULL,
  product_codes = NULL,
  product_ids = NULL,
  nocache = F
)
}
\arguments{
\item{page}{Page number}

\item{reporter_member_code}{Country code of the reporting member}

\item{in_force_only}{TRUE to restrict to quantitative restrictions currently in force}

\item{year_of_entry_into_force}{Year in which the quantitative restrictions went into force}

\item{product_codes}{A vector or a scalar containing the product code(s) that partially or fully match the codes in any HS version}

\item{product_ids}{A vector or a scalar containing the product of product ids in format such as hx-yyyyyy, where x is the HS version and yyyyyy the product id.}

\item{nocache}{Logical. If TRUE, disables retrieval of results from local cache.}
}
\value{
A tibble containing the list of matching quantitative restrictions.
}
\description{
Retrieve a list of quantitative restrictions
}
\examples{
get_qr_list()
}
