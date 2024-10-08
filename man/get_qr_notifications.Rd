% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_qr_notifications.R
\name{get_qr_notifications}
\alias{get_qr_notifications}
\title{Retrieve a the notified quantitative restrictions for a given year and current reporting member, from the Quantitative Restrictions API}
\usage{
get_qr_notifications(
  page = NULL,
  reporter_member_code = NULL,
  notification_year = NULL,
  nocache = T
)
}
\arguments{
\item{page}{Page to be retrieved}

\item{reporter_member_code}{Member code}

\item{notification_year}{Year of the notification}

\item{nocache}{Logical. If TRUE, disables retrieval of results from local cache.}
}
\description{
Retrieve a the notified quantitative restrictions for a given year and current reporting member, from the Quantitative Restrictions API
}
\examples{
get_qr_notifications(reporter_member_code="C554", notification_year="2022")
}
