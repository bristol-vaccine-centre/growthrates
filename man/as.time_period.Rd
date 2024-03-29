% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/periodic-times.R
\name{as.time_period}
\alias{as.time_period}
\alias{c.time_period}
\alias{[.time_period}
\alias{[<-.time_period}
\alias{[[.time_period}
\alias{[[<-.time_period}
\alias{is.time_period}
\alias{print.time_period}
\title{Convert to a time period class}
\usage{
as.time_period(x, unit = NULL, start_date = NULL, anchor = NULL, ...)

\method{c}{time_period}(..., recursive = F)

\method{[}{time_period}(x, ...)

\method{[}{time_period}(x, ...) <- value

\method{[[}{time_period}(x, ...)

\method{[[}{time_period}(x, ...) <- value

is.time_period(x)

\method{print}{time_period}(x, ...)
}
\arguments{
\item{x}{a vector of numbers (may be integer or real) or a time_period}

\item{unit}{the length of one unit of time. This will be either a integer
number of days, or a specification such as "1 week", or another \code{time_period}.
If \code{x} is a \code{time_period}, and the unit is different then from that of \code{x}
this will return a new \code{time_period} using the new units.}

\item{start_date}{the zero time date as something that can be coerced to a
date. If the \code{x} input is already a \code{time_period} and this is different to its
\code{start_date} then it will be recalibrated to use the new start date.}

\item{anchor}{only relevant is \code{x} is a vector of dates and \code{start_date} is
not specified, this is a date, or "start" or "end" or a weekday name e.g.
"mon". With the vector of dates in \code{x} it will find a reference date for
the time-series. If this is \code{NULL} and \code{start_date} is also \code{NULL} it will
fall back to \code{getOption("day_zero","2019-12-29")}}

\item{...}{used for subtype implementations}

\item{recursive}{concatenate recursively}

\item{value}{the value}
}
\value{
a \code{time_period} class, consisting of a vector of numbers, with
attributes for time period and \code{start_date}
}
\description{
Time periods are just a zero based numeric representation of dates with a
time unit baked in. This allows variable length periods (e.g. days or weeks),
and fractional days to be represented in a consistent(ish) way
}
\section{Functions}{
\itemize{
\item \code{c(time_period)}: Combine \code{time_period}

\item \code{[}: Subset a \code{time_period}

\item \code{`[`(time_period) <- value}: Assign values to a subset of a \code{time_period}

\item \code{[[}: Get a value in a \code{time_period}

\item \code{`[[`(time_period) <- value}: Assign a value in a \code{time_period}

\item \code{is.time_period()}: Check is a \code{time_period}

\item \code{print(time_period)}: Print a time_period

}}
\examples{
# 100 weeks from 2020-01-01

tmp = as.time_period(0:100, 7, "2020-01-01")
as.Date(tmp)

range(tmp)
min(tmp)
tmp2 = as.integer(as.Date(tmp))
# testthat::expect_true(all(na.omit(tmp2-lag(tmp2)) == 7))

tmp2 = as.time_period(0:23, 1/24, "2020-01-01")
as.POSIXct(tmp2)

# convert timeseries to new "unit"
tmp = as.time_period(0:100, 7, "2020-01-01")
tmp2 = as.time_period(tmp,1)
testthat::expect_equal(as.numeric(tmp2), 0:100*7)
}
\concept{time_period}
