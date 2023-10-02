# GLM simple spline models ----

.nn_from_window = function(window, d) {
  nn = 2*window/nrow(d)
  if (nn < 8/nrow(d)) {
    nn = 8/nrow(d)
    warning("window is too small for data. using: ", nn/2*nrow(d))
  }
  return(nn)
}

.null_result = function(new_time, ...) {
  dots = rlang::list2(...)
  out = tibble::tibble(time = new_time)
  for (k in names(dots)) {
    v = dots[[k]]
    out = out %>% dplyr::mutate(
      !!paste0(k,".0.025") := v[1],
      !!paste0(k,".0.5") := v[2],
      !!paste0(k,".0.975") := v[3]
    )
  }
  return(out)
}


#' A binomial proportion estimate and associated exponential growth rate
#'
#' takes a list of times and counts based on and fits a
#' quasi-binomial model fitted with a logit link function to proportion data
#' using local regression.
#'
#' This expects d to contain one combination of:
#' * `time` and `count` and `denom` columns - e.g. all tests conducted.
#'
#' This results is a one versus others comparison binomial proportion estimate plus a
#' relative growth rate estimate.
#'
#' @param d input dataframe with: `time` (as a `time_period`), `count`, and `denom` columns.
#'   This must be ungrouped (but this function can be used directly within a `group_modify`)
#' @param ... not used and present to allow proportion model to be used in a `group_modify`
#' @param window a number of data points defining the bandwidth of the estimate, smaller values result in
#'   less smoothing, large value in more.
#' @param frequency the desired time unit between the output estimates.
#' @param deg polynomial degree
#' @param predict result a prediction. If false we return the model.
#'
#' @return a new dataframe with columns `time` (as a time period), `proportion.0.5`
#'   plus quantiles, and `relative.growth.0.5` (plus quantiles) columns or a list of
#'   2 locfit model objects (one for proportion and one for relative growth rate)
#' @export
#'
#' @example inst/examples/locfit-example.R
proportion_locfit_model = function(d, ..., window = 14, deg = 2, frequency = "1 day", predict = TRUE) {

  .exact_cols(d, "time", "count", "denom")

  output_times = full_seq.time_period(d$time, period = frequency)
  nn = .nn_from_window(window, d)

  # Not enough non zero results
  if(sum(stats::na.omit(d$count) != 0) < deg) {
    return(.null_result(output_times, proportion=c(0,0,0), relative.growth=c(NA,0,NA)))
  }

  # Not enough non one results
  # TODO: calculate CIs.
  if(sum(stats::na.omit(d$count != d$denom)) < deg) {
    return(.null_result(output_times, proportion=c(1,1,1), relative.growth=c(NA,0,NA)))
  }

  # y = cbind(d$count, d$denom - d$count)
  # fit = suppressWarnings(locfit::locfit(y~locfit::lp(time,nn=nn,deg=deg),data = d,family="qbinomial", link="logit", maxit = 5000, maxk=5000))
  # deriv = suppressWarnings(locfit::locfit(y~locfit::lp(time,nn=nn,deg=deg),deriv=1,data = d,family="qbinomial", link="logit", maxit = 5000, maxk=5000))

  fit = suppressWarnings(locfit::locfit(count~locfit::lp(time,nn=nn,deg=deg), weights = denom, data = d,family="qbinomial", link="logit", maxit = 5000, maxk=5000))
  deriv = suppressWarnings(locfit::locfit(count~locfit::lp(time,nn=nn,deg=deg), weights = denom, deriv=1,data = d,family="qbinomial", link="logit", maxit = 5000, maxk=5000))

  if (!predict) return(list(proportion = fit, relative.growth = deriv))

  # proportion
  tmp = stats::preplot(fit,newdata=output_times, se.fit = TRUE, band="local")
  # logit transformer function:
  t = tmp$tr

  # growth rate
  tmp2 = stats::preplot(deriv,newdata=output_times, se.fit = TRUE, band="local")
  t2 = function(x) x

  new_data = tibble::tibble(
    time = output_times
    ) %>%
    .result_from_fit(type = "proportion", tmp$fit, tmp$se.fit, t) %>%
    .result_from_fit(type = "relative.growth", tmp2$fit, tmp2$se.fit, t2)

  return(new_data)

}


#' Poisson time-series model.
#'
#' @param d input dataframe with: `time` (as a `time_period`), `count` columns.
#'   This must be ungrouped (but can be combined with a `group_modify`)
#' @param ... not used and present to allow proportion model to be used ina  group_modify
#' @param window a number of data points between knots, smaller values result in
#'   less smoothing, large value in more.
#' @param frequency the density of the output estimates.
#' @param deg polynomial degree
#' @param predict result a prediction. If false we return the model.
#'
#' @return a new dataframe with time (as a time period)
#' @export
#'
#' @example inst/examples/locfit-example.R
poisson_locfit_model = function(d, ..., window = 14, deg = 2, frequency = "1 day", predict = TRUE) {

  .exact_cols(d, "time", "count", .ignore = "denom")

  nn = .nn_from_window(window, d)
  output_times = full_seq.time_period(d$time, period = frequency)

  # Not enough non zero results
  if(sum(stats::na.omit(d$count) != 0) < deg) {
    return(.null_result(output_times, incidence=c(0,0,0), growth=c(NA,0,NA)))
  }

  fit = locfit::locfit(count~locfit::lp(time,nn=nn,deg=deg),data = d,family="qpoisson", link="log")
  deriv = locfit::locfit(count~locfit::lp(time,nn=nn,deg=deg),deriv=1,data = d,family="qpoisson", link="log")

  if (!predict) return(list(incidence = fit, growth = deriv))

  tmp = stats::preplot(fit,newdata=output_times, se.fit = TRUE, band="local", maxit = 5000, maxk=5000)
  # transformer function:
  t = tmp$tr

  tmp2 = stats::preplot(deriv, newdata=output_times, se.fit = TRUE, band="local", maxit = 5000, maxk=5000)
  t2 = function(x) x

  new_data = tibble::tibble(
    time = output_times
  ) %>%
    .result_from_fit(type = "incidence", tmp$fit, tmp$se.fit, t) %>%
    .result_from_fit(type = "growth", tmp2$fit, tmp2$se.fit, t2)

  return(new_data)
}






