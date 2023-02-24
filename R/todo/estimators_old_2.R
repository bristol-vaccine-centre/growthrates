## Estimators ----

#' Estimate an incidence rate from count data using a poisson regression
#'
#' takes a list of times and counts based on a quasi-poisson model fitted to
#' count data using local regression. expects timed_df to contain time and count
#' columns only.
#'
#' @param timeseries a timeseries including a `count` column. Any `group` or `.class`
#'   columsn are used for grouping and treated as independent time series
#' @param window the width of the window of data to choose in days. If your data
#'   is weekly you probably will want to up this from it default value of 14.
#'   Larger numbers give fewer wiggles.
#' @param deg the polynomial degree to fit (higher numbers give more wiggles)
#' @param daily_incidence Do you want to return estimates of the incidence as a daily
#'   rate regardless of the periodicity of the input, or stick with the time unit of
#'   the input? regardless of this setting results will be returned as a daily
#'   frequency (but might be in cases per week if the input data was weekly).
#'
#' @return a new timeseries with time, date, and various incidence and growth
#'   rate metrics
#' @export
#'
#' @examples
#' if (FALSE) {
#'   utils::vignette("estimators-example", package="growthrates")
#' }
estimate_poisson_rate = function(timeseries, window=14, deg=2, daily_incidence = FALSE) {

  timeseries %>% .check_timeseries("count")
  if (.is_multiple(timeseries, treat_classes_as_group = TRUE)) {
    return(.handle_multiple(timeseries, treat_classes_as_group = TRUE, window=window, deg=deg, daily_incidence=daily_incidence))
  } else {

    nn = .nn_from_window(window, timeseries)
    new_time = .daily_times(timeseries$.time)

    # Not enough non zero results
    if(sum(na.omit(timeseries$count) != 0) < deg) {
      return(.null_result(new_time, incidence=c(0,0,0), growth=c(NA,0,NA)))
    }

    fit = locfit::locfit(count~locfit::lp(.time,nn=nn,deg=deg),data = timeseries,family="qpoisson", link="log")
    deriv = locfit::locfit(count~locfit::lp(.time,nn=nn,deg=deg),deriv=1,data = timeseries,family="qpoisson", link="log")

    tmp = stats::preplot(fit,newdata=new_time, se.fit = TRUE, band="local", maxit = 5000, maxk=5000)
    # transformer function:
    t = tmp$tr

    tmp2 = stats::preplot(deriv,newdata=new_time, se.fit = TRUE, band="local", maxit = 5000, maxk=5000)

    # if the original timeseries is weekly and we want to get out a daily rate
    # we need to adjust the estimated mean by x=x-log(7) before transformation
    # (i.e. the same as x/7 after transformation). This preserves confidence limits
    if (daily_incidence) {
      # This will be approximate if the unit is months etc.
      period = attributes(timeseries$.time)$unit  / lubridate::as.period("1 day")
    } else {
      period = 1
    }

    out = tibble::tibble(
      .time = new_time,
      incidence.0.025 = .opt(t(stats::qnorm(0.025, tmp$fit-log(period), tmp$se.fit))),
      incidence.0.5 = t(stats::qnorm(0.5, tmp$fit-log(period), tmp$se.fit)),
      incidence.0.975 = .opt(t(stats::qnorm(0.975,tmp$fit-log(period),tmp$se.fit))),
      growth.0.025 = .opt(stats::qnorm(0.025, tmp2$fit, tmp2$se.fit)),
      growth.0.5 = .opt(stats::qnorm(0.5, tmp2$fit, tmp2$se.fit)),
      growth.0.975 = .opt(stats::qnorm(0.975, tmp2$fit, tmp2$se.fit))
    )
    return(out %>% as.timeseries())
  }

}



.null_result = function(new_time, ...) {
  dots = rlang::list2(...)
  out = tibble(.time = new_time)
  for (k in names(dots)) {
    v = dots[[k]]
    out = out %>% mutate(
      !!paste0(k,".0.5") := v[2],
      !!paste0(k,".0.025") := v[1],
      !!paste0(k,".0.975") := v[3]
    )
  }
  return(out)
}

#' A binomial proportion estimate and associated exponential growth rate
#'
#' takes a list of times and counts based on and fits a
#' quasi-binomial model fitted with a logit link function to proportion data
#' using local regression. Behaviour depends on inputs a bit.
#'
#' This expects timeseries to contain one combination of:
#' * time and proportion columns (+/- class), and in this case fits
#' independent models to any classes and with proportion as the response and
#' time as the predictor. This format assumes you have pre-calculated proportions
#' for each day, but does not take into account sample size for individual days
#' when calculating confidence intervals.
#' * time and count and total columns - Total for example might be all tests
#' conducted for example. In this case if `quick=TRUE` it calculates
#' the proportion and proceeds as above. If `quick=FALSE` it expands the
#' time-series of counts to a line list of individuals with either 1 for
#' positive or 0 for negative outcome (count positives, total-count negatives).
#' The model is then fitted to individuals with a outcome predicted by time. This
#' is only worth doing if numbers are small. If class is present, then each
#' class will be treated as an independent time series.
#' * time and class and count, without total columns. In this case the class
#' columns are treated as a complete set of observations, and the algorithm
#' calculates a total based on the sum of the class counts. This results in a one
#' versus others comparison, which is only strictly valid is class is binomial
#' but will work for multinomial. Once the total is defined the algorithm
#' proceeds as above depending on the `quick` parameter.
#'
#' @param timeseries a timeseries, which must have a `.time` and one combination of
#' `proportion` OR (`count` AND `total`) OR (`.class` AND `count`).
#' @param ... not used. Allows use in a group modify
#' @param window the number of days data to include, Should be upped for weekly
#'   time series.
#' @param deg the polynomial degree (higher = more wiggly)
#' @param quick should rely solely on count data wherever possible.
#'
#' @return a timeseries including `proportion` columns, and `relative.growth`
#'   columns. The growth is relative to whatever makes up in the `total` column.
#'   This might be the total number of tests (which can be complex to interpret
#'   in the situation where testing is changing) if the `total` column was
#'   given, or might be the total number of all variants of the class (e.g. when
#'   comparing genomic variants) if the `.class` column was given.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   utils::vignette("estimators-example", package="growthrates")
#' }
estimate_binomial_proportion = function(timeseries, ... ,window=14,deg=2, quick=FALSE) {

  timeseries %>% .check_timeseries()
  timeseries %>% .check_timeseries("count")

  if (.is_multiple(timeseries, treat_classes_as_group = FALSE)) {

    return(.handle_multiple(timeseries, treat_classes_as_group = FALSE, window=window, deg=deg, quick=quick, ...))

  } else {

    # Deal with a multi class timeseries by calculating the proportions for each
    # individual value of the class and then recursively calling the method
    # on the individual data (with proportions)
    if (is.multinomial(timeseries)) {
      # so this is a one versus many and has a class column. it maybe proportion is already calculated
      # but if not:
      if (!.has_cols(timeseries,"proportion")) {
        if (!.has_cols(timeseries,"total")) {
          if (!.has_cols(timeseries,"count")) {
            stop("not enough data, at a minimum multinomial data must have a class and count column")
          } else {
            # count and class only
            # create a total column. proportion will be calculates as a
            # one versus others binomial
            timeseries = timeseries %>% dplyr::group_by(.time) %>% dplyr::mutate(total = sum(count))
          }
        }
      }
      return(
        timeseries %>%
          dplyr::group_by(.class) %>%
          # does group_modify preserve the attributes of the time column?
          dplyr::group_modify(function(d,g,..) {
            d %>% as.timeseries() %>% estimate_binomial_proportion(window=window, deg=deg, quick=quick)
          }) %>%
          dplyr::ungroup() %>%
          .copy_metadata(timeseries) %>%
          as.timeseries()
      )
    }


    # If when
    if (
      !.has_cols(timeseries,"proportion") &&
      !.has_cols(timeseries,"count","total")
    ) stop("not enough data we need at least time (integer, seqeuential), count, total columns, or time, proportion columns")

    nn = .nn_from_window(window, timeseries)
    new_time = .daily_times(timeseries$.time)

    if (!.has_cols(timeseries,"count","total")) {
      # if we only have proportion we cannot do the slower method
      quick=TRUE
    } else if (sum(timeseries$total) > 10000) {
      quick=TRUE
    }

    if (!.has_cols(timeseries,"proportion")) {
      # we know count and total must be present
      timeseries = timeseries %>% dplyr::mutate(proportion = count/total)
    }

    if(sum(na.omit(timeseries$proportion) != 0) <= deg+1) {
      return(.null_result(new_time, proportion=c(0,0,1), relative.growth=c(NA,0,NA)))
    }

    if(sum(na.omit(timeseries$proportion) != 1) <= deg+1) {
      return(.null_result(new_time, proportion=c(0,1,1), relative.growth=c(NA,0,NA)))
    }

    # if (!quick) {
    #   message("using the slower maybe more accurate (for small numbers) method")
    #   linelist = timeseries %>%
    #     dplyr::mutate(positive = count, negative = total-count) %>%
    #     dplyr::select(.time,positive,negative) %>%
    #     tidyr::pivot_longer(cols = c(positive,negative), names_to = ".class", values_to = "count") %>%
    #     dplyr::group_by(dplyr::across(c(tidyselect::any_of("group"),.time,.class))) %>%
    #     # this next line changes a dataframe with single (.time,.class,count=n) row to one
    #     # where (.time,.class,count=1)*n rows:
    #     dplyr::group_modify(function(d,g,..) {
    #       if (nrow(d) > 1) stop("timed_df had some duplicate rows in it (probably for time)")
    #       return(tibble::tibble(count = rep(1,d$count)))
    #     }) %>%
    #     dplyr::ungroup() %>%
    #     dplyr::mutate(class_bool = ifelse(.class=="positive",1,0))
    #   # now we can fit the qbinomial model using locfit
    #   fit = locfit::locfit(class_bool~locfit::lp(.time,nn=nn,deg=deg),data = linelist,family="qbinomial", link="logit", maxit = 5000, maxk=5000)
    #   deriv = locfit::locfit(class_bool~locfit::lp(.time,nn=nn,deg=deg),deriv=1,data = linelist,family="qbinomial", link="logit", maxit = 5000, maxk=5000)
    # } else {

    message("using the quicker inaccurate method")
    tryCatch({
      fit = suppressWarnings(locfit::locfit(proportion~locfit::lp(.time,nn=nn,deg=deg),data = timeseries,family="qbinomial", link="logit", maxit = 5000, maxk=5000))
      deriv = suppressWarnings(locfit::locfit(proportion~locfit::lp(.time,nn=nn,deg=deg),deriv=1,data = timeseries,family="qbinomial", link="logit", maxit = 5000, maxk=5000))
      tmp = stats::preplot(fit,newdata=new_time, se.fit = TRUE, band="local")
      # logit transformer function:
      t = tmp$tr

      tmp2 = stats::preplot(deriv,newdata=new_time, se.fit = TRUE, band="local")

      tibble::tibble(
        .time = new_time,
        proportion.0.025 = .opt(t(stats::qnorm(0.025, tmp$fit, tmp$se.fit))),
        proportion.0.5 = .opt(t(stats::qnorm(0.5, tmp$fit, tmp$se.fit))),
        proportion.0.975 = .opt(t(stats::qnorm(0.975,tmp$fit,tmp$se.fit))),
        relative.growth.0.025 = .opt(stats::qnorm(0.025, tmp2$fit, tmp2$se.fit)),
        relative.growth.0.5 = .opt(stats::qnorm(0.5, tmp2$fit, tmp2$se.fit)),
        relative.growth.0.975 = .opt(stats::qnorm(0.975, tmp2$fit, tmp2$se.fit))
      ) %>% mutate(
        proportion.0.025 = ifelse(proportion.0.975>0.9999 & proportion.0.025 < 0.00001, NA_real_, proportion.0.025),
        proportion.0.5 = ifelse(proportion.0.975>0.9999 & proportion.0.025 < 0.00001, NA_real_, proportion.0.5),
        proportion.0.975 = ifelse(proportion.0.975>0.9999 & proportion.0.025 < 0.00001, NA_real_, proportion.0.975)
      )
    },error=function(e) {
      .null_result(new_time, proportion=c(NA,NA,NA), relative.growth=c(NA,NA,NA))
    }) %>% as.timeseries()
    # }

    # tmp = stats::preplot(fit,newdata=new_time, se.fit = TRUE, band="local")
    # # logit transformer function:
    # t = tmp$tr
    #
    # tmp2 = stats::preplot(deriv,newdata=new_time, se.fit = TRUE, band="local")
    #
    # tibble::tibble(
    #   .time = new_time,
    #   proportion.0.025 = .opt(t(stats::qnorm(0.025, tmp$fit, tmp$se.fit))),
    #   proportion.0.5 = .opt(t(stats::qnorm(0.5, tmp$fit, tmp$se.fit))),
    #   proportion.0.975 = .opt(t(stats::qnorm(0.975,tmp$fit,tmp$se.fit))),
    #   relative.growth.0.025 = .opt(stats::qnorm(0.025, tmp2$fit, tmp2$se.fit)),
    #   relative.growth.0.5 = .opt(stats::qnorm(0.5, tmp2$fit, tmp2$se.fit)),
    #   relative.growth.0.975 = .opt(stats::qnorm(0.975, tmp2$fit, tmp2$se.fit))
    # ) %>% as.timeseries()
  }
}





#' Estimate the probabilities of a multinomial class based on count data
#'
#' takes a list of times, classes and counts, e.g. a COGUK variant like data set
#' with time, (multinomial) class (e.g. variant) and count being the count in
#' that time period. Fits a quadratic B-spline on time to the proportion of the
#' data using `nnet::multinom`.
#'
#' @param timeseries a timeseries of `.class` and `count` data where class has
#'   2 or more levels. The classwise counts are used to assess a time varying
#'   probability using spline terms.
#' @param deg the degree of the polynomial spline
#' @inheritDotParams splines::bs
#'
#' @return a timeseries of `.class` and `proportion` columns where the proportion
#'   is an estimate of the multinomial class probability at any given time.
#' @export
#'
#' @examples
#' if (FALSE) {
#'   utils::vignette("variant-proportions", package="growthrates")
#' }
estimate_multinomial_proportion = function(timeseries, deg=2, ... ) {

  timeseries %>% .check_timeseries()
  if (.is_multiple(timeseries, treat_classes_as_group = FALSE)) {
    return(.handle_multiple(timeseries, treat_classes_as_group = FALSE, deg=deg, ...))
  } else {

    if (!is.multinomial(timeseries)) stop("must be a multinomial time series")

    # remove zero count time points as these crash the algorithm
    tmp2 = timeseries %>% dplyr::group_by(.time) %>% dplyr::filter(sum(count)>0) %>% dplyr::ungroup()
    tmp2 = tmp2 %>% select(.date,.time,.class,count) %>% tidyr::pivot_wider(names_from = .class, values_from = count, values_fill = 0)

    response = tmp2 %>% dplyr::select(-tidyselect::any_of(c(".time",".date"))) %>% as.matrix()
    predictor = tmp2 %>% dplyr::pull(.time)

    model = nnet::multinom(class_count ~ splines::bs(.time,degree=deg, ...), data = tibble::tibble(class_count=response,.time=predictor))

    # https://cran.r-project.org/web/packages/survival/vignettes/splines.pdf

    new_time = .daily_times(timeseries$.time)

    times = tibble::tibble(.time = new_time)
    probs = dplyr::bind_cols(times,as.data.frame(stats::predict(model,newdata = times,type = "probs")))
    # probs %>% glimpse()
    plot_data = probs %>%
      tidyr::pivot_longer(cols = -.time, names_to = ".class", values_to = "probability") %>%
      as.timeseries() %>%
      .copy_metadata(timeseries)

    plot_data

  }
}
