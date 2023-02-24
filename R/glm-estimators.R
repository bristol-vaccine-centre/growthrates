# GLM simple spline models ----

.df_from_window = function(window, timeseries, classes=1) {
  tmp = ceiling(nrow(timeseries) / classes / window)
  if (tmp < 1 ) tmp = 1
  return(tmp)
}

.softmax <- function(par){
  n.par <- length(par)
  par1 <- sort(par, decreasing = TRUE)
  Lk <- par1[1]
  for (k in 1:(n.par-1)) {
    Lk <- max(par1[k+1], Lk) + log1p(exp(-abs(par1[k+1] - Lk)))
  }
  val <- exp(par - Lk)
  return(val)
}

.inv_logit = function(x) exp(x)/(1+exp(x))

#' Multinomial time-series model.
#'
#' Takes a list of times, classes and counts, e.g. a COGUK variant like data set
#' with time, (multinomial) class (e.g. variant) and count being the count in
#' that time period. Fits a quadratic B-spline on time to the proportion of the
#' data using `nnet::multinom`.
#'
#' @param d input dataframe with: `time` (as a `time_period`), `class`, `count` columns.
#'   This must be ungrouped (but can be combined with a `group_modify`)
#' @param ... not used and present to allow proportion model to be used in a group_modify
#' @param window a number of data points between knots, smaller values result in
#'   less smoothing, large value in more.
#' @param frequency the density of the output estimates.
#' @param predict result a prediction. If false we return the model.
#'
#' @return a new dataframe with `time` (as a time period), `class`, and `proportion.0.5`, or a model object
#' @export
multinomial_nnet_model = function(d, ..., window = 14, frequency = "1 day", predict = TRUE) { #, output_unit="1 day") {

  .exact_cols(d, "time", "class", "count")

  # remove zero count time points as these crash the algorithm
  tmp2 = d %>% dplyr::group_by(time) %>% dplyr::filter(sum(count)>0) %>% dplyr::ungroup()
  tmp2 = tmp2 %>% select(time,class,count) %>%
    tidyr::pivot_wider(names_from = class, values_from = count, values_fill = 0)

  response = tmp2 %>% dplyr::select(-tidyselect::any_of(c("time"))) %>% as.matrix()
  predictor = tmp2 %>% dplyr::pull(time)
  data = tibble::tibble(prob=response,time=predictor)

  output_times = full_seq.time_period(d$time, period = frequency)
  df = .df_from_window(window, timeseries = d, classes = ncol(response))
  model = nnet::multinom(prob ~ splines::ns(time, df = df), Hess = TRUE,data = data)


  if (!predict) return(model)
  new_data = tibble::tibble(time = output_times)

  # TODO: get the confidence intervals from the spline.
  # This will need to be done with resampling and a spaghetti plot.
  # browser()
  # Gets the basis model:
  # new_matrix = model.matrix(~ splines::ns(time, df = df), data=new_data)
  # betahat = t(rbind(0, coef(model))) # model coefficients, with explicit zero row added for reference category & transposed
  # # transform works rowwise: in another log link type thing it could just be applied I think.
  # preds = t(apply(new_matrix %*% betahat, MARGIN = 1,FUN=.softmax))
  # colnames(preds) = colnames(response)
  preds2 = stats::predict(model,newdata = new_data,type = "probs")

  probs = dplyr::bind_cols(new_data, as.data.frame(preds2))
  probs %>%
    tidyr::pivot_longer(cols = -time, names_to = "class", values_to = "proportion.0.5")

}


# tmp = summary(model)
# v = tmp$terms %>% attr("predvars")
# The call for the splines including the knots
# v[[3]]
# slpine coefficients & SE:
# tmp$coefficients
# tmp$standard.errors
# https://stats.stackexchange.com/questions/422068/how-to-predict-by-hand-in-r-using-splines-regression
# https://www.r-bloggers.com/2014/06/simultaneous-confidence-intervals-for-derivatives-of-splines-in-gams/



#' Binomial time-series model.
#'
#' @param d input dataframe with: `time` (as a `time_period`), `count`, and `denom` columns.
#'   This must be ungrouped (but can be combined with a `group_modify`)
#' @param ... not used and present to allow proportion model to be used ina group_modify
#' @param window a number of data points between knots, smaller values result in
#'   less smoothing, large value in more.
#' @param frequency the density of the output estimates.
#'
#' @return a new dataframe with `time` (as a time period), and `proportion.0.5` plus quantiles, or a model object
#'
#' @return a new dataframe with time (as a time period)
#' @export
proportion_glm_model = function(d, ..., window = 14, frequency = "1 day") { #, output_unit = "1 day") {

  .exact_cols(d,"time", "count", "denom")

  # see http://www.simonqueenborough.info/R/statistics/glm-binomial
  # for this next bit:
  y = cbind(d$count, d$denom - d$count)

  output_times = full_seq.time_period(d$time, period = frequency)
  df = .df_from_window(window,timeseries = d)

  if (df < 2) df=2
  model = glm(y ~ splines::ns(time, df = df), family = quasibinomial, data = d)
  new_data = tibble(time = output_times)
  # response is transformed:
  # proportion_estimate = predict(model, newdata = new_data, type="response")
  # this prediction in in the logit space:
  est2 = predict(model, newdata = new_data, se.fit = TRUE)
  # it is possible to get CIs out here but they are the logit transformed ones.
  new_data = new_data %>%
    .result_from_fit(type = "proportion", est2$fit, est2$se.fit, model$family$linkinv)
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
#'
#' @return a new dataframe with time (as a time period)
#' @export
poisson_glm_model = function(d, ..., window = 14, frequency = "1 day") {

  # TODO:
  # Extract the gradient from the spline.

  .exact_cols(d, "time", "count", .ignore = "denom")

  output_times = full_seq.time_period(d$time, period = frequency)
  # We normalise the spline degrees of freedom by data length
  # to stop extra wigglyness in shorter timeseries.
  df = .df_from_window(window,timeseries = d)

  model = glm(count ~ splines::ns(time, df = df), family = quasipoisson, data = d)
  new_data = tibble(time = output_times)
  # response is transformed:
  rate_estimate = predict(model, newdata = new_data, type="response")
  # this prediction in in the logit space:
  est2 = predict(model, newdata = new_data, se.fit = TRUE)
  # it is possible to get CIs out here but they are the logit transformed ones.

  new_data = new_data %>%
    .result_from_fit(type = "incidence", est2$fit, est2$se.fit, model$family$linkinv)
  return(new_data)
}
