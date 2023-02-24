
## Utility ----



# return NA for errors
.opt = function(expr) tryCatch(expr,error=function(e) NA_real_)

# check colums are present in df
.has_cols = function(df, ...) {
  cols = unlist(rlang::list2(...))
  if (is.symbol(cols)) cols = rlang::as_label(cols)
  return(all(cols %in% colnames(df)))
}

# check for additional columns and chuck a warning
.exact_cols = function(df, ..., .ignore = NULL) {
  if (!.has_cols(df,...)) stop("Missing column(s): ", paste(..., sep=", ", collapse=", "))
  expected = c(unlist(c(...)),.ignore)
  if (!all(colnames(df) %in% expected)) {
    extra = setdiff(colnames(df), expected)
    xcols = paste0(extra,collapse=", ")
    rlang::warn(c("!"="Removing unsupported column(s): ","*" = xcols,"*" = "Did you mean to group beforehand?"), .frequency = "once", .frequency_id = xcols)
    df = df %>% dplyr::select(tidyselect::any_of(expected)) # any of because of ignored columns.
  }
  invisible(NULL)
}


#' Summarise data from a line list to a time-series of counts.
#'
#' Summarises by group, class and time/date.
#'
#' @param df a line list of data you want to summarise, optionally grouped.
#'   If this is grouped then each group is treated independently. The remaining
#'   columns must contain a `date` column and may contain a `class` column.
#' @inheritParams cut_date
#' @param rectangular should the resulting time series be the same length for
#'   all groups. This is only the case if you can be sure that all data has been
#'   provided from all otherwise missing data will be treated as zero counts.
#' @param ... a spec for a dplyr::summary(...) - optional, and if not provided a
#'   `count = dplyr::n()` or a `count = sum(count)` is performed.
#' @param .fill a list similar to tidyr::complete for values to fill
#'   variables with
#'
#' @return a timeseries dataframe grouped by time period,
#' @export
time_summarise = function(df, unit, anchor = "start", rectangular = FALSE, ..., .fill = list(count = 0)) {

  # TODO: a line list with no data at the end is either missing data or there were no observations.
  # A date at which the observations are complete up to may be needed for each
  # group.

  if (!.has_cols(df,"date")) stop("a date column must be present")
  if (.has_cols(df,"time")) warning("time_summarise ignores existing time_period columns")

  has_class = .has_cols(df,"class")
  grps = df %>% dplyr::groups()

  df = df %>% dplyr::mutate(time = cut_date(date, unit=  unit, anchor = anchor, output="time_period"))
  if (has_class) df = df %>% dplyr::group_by(class, .add = TRUE)
  df = df %>% dplyr::group_by(time, .add = TRUE)

  dots = dplyr::enexprs(...)

  if (length(dots)==0) {
    if (.has_cols(df,"count")) {
      dots=list(count=rlang::expr(sum(count, na.rm=TRUE)))
    } else {
      dots=list(count=rlang::expr(dplyr::n()))
    }
    .fill = list(count=0)
  }

  # when rectangular the full sequence is for all the obseverations
  times = full_seq.time_period(df$time)

  # df grouped by grps, class(?), and time at this point
  tmp = df %>%
    dplyr::summarise(!!!dots, .groups="drop_last") %>%
    dplyr::group_by(!!!grps) %>%
    dplyr::group_modify(function(d,g,...) {
      if (!rectangular) times = full_seq.time_period(d$time)
      if (has_class) {
        dtmp = d %>% tidyr::complete(class, time = times, fill = .fill)
        if (!.has_cols(d, "denom") & .has_cols(d,"count")) {
          # calculate a classwise denominator if class and count are present and no denom column
          # created by the summarise.
          dtmp = dtmp %>%
            dplyr::group_by(time) %>%
            dplyr::mutate(denom = sum(count, na.rm = TRUE)) %>%
            dplyr::ungroup()
        }
      } else {
        dtmp = d %>% tidyr::complete(time = times, fill = .fill)
      }

    })


  return(tmp %>% dplyr::ungroup())
}


.result_from_fit = function(new_data, type, fit, se.fit, inv = function(x) x) {
  .opt_inv = function(x) {
    tryCatch(
      inv(x),
      error = function(e) rep(NA,length(x))
    )
  }

  return(new_data %>% dplyr::mutate(
    !!(paste0(type,".fit")) := unname(fit),
    !!(paste0(type,".se.fit")) := unname(se.fit),
    !!(paste0(type,".0.025")) := .opt_inv(stats::qnorm(0.025, fit, se.fit)),
    !!(paste0(type,".0.5")) := .opt_inv(fit),
    !!(paste0(type,".0.975")) := .opt_inv(stats::qnorm(0.975, fit, se.fit))
  ))
}

.has_time = function(df) {
  return(.has_cols(df, "time") && is.time_period(df$time))
}

preprocess_data = function(df, multinom, ...,  date_col = "date") {

  grps = df %>% dplyr::groups()

  # Is this a dated line list? i.e. a datafram with a date, or a time, but no count:
  if (!.has_cols(df, "count")) {

    if (.has_time(df) && !.has_cols(df, "date")) {
      # need a date column to summarise
      df = df %>% dplyr::mutate(date = as.Date(time))
    } else {
      dateVar = rlang::ensym(date_col)
      df = df %>% dplyr::rename(date = !!dateVar)
    }

    # this does everything:
    df = df %>% time_summarise(...)

  } else {

    # this is a time series already with a count column
    # could check here it is complete and fill it?

    # make sure there is a time column.
    if (!.has_time(df)) {
      dateVar = rlang::ensym(date_col)
      df = df %>% dplyr::mutate(time = as.time_period(!!dateVar, ...))
    }

    if (.has_cols(df, "class") && !.has_cols(df, "denom")) {
      df = df %>%
        dplyr::group_by(!!!grps, time) %>%
        dplyr::mutate(denom = sum(count)) %>%
        dplyr::group_by(!!!grps)
    }

  }

  if (!multinom) {df = df %>% dplyr::group_by(class, .add = TRUE)}

  return(df)
}


