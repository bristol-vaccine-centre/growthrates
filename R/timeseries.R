## Utility ----

#' Check whether vector is a date
#'
#' @param x a vector to check
#'
#' @return TRUE if dates, FALSE otherwise
#' @export
#'
#' @examples
#' is.Date(Sys.Date())
is.Date = function(x) {
  class(x)=="Date"
}

## Time_period ----

#' Convert numbers to a time period class
#'
#' Time periods are just a zero based numeric representation of dates
#' with a time unit baked in. This allows days or weeks, or fractional days
#' to be represented
#'
#' @param times a vector of numbers (may be integer or real) or a time_period
#' @param days_in_period the number of days in one unit of time. If times is a
#'   time_period, and the number of days is different then this will recalibrate
#'   the time_period to use the new days in period
#' @param start_date the zero time date. If the input is already a time_period and
#'   this is different to its start_date then it will be recalibrated.
#'
#' @return a `time_period` class
#' @export
#'
#' @example inst/examples/time-period-example.R
as.time_period = function(times, days_in_period = NULL, start_date = NULL) {

  if (is.time_period(times)) {
    orig_days_in_period = attributes(times)$days_in_period
    orig_start_date = attributes(times)$start_date
    if (is.null(start_date)) start_date = orig_start_date
    if (is.null(days_in_period)) days_in_period = orig_days_in_period
    start_date = as.Date(start_date)
    if (orig_days_in_period != days_in_period || orig_start_date != start_date) {
      # time period needs conversion from one periodicity to another
      dates = .time_to_date(times)
      new_times = .date_to_time(dates, days_in_period, start_date)
      return(new_times)
    } else {
      return(times)
    }
  } else {
    if (is.null(days_in_period) | is.null(start_date)) stop("must define start_date and days_in_period")
    start_date = as.Date(start_date)
    times = as.numeric(times)
    return(structure(
      times,
      start_date = start_date,
      days_in_period = days_in_period,
      class = c("time_period",class(times))
    ))
  }
}


#' Convert time period to dates
#'
#' @param x a time_period
#' @param ... not used
#'
#' @return a vector of dates
#' @export
as.Date.time_period = function(x, ...) {
  return(.time_to_date(x))
}

#' @describeIn as.Date.time_period Convert to a vector of POSIXct
#' @export
as.POSIXct.time_period = function(x,...) {
  days_in_period = attributes(x)$days_in_period
  start_date = attributes(x)$start_date
  return(as.POSIXct.numeric(x*days_in_period*24*60*60, origin=start_date))
}

#' @describeIn as.time_period Check is a time_period
#' @export
is.time_period = function(times) {
  return("time_period" %in% class(times))
}

# create a new time_period on the same scale as the old one but
# as an ordered sequence of one day intervals
.daily_times = function(times) {
  days_in_period = attributes(times)$days_in_period
  start_date = attributes(times)$start_date
  as.time_period(
    seq(min(times,na.rm = TRUE),max(times,na.rm = TRUE),by = 1/days_in_period),
    days_in_period,
    start_date
  )
}

# convert a time period to normal Date
.time_to_date = function(times)  {
  if (!is.time_period(times)) stop("times must be a time_period class")
  days_in_period = attributes(times)$days_in_period
  start_date = attributes(times)$start_date
  if (is.null(days_in_period) | is.null(start_date)) stop("must define start_date and days_in_period")
  times*days_in_period+start_date
}

# creates a normal Date from a time period minimally requiring the time unit.
.date_to_time = function(dates, days_in_period, start_date = min(dates,na.rm=TRUE)) {
  as.time_period(as.numeric(dates-start_date)/days_in_period, days_in_period, start_date)
}

## Timed_df ----

#' Timed dataframes
#'
#' A timed dataframe is a dataframe with a time point (as a time_period vector)
#' associated with every entry. It will also have a date column (as a normal
#' Date object) once converted. This is a line list with a date not a time series.
#'
#' @param df a dataframe containing either a time (as time_period, or numeric)
#'   or a date (as a Date) column. This may be grouped in which case the grouping
#'   defines individual timeseries, which are generally treated as independent.
#' @param days_in_period the number of days in one time unit. May be non
#'   integer. If the time column in the dataframe is already a time_period then
#'   this is optional, but if provided will recalibrate the time_period to a new
#'   interval. Otherwise it is required
#' @param start_date the zero day. If the time column in the dataframe is
#'   already a time_period then this is optional, but if provided will
#'   recalibrate the time_period to a new interval. If the dataframe has a
#'   date column then this is optional and the zero day is assumed to be the
#'   start of the timeseries.
#' @param class_col the column name of a class column if present. Defaults to
#'   "class" but will only be used if the column is present. The class column
#'   may be a binomial or multinomial class marker.
#' @param grp_label a glue specification to apply to the groups of the `df`
#'   dataframe to create a unique label for the groups.
#'
#' @return a dateframe with a time column (as a time_period) and date as a Date.
#'   if originally grouped it will have a `group` column and if `class_col` was#
#'   defined a `class` column. Other columns are preserved.
#' @export
#'
#' @example inst/examples/timed-df-example.R
as.timed_df = function(df, days_in_period=NULL, start_date=NULL, class_col = "class", grp_label = "Group {.grp_id}") {

  # TODO: name collisions.

  class_col = tryCatch(rlang::ensym(class_col), error = function(e) NULL)

  if (is.timed_df(df)) return(df)

  if (!.has_cols(df,"time") && !.has_cols(df,"date")) stop("df must have a time or date column")

  # See whether df has the information we need
  if (suppressWarnings(is.time_period(df$time))) {
    if (is.null(days_in_period)) days_in_period = attributes(df$time)$days_in_period
    if (is.null(start_date)) start_date = attributes(df$time)$start_date
  } else if (suppressWarnings(is.Date(df$date))) {
    if (is.null(start_date)) start_date = as.Date(min(df$date, na.rm=TRUE),"1970-01-01")
    # this default could be inferred from dates
    if (is.null(days_in_period)) days_in_period = 1
  }

  if (is.null(days_in_period) || is.null(start_date))
    stop("Must define start_date and days_in_period.")

  if("time" %in% colnames(df)) {
    df = df %>%
      dplyr::mutate(time = as.time_period(time,days_in_period,start_date)) %>%
      dplyr::mutate(date = .time_to_date(time))
  } else if ("date" %in% colnames(df)) {

    if (is.null(start_date)) start_date = as.Date(min(df$date, na.rm=TRUE),"1970-01-01")
    df = df %>% dplyr::mutate(time = .date_to_time(date, days_in_period, as.Date(start_date)))
  } else {
    stop("df must contain a time (numeric), or date (Date) column")
  }

  # any classes?
  if (!is.null(attr(df,"class_col")) ) {
    if (.has_cols(df,"class")) class_col = attr(df,"class_col")
  } else {
    if (.has_cols(df, rlang::as_label(class_col))) {
      message("classes defined: class=", rlang::as_label(class_col))
      df = df %>% dplyr::rename(class = !!class_col)
    } else {
      class_col = NULL
    }
  }

  # Deal with grouping:
  if (!is.null(attr(df,"grouping"))) {
    grouping = attr(df,"grouping")
    grps = list(rlang::sym("group"))
  } else if (dplyr::is.grouped_df(df)) {
    grps = df %>% dplyr::groups()
    message("groups defined (", paste0(df%>%dplyr::group_vars(),collapse=","), "): group=`",grp_label,"`")
    df = df %>%
      dplyr::mutate(
        .grp_id = dplyr::cur_group_id(),
        group = glue::glue(grp_label)
      ) %>%
      dplyr::select(-.grp_id)

    grouping = df %>% dplyr::group_data() %>%
      dplyr::mutate(
        .grp_id = dplyr::row_number(),
        group = glue::glue(grp_label)
      ) %>%
      dplyr::select(-.rows) %>%
      dplyr::select(-.grp_id)
    if (any(duplicated(grouping$group))) stop("grp_label does not produce a unique set of labels for the grouping.")

    df = df %>% dplyr::ungroup() %>% dplyr::select(-c(!!!grps))
  } else {
    grouping = NULL
  }

  # observations = colnames(df %>% dplyr::select(-any_of(c("group","class","time","date"))))
  # if (length(observations)>0) message("observations in data set: ",paste0(observations,collapse=", "))

  structure(df,
      grouping = grouping,
      class_col = class_col,
      class = c("timed_df", class(df)))
}

#' @describeIn as.timed_df Check if this is a timed_df
#' @export
is.timed_df = function(df) {
  return("timed_df" %in% class(df))
}


#' @inherit dplyr::group_by title description params return details
#' @importFrom dplyr group_by
#' @export
group_by.timed_df = function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  tmp = NextMethod(.data)
  class(tmp) = c("timed_df",class(tmp))
  return(tmp)
}

#' Summarise data from a `timed_df` to a `timeseries`
#'
#' @param timed_df a line list of data you want to summarise by the time column.
#'   If this is grouped the grouping is preserved and converted into a `class`
#'   column in the output. Ff the timed_df has a `class` column that is used in
#'   preference.
#' @param ... a spec for a dplyr::summary(...) - optional, and if not provided a
#'   `count = dplyr::n()` or a `count = sum(count)` is performed.
#' @param .fill a list similar to tidyr::complete for values to fill
#'   variables with
#'
#' @return a timeseries dataframe, by time
#' @export
#'
#' @example inst/examples/timed-df-example.R
time_summarise = function(timed_df, ..., .fill = list()) {
  if (!is.timed_df(timed_df) && !is.timeseries(timed_df)) stop("must be a timed_df or timeseries class")
  grps = .ts_groups(timed_df, time = FALSE)
  dots = dplyr::enexprs(...)
  if (length(dots)==0) {
    if (.has_cols(timed_df,"count")) {
      dots=list(count=rlang::expr(sum(count)))
    } else {
      dots=list(count=rlang::expr(dplyr::n()))
    }
    .fill = list(count=0)
  }
  tmp = timed_df %>%
    dplyr::group_by(!!!grps,time) %>%
    dplyr::summarise(!!!dots, .groups="drop_last") %>%
    dplyr::ungroup()
  if (length(fill) > 0) {
    tmp = tmp %>%
      tidyr::complete(!!!grps,time,fill = .fill)
  }
  tmp = tmp %>% .copy_metadata(timed_df)
  tmp = tmp %>% as.timeseries()

  return(tmp)
}

.copy_metadata = function(target, source) {
  attr(target, "class_col") = attr(source, "class_col")
  attr(target, "grouping") = attr(source, "grouping")
  # observations = colnames(target %>% dplyr::select(-any_of(c("group","class","time","date"))))
  # attr(target, "observations") = observations
  return(target)
}

.ts_groups = function(timed_df, time=TRUE) {
  grps = list()
  if(time) grps = c(grps,rlang::sym("time"))
  if(.has_cols(timed_df, "class")) grps = c(rlang::sym("class"),grps)
  if(.has_cols(timed_df, "group")) grps = c(rlang::sym("group"),grps)
  return(grps)
}

#' Decompose a timeseries with counts to timed data at individual level
#'
#' @param timeseries a timeseries with a count column
#' @param ... not used for supporting group_modify
#'
#' @return a timed_df with a single entry for every value of count in timeseries
#' @export
time_decompose = function(timeseries, ...) {
  .check_timeseries(timeseries,"count")
  grps = .ts_groups(timeseries)
  tmp = timeseries %>%
    dplyr::group_by(!!!grps) %>%
    # this next line changes a dataframe with single (time,class,count=n) row to one
    # where (time,class,count=1)*n rows:
    dplyr::group_modify(function(d,g,..) {
      if (nrow(d) > 1) stop("timed_df had some duplicate rows in it (probably for time)")
      return(tibble::tibble(count = rep(1,d$count)))
    }) %>%
    dplyr::select(-count) %>%
    dplyr::ungroup()
  tmp %>% .copy_metadata(timeseries) %>%
    magrittr::set_class(c("timed_df",setdiff(class(timeseries),"timeseries")))
}

## Timeseries ----

#' A time-series is a regular sequence of times with other data
#'
#' The time series is expected to have at least one of a `time` or `date`
#' columns and any other set of data columns. There can be multiple time-series
#' within this if the `class` column is present or the input is grouped. For
#' every `class` and `time`/`date` combination there can be only one entry.
#' Missing values are not filled in. If there is grouping (but no `class`
#' column) then it is temporarily stored and replaced with an integer `class`
#' column. This can be restored with `restore_grouping()` later
#'
#' @param df a dataframe with `time` or `date` and optionally `class` or class
#'   defined by grouping
#' @inheritDotParams as.timed_df
#'
#' @return a timeseries object with groupwise unique time and date columns
#' @export
#'
#' @example inst/examples/timeseries-example.R
as.timeseries = function(df, ...) {
  if (is.timeseries(df)) return(df)
  if (!is.timed_df(df)) df = as.timed_df(df, ...)

  # if (dplyr::is.grouped_df(df)) {
  grps = .ts_groups(df)
  classes = c("timeseries", class(df))
  # we add

  # check uniqueness of group and class
  tmp = df %>% dplyr::group_by(!!!grps) %>% dplyr::count()
  if (any(tmp$n > 1)) stop("duplicate entries for individual times in series, did you mean to summarise first?")
  df = df %>% dplyr::ungroup()

  return(structure(
    df,
    class = classes
  ))
}

#' @describeIn as.timeseries Check is a timeseries
#' @export
is.timeseries = function(df) {
  return("timeseries" %in% class(df))
}

#' @inherit dplyr::group_by title description params return details
#' @export
group_by.timeseries = function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  tmp = NextMethod(.data)
  class(tmp) = c("timeseries",class(tmp))
  return(tmp)
}

#' @describeIn as.timeseries Check is a mulinomial timeseries
#' @export
is.multinomial_ts = function(df) {
  if (!is.timeseries(df)) return(FALSE)
  return("class" %in% colnames(df))
}


#' Restore original format to a timed_df
#' #'
#' @param timed_df the data frame to restore
#'
#' @return a copy of the dataframe with groups expanded and class renamed
#' @export
restore_groups = function(timed_df) {
  grouping = attributes(timed_df)$grouping
  class_col = attributes(timed_df)$class_col
  if (!is.null(class_col)) {
    timed_df = timed_df %>% dplyr::rename(!!class_col:=class)
  }
  if (is.null(grouping)) return(timed_df)
  return(
    grouping %>% dplyr::full_join(timed_df, by="group") %>%
      dplyr::select(-group) %>%
      magrittr::set_class(setdiff(class(timed_df),c("timed_df","timeseries")))
  )
}

## Validity checking (internal) ----

.has_cols = function(df, ...) {
  cols = unlist(rlang::list2(...))
  return(all(cols %in% colnames(df)))
}

.check_timed_df = function(timed_df, ...) {
  cols = unlist(rlang::list2(...))
  if (!is.timed_df(timed_df)) stop("Need a timed_df as input (see as.timed_df(...))")
  if (!is.time_period(timeseries$time)) stop("timed_df must have a time column as a time_period class")
  if (!.has_cols(timed_df,...)) stop("timed_df must have ",paste0("\"",cols,"\"",collapse = ";")," columns")
}

.check_timeseries = function(timeseries, ...) {
  cols = unlist(rlang::list2(...))
  if (!is.timeseries(timeseries)) stop("Need a timeseries as input (see as.timeseries(...))")
  if (!is.time_period(timeseries$time)) stop("timeseries must have a time column as a time_period class")
  if (!.has_cols(timeseries,...)) stop("timeseries must have ",paste0("\"",cols,"\"",collapse = ";")," columns")
}
