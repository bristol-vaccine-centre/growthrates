
## Timed_df ----

# check for consistency
.tidy_timed_df = function(df, ...) {
  # date_col = .get_date_col(df)
  # class_col = .get_class_col(df)
  # time_col = .get_time_col(df)

  # inferring units makes sure that the .time column is a timed_df.
  # if this function is called with start_date or units then this will
  # rebase the time period. Otherwise if .time is  this is a no-op
  df = df %>% mutate(.time = .infer_units(.time, .date) %>% as.time_period(...))

  # make sure that the .date column is present and consistent with the .time
  df = df %>% mutate(.date = time_to_date(.time))

  # TODO: other checks?
  return(df)
}

#' Timed dataframes
#'
#' A timed dataframe is a dataframe with a time point (as a `time_period`
#' vector) associated with every entry. It will also have a date column (as a
#' normal Date object) once converted. Typically a `timed_df` is a line list of
#' observations and not a regular `timeseries`, however a regular `timeseries`
#' is logically also a `timed_df`. A `timed_df` linelist may have a class which
#' describes the subtype of the observations (related observations), for example
#' the class may be a binomial or multinomial feature. The dataframe may also be
#' grouped in which case the grouping defines multiple independent observations.
#'
#' @param df a dataframe containing either a time (as `time_period`, or numeric)
#'   or a date (as a Date) column.
#' @inheritDotParams as.time_period
#' @param time_col the column name of the time column. Defaults to
#'   "time" but will only be used if the column is present.
#' @param date_col the column name of a date column. Defaults to
#'   "date" but will only be used if the column is present.
#' @param class_col the column name of a class column if present. Defaults to
#'   "class" but will only be used if the column is present.
#'
#' @return a dateframe with a `time_col` column (as a time_period) and `date_col` as a Date.
#'   and optionally a `class_col`. Other columns are preserved.
#' @export
#'
#' @example inst/examples/timed-df-example.R
as.timed_df = function(df, ..., time_col = "time", date_col = "date", class_col = "class") {

  date_col = rlang::ensym(date_col)
  time_col = rlang::ensym(time_col)
  class_col = rlang::ensym(class_col)
  if (!(.has_cols(df,class_col) || .has_cols(df,".class"))) class_col = NULL

  # all the column names are already defined and embedded, but sometimes we need
  # to check everything is still consistent, and we haven;t accidentally lost any
  # of the columns.
  if (is.timed_df(df)) {
    return(.tidy_timed_df(df, ...))
  }

  # Sometimes the class is not yet assigned but there is a .time column
  # this is one minimum requirement.
  if ((.has_cols(df, ".time") && is.time_period(df$.time))) {
    class(df) = unique(c("timed_df",class(df)))
    return(.tidy_timed_df(df, ...))
  }

  # We are not dealing with a converted data here.
  if (!.has_cols(df,time_col) && !.has_cols(df,date_col)) stop("df must have a time or date column (as `",as_label(date_col),"`)")

  # we there was a .time and it is a time_period class we would have alredy left
  # this function so we need to create a .time column from the date and the ... params.
  if (!.has_cols(df,".time")) {
    df = df %>% mutate(.time = as.time_period(!!date_col, ...))
  }

  # we need to check there is a .date col and
  # if not try and create it from the .time col
  if (!.has_cols(df,".date")) {
    if (.has_cols(df,date_col)) {
      df = df %>% dplyr::mutate(.date = !!date_col)
    } else {
      df = df %>% dplyr::mutate(.date = time_to_date(.time))
    }
  }

  # any classes?
  if (!is.null(class_col)) {
    # if there is no .class but there is a class_col:
    if (!.has_cols(df,".class") && .has_cols(df,class_col)) {
      df = df %>% dplyr::mutate(.class = !!class_col)
    }
  }

  structure(df,
      time_col = time_col,
      date_col = date_col,
      class_col = class_col,
      class = c("timed_df", class(df)))
}

#' @describeIn as.timed_df Check if this is a timed_df
#' @export
is.timed_df = function(df) {
  return("timed_df" %in% class(df))
}

#' @describeIn as.timed_df Check is a multinomial timeseries or timed_df
#' @export
is.multinomial = function(df) {
  return(.has_cols(df, ".class"))
}

#' @describeIn as.timed_df Print a timed dataset
#' @export
print.timed_df = function(df, ...) {
  time_units = attributes(df$.time)
  df = .tidy_timed_df(df)

  if (!is.timeseries(df)) {
    if (dplyr::is.grouped_df(df)) grp_count = dplyr::n_groups(df) else grp_count = 1
    ts_cols = setdiff(colnames(df),c(".time",".date",".class",group_vars(df))) %>% paste0(collapse = ", ")
    cat(sprintf("%s time stamped data set(s) with observations: %s\n",grp_count, ts_cols))
  }
  if (is.multinomial(df)) {
    cat(sprintf("with %d classes: %s\n", length(unique(df$.class)), paste0(unique(as.character(df$.class)),collapse=", ")))
  }
  cat(sprintf("time unit: %s days, origin: %s\n",time_units$unit,time_units$start_date))
  cat(sprintf("running from: %s, until: %s\n",min_date(df$.date),max_date(df$.date)))
}




#' @inherit dplyr::group_by title description params return details
#' @importFrom dplyr group_by
#' @export
group_by.timed_df = function(.data, ..., .add = FALSE, .drop = dplyr::group_by_drop_default(.data)) {
  tmp = NextMethod(.data)
  tmp = tmp %>% .copy_metadata(.data)
  return(tmp)
}

#' Summarise data from a `timed_df` to a `timeseries`
#'
#' Summarises by group, class and time/date.
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
  if (!is.timed_df(timed_df) && !is.timeseries(timed_df)) stop("`timed_df` must be a timed_df or timeseries class")
  grps = .ts_groups(timed_df, time = TRUE)
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
    dplyr::group_by(!!!grps) %>%
    dplyr::summarise(!!!dots, .groups="drop_last") %>%
    dplyr::ungroup()
  if (length(fill) > 0) {
    tmp = tmp %>%
      tidyr::complete(!!!grps, fill = .fill)
  }
  tmp = tmp %>% .copy_metadata(timed_df)
  tmp = tmp %>% as.timeseries()

  return(tmp)
}

.set_metadata = function(source, date_col, class_col, time_col, classes) {
  structure(source,
    date_col = date_col,
    class_col = class_col,
    time_col = time_col,
    class = unique(c(classes,setdiff(class(source),c("timed_df","timeseries")))))
}

.get_date_col = function(source) {
  attr(source, "date_col")
}

.get_class_col = function(source) {
  attr(source, "class_col")
}

.get_time_col = function(source) {
  attr(source, "time_col")
}

.copy_metadata = function(target, source) {
  attr(target, "date_col") = attr(source, "date_col")
  attr(target, "class_col") = attr(source, "class_col")
  attr(target, "time_col") = attr(source, "time_col")
  class(target) = unique(c(intersect(class(source),c("timed_df","timeseries")),class(target)))
  return(target)
}

# get a set of groups which may or may not include time column.
.ts_groups = function(timed_df, time=TRUE, class=TRUE) {
  grps = timed_df %>% groups()
  if(time) grps = c(grps,rlang::sym(".time"))
  if(class && .has_cols(timed_df, ".class")) grps = unique(c(grps,rlang::sym(".class")))
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
  grps = timeseries %>% groups()
  tmp = timeseries %>%
    dplyr::group_by(!!!grps) %>%
    # this next line changes a dataframe with single (time,class,count=n) row to one
    # where (time,class,count=1)*n rows:
    dplyr::group_modify(function(d,g,..) {
      if (nrow(d) > 1) stop("timed_df had some duplicate rows in it (probably for time)")
      return(tibble::tibble(count = rep(1,d$count)))
    }) %>%
    dplyr::select(-count) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(date = .time_to_date(time))
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
  orig_grps = df %>% groups()

  if (is.timeseries(df)) return(.tidy_timed_df(df, ...))
  if (!is.timed_df(df)) df = as.timed_df(df, ...)

  if (any(is.na(df$.time))) stop("there are miossing dates in the timeseries")
  # if (dplyr::is.grouped_df(df)) {
  grps = .ts_groups(df,time = TRUE)

  # check uniqueness of group and class
  tmp = df %>% dplyr::group_by(!!!grps) %>% dplyr::count()

  if (any(tmp$n > 1)) stop("duplicate entries for individual times in series, did you mean to summarise first?")
  df = df %>% dplyr::group_by(!!!orig_grps)

  class(df) = unique(c("timeseries", class(df)))
  return(df)
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
  tmp = tmp %>% .copy_metadata(.data)
  return(tmp)
}

#' @describeIn as.timeseries Print a timeseries dataset
#' @export
print.timeseries = function(df, ...) {
  if (dplyr::is.grouped_df(df)) grp_count = dplyr::n_groups(df) else grp_count = 1
  ts_cols = setdiff(colnames(df),c(".time",".date",".class",group_vars(df))) %>% paste0(collapse = ", ")
  cat(sprintf("%s regular timeseries with observations: %s\n",grp_count, ts_cols))
  NextMethod(df, ...)
}

#' Restore original format to a timed_df
#'
#' @param timed_df the data frame to restore
#'
#' @return a copy of the dataframe with groups expanded and class renamed
#' @export
restore = function(timed_df) {
  date_col = .get_date_col(timed_df)
  class_col = .get_class_col(timed_df)
  time_col = .get_time_col(timed_df)
  if (!is.null(class_col)) {
    timed_df = timed_df %>% dplyr::rename(!!class_col := .class)
  }
  timed_df = timed_df %>% dplyr::rename(
    !!time_col := .time,
    !!date_col := .date
  ) %>%
  magrittr::set_class(setdiff(class(timed_df),c("timed_df","timeseries")))
  return(timed_df)
}

## Validity checking (internal) ----


# .maybe_symbol_list = function(...) {
#   ensyms(...)
#   browser()
# }


.check_timed_df = function(timed_df, ...) {
  cols = unlist(rlang::list2(...))
  if (!is.timed_df(timed_df)) stop("Need a timed_df as input (see as.timed_df(...))")
  if (!is.time_period(timeseries$.time)) stop("timed_df must have a time column as a time_period class")
  if (!.has_cols(timed_df,...)) stop("timed_df must have ",paste0("\"",cols,"\"",collapse = ";")," columns")
}

.check_timeseries = function(timeseries, ...) {
  cols = unlist(rlang::list2(...))
  if (!is.timeseries(timeseries)) stop("Need a timeseries as input (see as.timeseries(...))")
  if (!is.time_period(timeseries$.time)) stop("timeseries must have a time column as a time_period class")
  if (!.has_cols(timeseries,...)) stop("timeseries must have ",paste0("\"",cols,"\"",collapse = ";")," columns")
}



## Grouped timeseries ----

.is_multiple = function(df, treat_classes_as_group = FALSE) {
  return(
    is.grouped_df(df) || (treat_classes_as_group && is.multinomial(df))
  )
}

.handle_multiple = function(df, treat_classes_as_group = FALSE, ..., this_function = rlang::caller_fn()) {

  if (!.is_multiple(df,treat_classes_as_group)) stop("Tried to handle a single dataset as multiple. Did you mean to check .is_multiple() first.")
  grps = .ts_groups(df, FALSE, treat_classes_as_group)
  cls = intersect(class(df), c("timed_df","timeseries"))
  df %>% group_by(!!!grps) %>%
    dplyr::group_modify(function(d,g,...) {
      class(d) = c(cls,class(d))
      this_function(d,...)
    }, ...) %>%
    .copy_metadata(df) %>%
    as.timeseries()

}
