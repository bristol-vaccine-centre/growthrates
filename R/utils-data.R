
## Utility ----



# return NA for errors
.opt = function(expr) tryCatch(expr,error=function(e) NA_real_)

# check colums are present in df
.has_cols = function(df, ...) {
  cols = unlist(rlang::list2(...))
  if (is.symbol(cols)) cols = rlang::as_label(cols)
  return(all(cols %in% colnames(df)))
}

# # check for additional columns and chuck a warning
# .exact_cols = function(df, ..., .ignore = NULL) {
#   if (!.has_cols(df,...)) stop("Missing column(s): ", paste(..., sep=", ", collapse=", "))
#   expected = c(unlist(c(...)),.ignore)
#   if (!all(colnames(df) %in% expected)) {
#     extra = setdiff(colnames(df), expected)
#     xcols = paste0(extra,collapse=", ")
#     rlang::warn(c("!"="Removing unsupported column(s): ","*" = xcols,"*" = "Did you mean to group beforehand?"), .frequency = "once", .frequency_id = xcols)
#     df = df %>% dplyr::select(tidyselect::any_of(expected)) # any of because of ignored columns.
#   }
#   invisible(NULL)
# }


.result_from_fit = function(new_data, type, fit, se.fit, inv = function(x) x) {

  .opt_inv = function(x) {
    purrr::map_dbl(x, ~ tryCatch(inv(.x) %>% ifelse(is.finite(.), ., NA_real_), error=function(e) NA_real_))
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

# preprocess_data = function(df, multinom, ...,  date_col = "date") {
#
#   grps = df %>% dplyr::groups()
#
#   # Is this a dated line list? i.e. a datafram with a date, or a time, but no count:
#   if (!.has_cols(df, "count")) {
#
#     if (.has_time(df) && !.has_cols(df, "date")) {
#       # need a date column to summarise
#       df = df %>% dplyr::mutate(date = as.Date(time))
#     } else {
#       dateVar = rlang::ensym(date_col)
#       df = df %>% dplyr::rename(date = !!dateVar)
#     }
#
#     # this does everything:
#     df = df %>% time_summarise(...)
#
#   } else {
#
#     # this is a time series already with a count column
#     # could check here it is complete and fill it?
#
#     # make sure there is a time column.
#     if (!.has_time(df)) {
#       dateVar = rlang::ensym(date_col)
#       df = df %>% dplyr::mutate(time = as.time_period(!!dateVar, ...))
#     }
#
#     if (.has_cols(df, "class") && !.has_cols(df, "denom")) {
#       df = df %>%
#         dplyr::group_by(!!!grps, time) %>%
#         dplyr::mutate(denom = sum(count)) %>%
#         dplyr::group_by(!!!grps)
#     }
#
#   }
#
#   if (!multinom) {df = df %>% dplyr::group_by(class, .add = TRUE)}
#
#   return(df)
# }


