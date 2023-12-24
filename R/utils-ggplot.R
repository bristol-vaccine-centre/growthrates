## Custom scales ----

#' A scales breaks generator for log1p scales
#'
#' @param n the number of breaks
#' @param base the base for the breaks
#'
#' @return a function for ggplot scale breaks
#' @export
#'
#' @examples
#' library(tidyverse)
#' ggplot2::ggplot(diamonds, ggplot2::aes(x=price))+
#'   ggplot2::geom_density()+
#'   ggplot2::scale_x_continuous(trans="log1p", breaks=breaks_log1p())
breaks_log1p = function(n=5,base=10) {
  #scales::force_all(n, base)
  n_default = n
  function(x, n = n_default) {
    tmp = scales::breaks_log(n_default,base)(x+1,n)
    return(c(0,tmp[-1]))
  }
}

#' logit scale
#'
#' @description it perform logit scaling with right axis formatting. To not be used directly but with ggplot (e.g. ggplot2::scale_y_continuous(trans = "logit") )
#'
#' @return A scales object
#'
#' @examples
#'
#' library(ggplot2)
#' library(tibble)
#'
#' tibble::tibble(pvalue = c(0.001, 0.05, 0.1), fold_change = 1:3) %>%
#'  ggplot2::ggplot(aes(fold_change , pvalue)) +
#'  ggplot2::geom_point() +
#'  ggplot2::scale_y_continuous(trans = "logit")
#'
#' @export
logit_trans = function() {

  trans = stats::qlogis
  inv = stats::plogis

  scales::trans_new("logit",
                    transform = trans,
                    inverse = inv,
                    breaks = functional::Compose(trans, scales::extended_breaks(), inv),
                    format = scales::label_scientific(digits = 2)
  )
}

#' A log1p y scale
#'
#' @inheritParams ggplot2::scale_y_continuous
#' @param dp decimal points
#' @param base the base for the logarithm
#' @param n  the number of major breaks
#'
#' @return a ggplot scale
#' @export
scale_y_log1p = function(..., n=5, base=10, dp=0) {
  return(ggplot2::scale_y_continuous(trans="log1p", breaks = breaks_log1p(n,base), labels = ~ sprintf("%.*f",dp,.x), ...))
}

#' A logit y scale
#'
#' @inheritParams ggplot2::scale_y_continuous
#'
#' @return a ggplot scale
#' @export
scale_y_logit = function(...) {
  return(ggplot2::scale_y_continuous(trans="logit", ...))
}

# make a colour aesthetic apply to fill
.fill_col = function(mapping) {
  if (is.null(mapping$fill)) {
    mapping$fill = mapping$colour
    mapping$colour=NULL
  }
  return(mapping)
}

# the subset of ... params that apply to a geom
.flt = function(geom, dots, .default = list()) {
  dots = dots[names(dots) %in% geom$aesthetics()]
  dots = c(dots, .default[!names(.default) %in% names(dots)])
  return(dots)
}

# internal function: allow a ggplot to be constructed more dynamically
.layer = function(geom, data = NULL, mapping, ..., .default = list(), .switch_fill = inherits(geom,"GeomRibbon")) {
  dots = rlang::list2(...)
  if (.switch_fill) {
    mapping = .fill_col(mapping)
    dots$fill = dots$colour
    dots$colour = NULL
  }
  return(
    ggplot2::layer(
      geom = geom,
      stat = ggplot2::StatIdentity,
      data = data,
      mapping = mapping,
      position = dots$position %||% "identity",
      show.legend = dots$show.legend %||% TRUE,
      inherit.aes = dots$inherit.aes %||% FALSE,
      check.aes = dots$check.aes %||% TRUE,
      check.param = dots$check.param %||% TRUE,
      param = .flt(geom, dots, .default = .default)
    )
  )
}

# defaults

.growth_scale_limits = function() {
  return(getOption("growthrates.growth_scale_limit",default = c(-0.15,0.15)))
}
