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
#' ggplot(diamonds, aes(x=price))+
#'   geom_density()+
#'   scale_x_continuous(trans="log1p", breaks=ggrrr::breaks_log1p)
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
#' @description it perform logit scaling with right axis formatting. To not be used directly but with ggplot (e.g. scale_y_continuous(trans = "logit") )
#'
#' @return A scales object
#'
#' @examples
#'
#' library(ggplot2)
#' library(tibble)
#'
#' tibble(pvalue = c(0.001, 0.05, 0.1), fold_change = 1:3) %>%
#'  ggplot(aes(fold_change , pvalue)) +
#'  geom_point() +
#'  scale_y_continuous(trans = "logit")
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

scale_y_log1p = function(..., n=5, base=10) {
  return(ggplot2::scale_y_continuous(trans="log1p", breaks = breaks_log1p(n,base), ...))
}

scale_y_logit = function(...) {
  return(ggplot2::scale_y_continuous(trans="logit", ...))
}
