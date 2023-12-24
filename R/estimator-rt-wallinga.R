
#' Wallinga-Teunis reproduction number
#'
#' Calculate a reproduction number estimate from growth rate using the Wallinga
#' 2007 estimation using empirical generation time distribution. This uses
#' resampling to transmit uncertainty in growth rate estimates
#'
#' @param df `r interfacer::idocument(rt_from_growth_rate, df)`
#' @param ip `r interfacer::idocument(rt_from_growth_rate, ip)`
#' @param bootstraps - the number of bootstraps to take to calculate for each point.
#'
#' @return `r i_reproduction_number`
#' @export
#' @examples
#' tmp = growthrates::england_covid %>%
#'   time_aggregate(count=sum(count))
#'
#'
#' if (FALSE) {
#'   # not run
#'   tmp2 = tmp %>%
#'     poisson_locfit_model() %>%
#'     rt_from_growth_rate()
#' }
#'
rt_from_growth_rate = function(df = i_growth_rate, ip = i_infectivity_profile, bootstraps = 20) {

  df = interfacer::ivalidate(df)
  ip = interfacer::ivalidate(ip)

  # e.g. converting a weekly to a daily growth rate
  # exp(r_wk) = exp(r_daily)^7
  # r_wk = r_daily*7
  .daily_unit = .step(df$time)

  ip_boots = ip %>% dplyr::n_groups()
  boots_per_ip = max(bootstraps %/% ip_boots,10)

  df = df %>% dplyr::mutate(
    rt = purrr::map2(growth.fit, growth.se.fit, .progress = TRUE, function(mean_r, sd_r) {

      mean_r = .daily_unit*mean_r
      sd_r = .daily_unit*sd_r

      # rather than bootstrap samples we sample quantiles of growth rate
      # to ensure a representative distribution
      # qnts = seq(0,1,length.out = boots_per_ip+3)[2:(boots_per_ip+2)]
      # r_samples = tibble::tibble(r = stats::qnorm(p=qnts,mean_r,sd_r)) %>%
      #   dplyr::mutate(r_i = dplyr::row_number())

      r_samples = tibble::tibble(
          r = stats::rnorm(boots_per_ip*ip_boots, mean_r, sd_r),
          boot = rep(unique(ip$boot),boots_per_ip)
        ) %>%
        dplyr::group_by(boot) %>%
        dplyr::mutate(r_i = dplyr::row_number())

      tmp = ip %>%
        dplyr::inner_join(r_samples, by="boot") %>%
        dplyr::rename(y = probability, a = time) %>%
        # get rid of a=0, y=0 if given.
        dplyr::filter(a > 0) %>%
        dplyr::group_by(boot,r_i) %>%
        dplyr::arrange(boot,r_i,a) %>%
        dplyr::summarise(
          R = r/sum(
            y
              *
            (exp(-r*dplyr::lag(a,default=0))-exp(-r*a))
              /
            (a - dplyr::lag(a,default=0))
          )
        )

      R_summ = tmp %>% dplyr::ungroup() %>% dplyr::summarise(
        rt.fit = mean(R, na.rm=TRUE),
        rt.se.fit = stats::sd(R, na.rm=TRUE),
        rt.0.025 = stats::quantile(R, probs=0.025, na.rm=TRUE),
        rt.0.5 = stats::quantile(R, probs=0.5, na.rm=TRUE),
        rt.0.975 = stats::quantile(R, probs=0.975, na.rm=TRUE),
      )
      return(R_summ)
    })
  )

  out = df %>% tidyr::unnest(rt)
  interfacer::ireturn(out, i_reproduction_number)

}







