

#' Calculate a normalised incidence rate per capita
#'
#' This assumes positive disease counts are stratified by a population grouping, e.g.
#' geography or age, and we have estimates of the size of that population during
#' that time period. Normalising by population size allows us to compare groups.
#'
#' @param pop `r interfacer::idocument(normalise_incidence, pop)`
#'   The population data must be grouped in the same way as `modelled`.
#' @param modelled `r interfacer::idocument(normalise_incidence, modelled)`
#'   Model output from processing the `raw` dataframe with something like
#'   `poission_locfit_model`
#' @param ... not used
#' @param population_unit what unit do you want the incidence in e.g. per 100K
#'
#' @return a dataframe with incidence rates per unit capita.
#'   `r i_incidence_per_capita_model`
#' @export
#' @concept models
#'
#' @examples
#' tmp = growthrates::england_covid %>%
#'   growthrates::poisson_locfit_model(window=21) %>%
#'   growthrates::normalise_incidence(growthrates::england_demographics) %>%
#'   dplyr::glimpse()
#'
normalise_incidence = function(
    modelled = i_incidence_model,
    pop = i_population_data,
    ...,
    population_unit = 100000
) {

  pop = interfacer::ivalidate(pop, ...)
  modelled = interfacer::ivalidate(modelled, ...)
  if (!all(dplyr::group_vars(pop) %in% dplyr::group_vars(modelled)))
    stop(
      "different column groupings in `modelled` and `pop` parameters.\n",
      "`modelled` must be compatible from the `pop` data"
    )

  if (interfacer::itest(pop, i_timeseries)) {

    # pop is given as a timeseries we need to impute values
    adj = pop %>% dplyr::reframe(
      population_fn = list(.loessfn(x = time, y=population))
    )
    modelled = modelled %>%
      dplyr::group_modify(function(d,g,...) {

        pop_fn = adj %>%
          dplyr::semi_join(g, by=colnames(g)) %>%
          dplyr::pull(population_fn) %>%
          `[[`(1)

        d %>% dplyr::mutate(population = pop_fn(time))
      }) %>%
      dplyr::mutate(population_unit = population_unit)
  } else {

    # pop is static.
    modelled = modelled %>%
      dplyr::inner_join(pop, by=dplyr::group_vars(pop)) %>%
      dplyr::group_by(dplyr::across(dplyr::all_of(dplyr::group_vars(pop))))
  }


  # interfacer::ireturn(
    modelled %>%
    dplyr::mutate(
      incidence.per_capita.0.025 = incidence.0.025 / population * population_unit,
      incidence.per_capita.0.5 = incidence.0.5 / population * population_unit,
      incidence.per_capita.0.975 = incidence.0.975 / population * population_unit,
      incidence.per_capita.fit = incidence.fit - log(population / population_unit),
      incidence.per_capita.se.fit = incidence.se.fit - log(population / population_unit),
      population_unit = population_unit
    )
  #  i_incidence_per_capita_model
  #)


}

# tmp = .loessfn(x=seq(0,5,0.01), y=seq(0,5,0.01)^2)
# tmp(xout=1:4)
.loessfn = function(x,y,window=14) {
  tofit = tibble::tibble(x=x,y=y)
  tmp = stats::loess(y~x, tofit, span = .nn_from_window(window,tofit))
  return(function(xout) {
    yout = stats::predict(tmp, tibble::tibble(x=xout))
    return(unname(yout))
  })
}
