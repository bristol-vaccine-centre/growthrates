# growthrates


<!-- badges: start -->
[![R-CMD-check](https://github.com/bristol-vaccine-centre/growthrates/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/bristol-vaccine-centre/growthrates/actions/workflows/R-CMD-check.yaml)
[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.7242762.svg)](https://doi.org/10.5281/zenodo.7242762)
[![growthrates status badge](https://bristol-vaccine-centre.r-universe.dev/badges/growthrates)](https://bristol-vaccine-centre.r-universe.dev)
<!-- badges: end -->


Simple statistical models and visualisations for calculating the 
incidence, proportion, exponential growth rate, and reproduction number of 
infectious disease case time series. This tool kit was largely developed during 
the COVID-19 pandemic.

## Installation

[Bristol Vaccine Centre r-universe](https://bristol-vaccine-centre.r-universe.dev/).
Installation from there is as follows:

``` r
options(repos = c(
  "bristol-vaccine-centre" = 'https://bristol-vaccine-centre.r-universe.dev/',
  CRAN = 'https://cloud.r-project.org'))

# Download and install growthrates in R
install.packages("growthrates")
```

You can install the development version of `growthrates` from 
[GitHub](https://github.com/bristol-vaccine-centre/growthrates) with:

``` r
# install.packages("devtools")
devtools::install_github("bristol-vaccine-centre/growthrates")
```
