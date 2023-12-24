## germany_covid definition ----

#' Weekly COVID-19 case counts by age group in Germany
#'
#' A dataset of the weekly count of covid cases by age group in Germany
#' downloaded from the Robert Koch Institute Survstat service, and formatted for
#' use in growth rates. A denominator is calculated which is the overall
#' positive count for all age groups. This data set can be used to calculate
#' group-wise incidence and absolute growth rates and group wise proportions and
#' relative growth rates.
#'
#' @usage data(germany_covid)
#'
#' @format
#' A dataframe containing the following columns:
#' - class (enum(`0–14`,`15–19`,`20–24`,`25–29`,`30–39`,`40–49`,`50–59`,`60–69`,`70–79`,`80+`,`Unknown`, .ordered=TRUE)) - the class column
#' - date (as.Date) - the date column
#' - count (integer) - the count column
#' - time (as.time_period) - the time column
#' - denom (integer) - the denom column
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 2070 rows and 6 columns
#'
#' @docType data
#' @keywords datasets
#' @name germany_covid
NULL

## germany_covid definition ends


## england_covid definition ----
## Generated code. remove this line to prevent manual changes being overwritten

#' Daily COVID-19 case counts by age group in England
#'
#' A dataset of the daily count of covid cases by age group in England
#' downloaded from the UKHSA coronavirus API, and formatted for
#' use in `growthrates`. A denominator is calculated which is the overall
#' positive count for all age groups. This data set can be used to calculate
#' group-wise incidence and absolute growth rates and group wise proportions and
#' relative growth rates.
#'
#' @usage data(england_covid)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (as.Date) - the date column
#' - class (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) - the class column
#' - count (numeric) - the count column
#' - denom (numeric) - the denom column
#' - time (as.time_period) - the time column
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 26790 rows and 5 columns
#'
#' @docType data
#' @keywords datasets
#' @name england_covid
NULL

## england_covid definition ends


## germany_demographics definition ----
## Generated code. remove this line to prevent manual changes being overwritten

#' Germany demographics
#'
#' Derived from the Robert Koch Survstat service by comparing counts and
#' incidence rates.
#'
#' @usage data(germany_demographics)
#'
#' @format
#' A dataframe containing the following columns:
#' - class (enum(`0–14`,`15–19`,`20–24`,`25–29`,`30–39`,`40–49`,`50–59`,`60–69`,`70–79`,`80+`, .ordered=TRUE)) - the class column
#' - population (integer) - the population column
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 10 rows and 2 columns
#'
#' @docType data
#' @keywords datasets
#' @name germany_demographics
NULL

## germany_demographics definition ends
## england_consensus_rt definition ----
## Generated code. remove this line to prevent manual changes being overwritten

#' The england_consensus_rt dataset
#'
#' @usage data(england_consensus_rt)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (as.Date) - the date column
#' - low (numeric) - the low column
#' - high (numeric) - the high column
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 113 rows and 3 columns
#'
#' @docType data
#' @keywords datasets
#' @name england_consensus_rt
NULL

## england_consensus_rt definition ends
## england_events definition ----
## Generated code. remove this line to prevent manual changes being overwritten

#' The england_events dataset
#'
#' @usage data(england_events)
#'
#' @format
#' A dataframe containing the following columns:
#' - label (character) - the label column
#' - start (date) - the start column
#' - end (date) - the end column
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 13 rows and 3 columns
#'
#' @docType data
#' @keywords datasets
#' @name england_events
NULL

## england_events definition ends
## england_covid_pcr_positivity definition ----
## Generated code. remove this line to prevent manual changes being overwritten

#' The england_covid_pcr_positivity dataset
#'
#' @usage data(england_covid_pcr_positivity)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (date) - the date column
#' - time (as.time_period) - the time column
#' - count (numeric) - the count column
#' - denom (numeric) - the denom column
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 1413 rows and 4 columns
#'
#' @docType data
#' @keywords datasets
#' @name england_covid_pcr_positivity
NULL

## england_covid_pcr_positivity definition ends
## england_consensus_growth_rate definition ----
## Generated code. remove this line to prevent manual changes being overwritten

#' The england_consensus_growth_rate dataset
#'
#' @usage data(england_consensus_growth_rate)
#'
#' @format
#' A dataframe containing the following columns:
#' - date (date) - the date column
#' - low (numeric) - the low column
#' - high (numeric) - the high column
#'
#' No mandatory groupings.
#'
#' No default value.
#'
#' 111 rows and 3 columns
#'
#' @docType data
#' @keywords datasets
#' @name england_consensus_growth_rate
NULL

## england_consensus_growth_rate definition ends
## england_demographics definition ----

#' England demographics
#'
#' Population counts by 5 year age group for England only from the 2021 census.
#'
#' @source https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021/census2021/census2021firstresultsenglandwales1.xlsx
#' @usage data(england_demographics)
#'
#' @format
#' A dataframe containing the following columns:
#' - class (enum(`00_04`,`05_09`,`10_14`,`15_19`,`20_24`,`25_29`,`30_34`,`35_39`,`40_44`,`45_49`,`50_54`,`55_59`,`60_64`,`65_69`,`70_74`,`75_79`,`80_84`,`85_89`,`90+`)) - the class column
#' - population (numeric) - the population column
#' - baseline_proportion (numeric) - the baseline_proportion column
#'
#' Must be grouped by: class (and other groupings allowed).
#'
#' No default value.
#'
#' 19 rows and 3 columns
#'
#' @docType data
#' @keywords datasets
#' @name england_demographics
NULL

## england_demographics definition ends
## england_variants definition ----
## Generated code. remove this line to prevent manual changes being overwritten

#' The england_variants dataset
#'
#' @usage data(england_variants)
#'
#' @format
#' A dataframe containing the following columns: 
#' - date (date) - the date column
#' - time (as.time_period) - the time column
#' - class (enum(`Other`,`Alpha (B.1.1.7)`,`Delta (B.1.617.2)`,`Delta (AY.4)`,`Omicron (Other)`,`Omicron (BA.2)`,`Omicron (BA.4)`,`Omicron (BA.5)`,`XBB (Other)`,`Kraken (XBB.1.5)`,`Arcturus (XBB.1.16)`,`Eris (EG.5.1)`)) - the class column
#' - who_class (enum(`Other`,`Alpha`,`Delta`,`Omicron`,`Kraken`,`Arcturus`,`Eris`)) - the who_class column
#' - count (numeric) - the count column
#' - denom (numeric) - the denom column
#' 
#' Must be grouped by: class (and other groupings allowed).
#' 
#' No default value.
#' 
#' 479 rows and 6 columns
#'
#' @docType data
#' @keywords datasets
#' @name england_variants
NULL

## england_variants definition ends
