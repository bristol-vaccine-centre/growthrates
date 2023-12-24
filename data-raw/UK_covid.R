

england_cases = readr::read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDateAgeDemographics&format=csv")


## Population
tmp = tempfile()
download.file("https://www.ons.gov.uk/file?uri=/peoplepopulationandcommunity/populationandmigration/populationestimates/datasets/populationandhouseholdestimatesenglandandwalescensus2021/census2021/census2021firstresultsenglandwales1.xlsx",tmp)
UK_pop = readxl::read_xlsx(tmp, range="A8:V383", sheet = "P02" )
UK_pop = UK_pop %>% tidyr::pivot_longer(cols=dplyr::starts_with("Aged"), names_to = "Age")

england_demographics = UK_pop %>%
  # england only
  dplyr::filter(`Area code [note 2]` == "E92000001") %>%
  dplyr::mutate(Age = Age %>% stringr::str_remove(stringr::fixed("\r\n[note 12]"))) %>%
  dplyr::transmute(
  class = dplyr::case_when(
    Age == "Aged 4 years and under" ~ "00_04",
    Age == "Aged 90 years and over" ~ "90+",
    Age == "Aged 5 to 9 years" ~ "05_09",
    TRUE ~ stringr::str_replace_all(Age,"^Aged ([0-9]+) to ([0-9]+) years$", "\\1_\\2")
  ) %>% forcats::as_factor(),
  population = value,
  baseline_proportion = population/sum(population),
) %>% dplyr::group_by(class)

# Trust LTLA probabilistic mappings
# https://epiforecasts.io/covid19.nhs.data/reference/load_mapping.html

if(interactive()) interfacer::use_dataframe(england_demographics)

england_covid = england_cases %>%
  dplyr::filter(age %in% levels(england_demographics$class)) %>%
  dplyr::transmute(
    date = date,
    class = factor(age, levels(england_demographics$class)),
    count = cases
  ) %>% # dplyr::inner_join(england_demographics, by="class") %>%
  dplyr::group_by(date) %>%
  dplyr::mutate(
    denom = sum(count)
  ) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(
    time = date_to_time(date,start_date = "2020-01-30")
  ) %>%
  dplyr::group_by(class)


if(interactive()) interfacer::use_dataframe(england_covid)

## England testing effort

england_cases_tests = readr::read_csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesPCROnlyBySpecimenDate&metric=newPCRTestsBySpecimenDate&format=csv")

england_covid_pcr_positivity = england_cases_tests %>%
  dplyr::transmute(
    date = date,
    time = as.time_period(date,unit="1 day"),
    count = newCasesPCROnlyBySpecimenDate,
    denom = newPCRTestsBySpecimenDate
  ) %>%
  dplyr::filter(!is.na(count) & !is.na(denom))

if(interactive()) interfacer::use_dataframe(england_covid_pcr_positivity)

# devtools::load_all()
# tmp = growthrates::england_covid %>%
# dplyr::group_modify(growthrates::poisson_locfit_model, window=21) %>%
# glimpse()
# plot_incidence(tmp)+facet_wrap(~class)+scale_y_log1p()
# tmp = growthrates::england_covid %>%
# dplyr::group_modify(growthrates::poisson_locfit_model, window=21) %>%
# glimpse()
# plot_incidence(tmp)+facet_wrap(~class)+scale_y_log1p()
# tmp2 = growthrates::england_covid %>%
# dplyr::group_modify(growthrates::proportion_locfit_model, window=21) %>%
# glimpse()
# tmp2 %>% view()
# plot_proportion(tmp2)
# tmp2$time
# devtools::load_all()
# plot_proportion(tmp2)
# devtools::load_all()
# plot_proportion(tmp2)
# plot_growth_rate(tmp2)
# plot_growth_rate(tmp2)+facet_wrap(~class)+coord_cartesian(ylim=c(-0.15,0.15))
# plot_growth_phase(tmp2)+facet_wrap(~class)+coord_cartesian(xlim=c(-0.15,0.15))
# plot_growth_phase(tmp2)+facet_wrap(~class)+coord_cartesian(xlim=c(-0.1,0.1))
# plot_growth_phase(tmp2)+facet_wrap(~class)+coord_cartesian(xlim=c(-0.075,0.075))
# plot_multinomial(tmp2,normalise=TRUE)
# plot_multinomial(tmp2,normalise=TRUE)+scale_fill_viridis_d()
# plot_multinomial(tmp2,normalise=TRUE,colour=NULL)+scale_fill_viridis_d()
# plot_multinomial(tmp2,normalise=TRUE,colour=NA)+scale_fill_viridis_d()

tmp = tempfile()
download.file("https://assets.publishing.service.gov.uk/media/63a46e3ed3bf7f375c7d833d/221221_R_and_growth_rate_time_series_for_publication_v1.0.ods",tmp)
consensus_rt = readODS::read_ods(tmp,range = "B10:F122",sheet = "Table1_-_R", col_names = FALSE)
england_consensus_rt = consensus_rt %>% dplyr::transmute(date = as.Date(...1,"%d-%b-%y"), low = ...4, high = ...5)

if(interactive()) interfacer::use_dataframe(england_consensus_rt)

consensus_growth_rate = readODS::read_ods(tmp,range = "B10:F122",sheet = "Table2_-_Growth_rate", col_names = FALSE)
england_consensus_growth_rate = consensus_growth_rate %>% dplyr::transmute(date = as.Date(...1,"%d-%b-%y"), low = ...4/100, high = ...5/100)

if(interactive()) interfacer::use_dataframe(england_consensus_growth_rate)


