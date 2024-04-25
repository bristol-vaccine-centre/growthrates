here::i_am("data-raw/dates.R")

england_events = readxl::read_excel(here::here("data-raw/COVID Dates.xlsx"))
england_events = england_events %>% dplyr::transmute(
  label = Label,
  start = as.Date(`Start date`),
  end = as.Date(`End date`)
)

if(interactive()) interfacer::use_dataframe(england_events)
