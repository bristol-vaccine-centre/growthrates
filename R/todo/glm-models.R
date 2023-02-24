setwd("~/Dropbox/GP_Study_Shared/")

library(tidyverse)

# The demoninators data
denom_xls = readxl::read_excel("DenominatorData.xlsx",sheet = "Table 1 CSR", range = "D5:L11")
denom_tidy = denom_xls %>% rename(Practice = `...1`) %>% select(-`...2`) %>%
  pivot_longer(cols = -Practice, names_to = "age_group", values_to = "denom") %>%
  mutate(
    Practice = case_when(
      # fix up practice names in demoninator
      Practice %>% stringr::str_starts("Tyntes") ~ "Tyntesfield",
      Practice %>% stringr::str_starts("Pion") ~ "Pioneer",
      TRUE ~ Practice
    ),
    age_group = case_when(
      age_group == "45-55" ~ "45-54", # Aargh - this needs fixing shoudl be 54
      TRUE ~ age_group
    ),
    date_onboard = case_when(
      Practice == "Concord" ~ "2022-02-14",
      # Came online on 14.02.22
      Practice == "Pioneer" ~ "2022-02-14",
      #14.02.22
      Practice == "Tyntesfield" ~ "2022-07-04",
      # Came online on 04.07.22
      Practice == "Courtside" ~ "2022-05-30",
      # 30.05.22
      Practice == "Montpelier" ~ "2022-08-01",
      # 01.08.22
      Practice == "Wellspring" ~ "2022-08-01"
      # 01.08.22
    ) %>% as.POSIXct()
  )

denom_timeseries = tibble(
  date = seq(start_date,end_date,"1 week")
) %>%
  inner_join(denom_tidy, by=character()) %>%
  filter(date_onboard <= date) %>%
  mutate(epiweek = lubridate::interval(start_date, date) %/% lubridate::period(1,"week")) %>%
  group_by(epiweek, age_group) %>%
  summarise(denom = sum(denom))

# Just to check this looks sensible.
# ggplot(denom_timeseries, aes(x=epiweek, y=denom, colour = age_group))+geom_line()

start_date = as.POSIXct("2022-02-14")
end_date = as.POSIXct("2022-11-25")
epiweeks = 0:(lubridate::interval(start_date, end_date) %/% lubridate::period(1,"week"))

# The cases data:
cases_csv = readr::read_csv("GP_Study_dates.csv",na = "#N/A")
cases_tidy = cases_csv %>%
  mutate(
    date = lubridate::as_datetime(date_1,format = "%d/%m/%Y %H:%M"),
    age_group = case_when(
      is.na(Age) | Age < 18 ~ NA_character_,
      Age < 25 ~ "18-24",
      Age < 35 ~ "25-34",
      Age < 45 ~ "35-44",
      Age < 55 ~ "45-54",
      Age < 65 ~ "55-64",
      TRUE ~ "≥65"
    ),
    epiweek = lubridate::interval(start_date, date) %/% lubridate::period(1,"week")
  )

# The dates at which additional practices join as
cutovers = cases_tidy %>%
  filter(!is.na(Practice) & !is.na(epiweek)) %>%
  select(Practice, epiweek) %>%
  group_by(Practice) %>%
  summarise(epiweek = min(epiweek))

# calculate confidence intervals in a log/logit transformed space
calculate_ci = function(d,...,inv_fn=NULL) {
  if (is.null(inv_fn)) {
    inv_fn =  d$inv_fn[[1]]
  }
  inv = function(x) {
    tryCatch( inv_fn(x) ,
              error = function(e) rep(NA,length(x)) )
  }
  d %>% mutate(
    lower = inv(qnorm(0.025, fit, se.fit)),
    median = inv(qnorm(0.5, fit, se.fit)),
    upper = inv(qnorm(0.975, fit, se.fit))
  )
}

# Non age specific ----
all_cases_timeseries = cases_tidy %>%
  filter(!is.na(epiweek)) %>%
  group_by(epiweek) %>%
  summarise(count = n()) %>%
  tidyr::complete(epiweek = epiweeks, fill=list(count=0)) %>%
  left_join(
    denom_timeseries %>% filter(age_group == "Total") %>% select(-age_group),
    by="epiweek"
  ) %>%
  mutate(
    date = start_date + epiweek * lubridate::period(1,"week")
  ) %>%
  mutate(
    norm_count = floor(count/denom*100000),
    positive_fraction = count/denom
  )

# Count model normalised by denominator
# This is good to check but proportions model preferred
# count_model = function(d,...) {
#   epiweeks = full_seq(d$epiweek,1)
#   model = glm(norm_count ~ splines::bs(epiweek,knots = 10), family = quasipoisson, data = d)
#   new_data = tibble(epiweek = epiweeks, denom = 100000 )
#   rate_estimate = predict(model, newdata = new_data, type="response")
#   est2 = predict(model, newdata = new_data,se.fit = TRUE)
#   new_data %>%
#     mutate(
#       rate = unname(rate_estimate),
#       orig = d$norm_count,
#       fit= unname(est2$fit),
#       se.fit = unname(est2$se.fit),
#       inv_fn = list(model$family$linkinv)
#     )
# }
#
# plot_data = all_cases_timeseries %>% count_model() %>% calculate_ci()
# ggplot(plot_data, aes(x=epiweek, y=median, ymin=lower, ymax=upper))+
#   geom_line()+
#   geom_ribbon(alpha=0.1)+
#   geom_point(aes(x=epiweek,y=orig),inherit.aes = FALSE)+
#   geom_vline(data = cutovers, aes(xintercept=epiweek), colour="blue")


# Proportions model
proportion_model = function(d,..., df=5) {
  # see http://www.simonqueenborough.info/R/statistics/glm-binomial
  # for this next bit:
  y = cbind(d$count, d$denom - d$count)
  epiweeks = full_seq(d$epiweek,1)
  # Some fiddling with the spline term here
  # model = glm(y ~ splines::bs(epiweek,knots = 5), family = quasibinomial, data = d)
  # I think we need to normalise the spline degreed of freedom by data length
  # to stop extra wigglyness in shorter timeseries.
  df = floor(df / 40 * length(epiweeks))
  model = glm(y ~ splines::ns(epiweek, df = df), family = quasibinomial, data = d)
  new_data = tibble(epiweek = epiweeks, denom = 100000 )
  # response is transformed:
  proportion_estimate = predict(model, newdata = new_data, type="response")
  # this prediction in in the logit space:
  est2 = predict(model, newdata = new_data,se.fit = TRUE)
  # it is possible to get CIs out here but they are the logit transformed ones.

  return(new_data %>% mutate(
    proportion = unname(proportion_estimate),
    orig = d$positive_fraction,
    fit= unname(est2$fit),
    se.fit = unname(est2$se.fit),
    inv_fn = list(model$family$linkinv) # < this is the inverse logit function
  ))
}

plot_data = all_cases_timeseries %>% proportion_model(df=6) %>% calculate_ci()

ggplot(plot_data, aes(x=epiweek, y=median*100000, ymin=lower*100000, ymax=upper*100000))+
  geom_line()+
  geom_ribbon(alpha=0.1)+
  geom_point(aes(x=epiweek,y=orig*100000),inherit.aes = FALSE)+
  coord_cartesian(ylim = c(0,NA))+
  geom_vline(data = cutovers, aes(xintercept=epiweek), colour="blue")

# Age specific ----
age_cases_timeseries = cases_tidy %>%
  filter(!is.na(epiweek) & !is.na(age_group)) %>%
  group_by(epiweek,age_group) %>%
  summarise(count = n()) %>%
  ungroup() %>%
  tidyr::complete(epiweek = epiweeks, age_group, fill=list(count=0)) %>%
  left_join(
    denom_timeseries %>% filter(age_group != "Total"),
    by=c("epiweek","age_group")
  ) %>%
  mutate(
    date = start_date + epiweek * lubridate::period(1,"week")
  ) %>%
  mutate(
    norm_count = floor(count/denom*100000),
    positive_fraction = count/denom,
    age_group = age_group %>% factor(c("18-24", "25-34", "35-44", "45-54", "55-64", "≥65"))
  )


# Count model normalised by denominator
# plot_data = age_cases_timeseries %>% group_by(age_group) %>% group_modify(count_model) %>% calculate_ci()
# ggplot(plot_data, aes(x=epiweek, y=median, ymin=lower, ymax=upper))+
#   geom_line()+
#   geom_ribbon(alpha=0.1)+
#   geom_point(aes(x=epiweek,y=orig),inherit.aes = FALSE)+
#   facet_wrap(vars(age_group))


# Proportions model grouped by age
plot_data = age_cases_timeseries %>% group_by(age_group) %>%
  group_modify(proportion_model, df=6) %>%
  calculate_ci()

ggplot(plot_data, aes(x=epiweek, y=median*100000, ymin=lower*100000, ymax=upper*100000))+
  geom_line()+
  geom_ribbon(alpha=0.1)+
  geom_point(aes(x=epiweek,y=orig*100000),inherit.aes = FALSE)+
  geom_vline(data = cutovers, aes(xintercept=epiweek), colour="blue")+
  coord_cartesian(ylim = c(0,NA))+
  facet_wrap(vars(age_group))



# Age and Practice specific broken down then rolled up into overall ----
# We use a proportions model for each combination of age group and practice.
# This will be different time periods.
# where the overlap is we combine normally distributes fit results prior to logit transform

age_and_practice_cases_timeseries = cases_tidy %>%
  filter(!is.na(epiweek) & !is.na(age_group) & !is.na(Practice)) %>%
  group_by(epiweek,age_group,Practice) %>%
  summarise(count = n()) %>%
  group_by(Practice) %>%
  # This is like this for a reason: Practice data collection may starts at different
  # times, however age_groups within the practice all at the same time.
  group_modify(function(d,g,...) {
    epiweeks = full_seq(d$epiweek,period = 1)
    d %>% tidyr::complete(age_group, epiweek = epiweeks, fill=list(count=0))
  }) %>%
  left_join(
    denom_tidy %>% filter(age_group != "Total"),
    by=c("Practice","age_group")
  ) %>%
  mutate(
    date = start_date + epiweek * lubridate::period(1,"week")
  ) %>%
  mutate(
    norm_count = floor(count/denom*100000),
    positive_fraction = count/denom,
    age_group = age_group %>% factor(c("18-24", "25-34", "35-44", "45-54", "55-64", "≥65"))
  )

# just check I got the data combining done OK
# ggplot(age_and_practice_cases_timeseries, aes(x=epiweek, y=positive_fraction*100000))+
#   geom_point()+
#   coord_cartesian(ylim = c(0,200)) +
#   facet_grid(age_group ~ Practice)

# Playing with the DF parameter here gives quite different fits (smaller = smoother):
plot_data = age_and_practice_cases_timeseries %>%
  group_by(Practice, age_group) %>%
  group_modify(proportion_model, df=6) %>%
  calculate_ci()

# This gives an idea of what the estimate is like if we only had one practice:
ggplot(plot_data, aes(x=epiweek, y=median*100000, ymin=lower*100000, ymax=upper*100000))+
  geom_line()+
  geom_ribbon(alpha=0.1)+
  geom_point(aes(x=epiweek,y=orig*100000),inherit.aes = FALSE)+
  coord_cartesian(ylim = c(0,500))+
  geom_vline(data = cutovers, aes(xintercept=epiweek), colour="blue")+
  facet_grid(age_group ~ Practice)

source(file = "mixture-distributions.R")

# calculate a weighted combination of Practice estimates based on
# the denom size. This is somewhat complex it gives you a joint mixture
# distributions in the logit transformed space. We need to combine this using a
# weighted mixture distribution rather
# than combining with central limit which is the the sum of the distributions.
# The mixture disctirbution function is in another file and I have used before
# but it's not exactkly bulletproof.
# see https://stats.stackexchange.com/questions/390931/compute-quantile-function-from-a-mixture-of-normal-distribution
inv_fn = plot_data$inv_fn[[1]]
plot_data2 = plot_data %>%
  group_by(age_group, epiweek) %>%
  summarise(
    lower = inv_fn(qmixnorm(p = 0.025, fit, se.fit, denom)),
    median = inv_fn(qmixnorm(p = 0.5, fit, se.fit, denom)),
    upper = inv_fn(qmixnorm(p = 0.975, fit, se.fit, denom)),
    orig = weighted.mean(orig,denom) # Not completely convinced this is legit.
  )

ggplot(plot_data2, aes(x=epiweek, y=median*100000, ymin=lower*100000, ymax=upper*100000))+
  geom_line()+
  geom_ribbon(alpha=0.1)+
  geom_point(aes(x=epiweek,y=orig*100000),inherit.aes = FALSE)+
  coord_cartesian(ylim = c(0,500))+
  geom_vline(data = cutovers, aes(xintercept=epiweek), colour="blue")+
  facet_wrap(vars(age_group))


# Aggregate all the different estimates for age_group / Practice into one single one.
plot_data3 = plot_data %>%
  group_by(epiweek) %>%
  summarise(
    lower = inv_fn(qmixnorm(p = 0.025, fit, se.fit, denom)),
    median = inv_fn(qmixnorm(p = 0.5, fit, se.fit, denom)),
    upper = inv_fn(qmixnorm(p = 0.975, fit, se.fit, denom)),
    orig = weighted.mean(orig,denom) # Not completely convinced this is legit.
  )

# I think the CI's here are pretty solid. I'm confident that they are wide enough :-/
ggplot(plot_data3, aes(x=epiweek, y=median*100000, ymin=lower*100000, ymax=upper*100000))+
  geom_line()+
  geom_ribbon(alpha=0.1)+
  geom_point(aes(x=epiweek,y=orig*100000),inherit.aes = FALSE)+
  coord_cartesian(ylim = c(0,500))+
  geom_vline(data = cutovers, aes(xintercept=epiweek), colour="blue")
