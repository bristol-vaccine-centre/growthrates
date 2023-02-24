library(tidyverse)
library(growthrates)

# a timed_df object:
tmp1 = tibble(
    time = floor(runif(100, 0, 20)),
    value = rnorm(100)
  ) %>%
  as.timed_df(days_in_period=7,start_date="2020-01-01")
# defaults to `count = n()`


tmp1 %>% time_summarise()


tmp1 %>% time_summarise(value = mean(value))
# the resulting timeseries are weekly as the inupt was weekly.

# class grouping example

tmp2 = tibble(
    mygroup = c(rep("g1",50),rep("g2",50),rep("g1",50),rep("g2",50)),
    myclass = c(rep("a",100),rep("b",100)),
    time = floor(runif(200, 0, 20)),
    value = rnorm(200)
  ) %>%
  group_by(mygroup) %>%
  as.timed_df(days_in_period=7,start_date="2020-01-01",class_col=myclass,grp_label = "{mygroup}")

tmp3 = tmp2 %>% time_summarise()
tmp3 %>% restore_groups()


tmp4 = tmp3 %>% time_decompose()
tmp4 %>% restore_groups()
