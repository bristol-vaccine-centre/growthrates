library(tidyverse)
library(growthrates)

 tibble(time = 0:99, value=rnorm(100)) %>%
   as.timeseries(days_in_period=7,start_date="2020-01-01")

 # defining a multinomial with a class column
 tmp1 = tibble(
     class = c(rep(1,100),rep(2,100)),
     time = rep(0:99,2),
     value = rnorm(200)
  ) %>% as.timeseries(days_in_period=7,start_date="2020-01-01")

 restore_groups(tmp1)
 # still a timeseries:
 class(restore_groups(tmp1))


 # defining a multinomial with a grouing
 tmp = tibble(
     mygroup = c(rep("a",100),rep("b",100)),
     time = rep(0:99,2),
     value = rnorm(200)
   ) %>%
   group_by(mygroup) %>%
   as.timeseries(days_in_period=7,start_date="2020-01-01")

 restore_groups(tmp)
 # no longer a time series:
 class(restore_groups(tmp))
