# Proportions
# data4 %>%
#   mutate(time = cut_date(date, unit = "1 week", anchor="start", output="time_period")) %>%
#   group_by(time, serotype) %>%
#   summarise(count = n(), .groups="drop_last") %>%
#   mutate(denom = sum(count)) %>%
#   group_by(serotype) %>%
#   group_modify(proportion_model, deg=3) %>%
#   glimpse()

# Counts
# data4 %>%
#   mutate(time = cut_date(date, unit = "1 week", anchor="start", output="time_period")) %>%
#   group_by(time, serotype) %>%
#   summarise(count = n(), .groups="drop_last") %>%
#   group_by(serotype) %>%
#   group_modify(poisson_model, df=4) %>%
#   glimpse()
