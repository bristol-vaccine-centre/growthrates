# 100 weeks from 2020-01-01

tmp = as.time_period(0:100, 7, "2020-01-01")
as.Date(tmp)

range(tmp)
min(tmp)
tmp2 = as.integer(as.Date(tmp))
# testthat::expect_true(all(na.omit(tmp2-lag(tmp2)) == 7))

tmp2 = as.time_period(0:23, 1/24, "2020-01-01")
as.POSIXct(tmp2)

# convert timeseries to new "unit"
tmp = as.time_period(0:100, 7, "2020-01-01")
tmp2 = as.time_period(tmp,1)
testthat::expect_equal(as.numeric(tmp2), 0:100*7)
