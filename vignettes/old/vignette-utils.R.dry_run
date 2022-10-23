
# .test_data()
# ggplot2::ggplot(.test_data(),ggplot2::aes(x=date,y=count))+ggplot2::geom_point()+ggplot2::scale_y_continuous(trans="log1p")
.test_data = function(
    changes = tibble::tibble(time = c(0,20,40,60,80), r = c(0.1,0,-0.1,0,0.1)),
    initial = 100,
    duration = 100,
    seed=100
) {
  set.seed(seed)
  tibble::tibble(time = 0:duration) %>%
    dplyr::left_join(changes, by="time") %>%
    tidyr::fill(r) %>%
    dplyr::mutate(rate = initial*exp(cumsum(r))) %>%
    dplyr::mutate(
      count = stats::rpois(dplyr::n(),rate),
      total = max(count)*2
    ) %>% as.timeseries(
      days_in_period = 7, start_date="2020-01-01"
    )
}

# .test_multinomial()
.test_multinomial = function(
    changes = tibble::tibble(
      time = c(0,20,40,60,80),
      variant1 = c(0.1,0,-0.1,0,0.1),
      variant2 = c(0.15,0.05,-0.05,-0.01,0.05)
    ), initial=c(100,1), ...) {
  cols = setdiff(colnames(changes),"time")
  out = tibble::tibble()
  i = 1
  for (col in cols) {
    out = dplyr::bind_rows(
      out,
      .test_data(changes = changes %>% dplyr::select(time, r=!!col), initial=initial[[i]], ...) %>% dplyr::mutate(class = col, time=as.numeric(time)) %>% tibble::as_tibble()
    )
  }

  out = out %>% as.timeseries(
    days_in_period = 7, start_date="2020-01-01"
  )

  return(out)
}


.download = function(url,filename) {
  download_dir = rappdirs::user_cache_dir("growthrates")
  if (!fs::dir_exists(download_dir)) fs::dir_create(download_dir)
  raw_lineages = fs::path(download_dir, filename)
  if (fs::file_exists(raw_lineages)) {
    tmp = fs::file_info(raw_lineages)
    if (tmp$change_time < as.POSIXct(Sys.Date()-7)) unlink(raw_lineages)
  }
  if (!fs::file_exists(raw_lineages)) {
    downloader::download(url,raw_lineages)
  }
  return(raw_lineages)
}
