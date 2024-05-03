ebola_data_file <- here::here("data", "ebola_wa.rdata")
if (!file.exists(ebola_data_file)) {
  download.file(
    "https://github.com/sbfnk/ebola.forecast.wa.sl/raw/master/data/ebola_wa.rdata",
    ebola_data_file
  )
}
