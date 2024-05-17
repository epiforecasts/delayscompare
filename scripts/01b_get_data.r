ebola_data_file <- here::here("data", "ebola_wa.rdata")
cholera_data_file <- here::here("data", "YEM-CHOLERA-EOC-DIS-WEEK-20170424-20200621.csv")

if (!file.exists(ebola_data_file)) {
  download.file(
    "https://github.com/sbfnk/ebola.forecast.wa.sl/raw/master/data/ebola_wa.rdata",
    ebola_data_file
  )
}

if (!file.exists(cholera_data_file)) {
  download.file(
    "https://figshare.com/ndownloader/files/42635185",
     here("data", "YEM-CHOLERA-EOC-DIS-WEEK-20170424-20200621.csv")
  )
}
