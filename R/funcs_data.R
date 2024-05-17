read_latest <- function(dir, file_name) {
  files <- list.files(dir, pattern = file_name)
  ## last one is the latest one
  readRDS(file.path(dir, files[length(files)]))
}

save_latest <- function(x, dir, file_name) {
  saveRDS(x, file.path(dir, paste0(file_name, Sys.Date(), ".rds")))
}
