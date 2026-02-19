read_latest <- function(dir, file_name) {
  # First try exact match (non-split files)
  files <- list.files(dir, pattern = paste0("^", file_name, "[0-9]{4}-[0-9]{2}-[0-9]{2}\\.rds$"))

  if (length(files) > 0) {
    # Return latest single file
    return(readRDS(file.path(dir, files[length(files)])))
  }

  # Try split files (e.g., file_name_tp1-2, file_name_tp3-4, etc.)
  split_files <- list.files(dir, pattern = paste0("^", file_name, "_tp[0-9]+-[0-9]+.*\\.rds$"))

  if (length(split_files) > 0) {
    # Read and combine all split files
    split_data <- lapply(file.path(dir, split_files), readRDS)
    return(dplyr::bind_rows(split_data))
  }

  # Fall back to original behaviour for backwards compatibility
  files <- list.files(dir, pattern = file_name)
  if (length(files) == 0) {
    stop(paste("No files found matching pattern:", file_name))
  }
  readRDS(file.path(dir, files[length(files)]))
}

save_latest <- function(x, dir, file_name) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  saveRDS(x, file.path(dir, paste0(file_name, Sys.Date(), ".rds")))
}
