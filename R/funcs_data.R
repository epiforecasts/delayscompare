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
    # Only bind data.frames; return first element if none are data.frames
    df_elements <- Filter(is.data.frame, split_data)
    if (length(df_elements) > 0) return(dplyr::bind_rows(df_elements))
    return(split_data[[1]])
  }

  # Fall back to original behaviour for backwards compatibility
  files <- list.files(dir, pattern = file_name)
  if (length(files) == 0) {
    stop(paste("No files found matching pattern:", file_name))
  }
  readRDS(file.path(dir, files[length(files)]))
}

read_latest_list <- function(dir, file_name) {
  # Like read_latest but concatenates lists (for summary files containing
  # lists of epinowfit objects rather than data.frames)
  files <- list.files(dir, pattern = paste0("^", file_name, "[0-9]{4}-[0-9]{2}-[0-9]{2}\\.rds$"))
  if (length(files) > 0) {
    return(readRDS(file.path(dir, files[length(files)])))
  }

  split_files <- list.files(dir, pattern = paste0("^", file_name, "_tp[0-9]+-[0-9]+.*\\.rds$"))
  if (length(split_files) > 0) {
    split_data <- lapply(file.path(dir, split_files), readRDS)
    return(do.call(c, split_data))
  }

  files <- list.files(dir, pattern = file_name)
  if (length(files) == 0) {
    stop(paste("No files found matching pattern:", file_name))
  }
  readRDS(file.path(dir, files[length(files)]))
}

extract_diagnostics <- function(summary_list) {
  # Extract MCMC diagnostics (Rhat, divergent transitions, ESS) from a list
  # of epinowfit objects. Returns a data.frame with one row per timepoint.
  diagnostics <- lapply(seq_along(summary_list), function(i) {
    tryCatch({
      fit <- summary_list[[i]]$fit
      if (is.null(fit)) {
        return(data.frame(result_list = i, max_rhat = NA_real_,
                          num_divergent = NA_integer_, min_ess = NA_real_))
      }

      s <- rstan::summary(fit)$summary
      sp <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
      divs <- sum(sapply(sp, function(z) sum(z[, "divergent__"])))

      rhat_vals <- s[, "Rhat"]
      ess_vals <- s[, "n_eff"]

      data.frame(
        result_list = i,
        max_rhat = if (all(is.na(rhat_vals))) NA_real_ else max(rhat_vals, na.rm = TRUE),
        num_divergent = divs,
        min_ess = if (all(is.na(ess_vals))) NA_real_ else min(ess_vals, na.rm = TRUE)
      )
    }, error = function(e) {
      data.frame(result_list = i, max_rhat = NA_real_,
                 num_divergent = NA_integer_, min_ess = NA_real_)
    })
  })

  dplyr::bind_rows(diagnostics)
}

save_latest <- function(x, dir, file_name) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  saveRDS(x, file.path(dir, paste0(file_name, Sys.Date(), ".rds")))
}
