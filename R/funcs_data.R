# Timepoint number parsed from a "..._tp<k>.rds" filename
tp_num <- function(paths) as.integer(sub(".*_tp([0-9]+)\\.rds$", "\\1", basename(paths)))

read_latest <- function(dir, file_name) {
  # Deterministic aggregated file: one <file_name>.rds
  exact <- file.path(dir, paste0(file_name, ".rds"))
  if (file.exists(exact)) return(readRDS(exact))

  # Raw per-timepoint files: <file_name>_tp<k>.rds, combined in timepoint order
  tp_files <- list.files(dir, pattern = paste0("^", file_name, "_tp[0-9]+\\.rds$"),
                         full.names = TRUE)
  if (length(tp_files) > 0) {
    tp_files <- tp_files[order(tp_num(tp_files))]
    dat <- lapply(tp_files, readRDS)
    dfs <- Filter(is.data.frame, dat)
    if (length(dfs) > 0) return(dplyr::bind_rows(dfs))
    return(dat[[1]])
  }
  stop(paste("No file(s) found for:", file_name, "in", dir))
}

read_latest_list <- function(dir, file_name) {
  # Like read_latest but concatenates lists (e.g. summary lists of epinowfit objects)
  exact <- file.path(dir, paste0(file_name, ".rds"))
  if (file.exists(exact)) return(readRDS(exact))

  tp_files <- list.files(dir, pattern = paste0("^", file_name, "_tp[0-9]+\\.rds$"),
                         full.names = TRUE)
  if (length(tp_files) > 0) {
    tp_files <- tp_files[order(tp_num(tp_files))]
    return(do.call(c, lapply(tp_files, readRDS)))
  }
  stop(paste("No file(s) found for:", file_name, "in", dir))
}

fit_diagnostics <- function(fit, timepoint) {
  # Compact MCMC diagnostics (max Rhat, divergences, min ESS) from one stan fit.
  # Called at run time with return_fit = TRUE, so the heavy fit can then be dropped.
  tryCatch({
    if (is.null(fit)) {
      return(data.frame(timepoint = timepoint, max_rhat = NA_real_,
                        num_divergent = NA_integer_, min_ess = NA_real_))
    }
    s  <- rstan::summary(fit)$summary
    sp <- rstan::get_sampler_params(fit, inc_warmup = FALSE)
    data.frame(
      timepoint = timepoint,
      max_rhat = if (all(is.na(s[, "Rhat"]))) NA_real_ else max(s[, "Rhat"], na.rm = TRUE),
      num_divergent = sum(sapply(sp, function(z) sum(z[, "divergent__"]))),
      min_ess = if (all(is.na(s[, "n_eff"]))) NA_real_ else min(s[, "n_eff"], na.rm = TRUE)
    )
  }, error = function(e) data.frame(timepoint = timepoint, max_rhat = NA_real_,
                                    num_divergent = NA_integer_, min_ess = NA_real_))
}

save_latest <- function(x, dir, file_name) {
  # Deterministic write: overwrites in place on re-run (no date suffix), so
  # results never accumulate stale copies.
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  saveRDS(x, file.path(dir, paste0(file_name, ".rds")))
}

save_scenario <- function(res, dir, disease, scenario, rt, gt, inc) {
  # Write one deterministic file per (type, timepoint):
  #   <dir>/<disease>_<scenario>_<rt>_<type>_gt<gt>_inc<inc>_tp<k>.rds
  # Splitting on the actual timepoint means single- and multi-timepoint jobs
  # produce the same per-timepoint layout, which read_latest() re-binds.
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  fname <- function(type, k)
    file.path(dir, paste0(disease, "_", scenario, "_", rt, "_", type,
                          "_gt", gt, "_inc", inc, "_tp", k, ".rds"))
  tps <- sort(unique(res$id$result_list))
  for (k in tps) {
    write_sub <- function(x, type, key) {
      sub <- x[x[[key]] == k, , drop = FALSE]
      if (nrow(sub) > 0) saveRDS(sub, fname(type, k))
    }
    write_sub(res$samples, "samples", "result_list")
    write_sub(res$id, "id", "result_list")
    write_sub(res$R, "R", "result_list")
    write_sub(res$diagnostics, "diagnostics", "result_list")
    write_sub(res$timing, "timing", "timepoint")
    saveRDS(res$warnings, fname("warnings", k))  # whole-run warnings, per timepoint
  }
}
