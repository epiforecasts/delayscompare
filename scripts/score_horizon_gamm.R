## Horizon-sweep power/sensitivity analysis. Scores log-scale CRPS at 1-6 week
## forecast horizons across the reduced GT grid (INC held correct = 4) on N
## replicate disease-like datasets, then fits a GAMM with a te(gt, horizon) tensor
## smooth per disease. This resolves whether GT misspecification degrades
## medium-horizon (3-4 week) case forecasts, and whether that effect scales
## inversely with the generation time (covid, shortest GT, expected most sensitive).
##
## Reads raw per-fit files (results/raw/<disease>_hz_rep<r>_latest_*_gt<g>_inc<i>_tp<t>.rds)
## directly so each replicate is scored against its own noise realisation
## (data/<disease>_sim_data_rep<r>.rds). Convergence filter drops fits with
## max_rhat > 1.05 or num_divergent > 100.

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

rt_opts   <- "latest"
diseases  <- c("ebola", "covid", "cholera")
N_SAMPLES <- 3000L
MAX_WK    <- 6L
raw_dir   <- here("results", "raw")

## GT misspecification level -> generation-time multiplier (level 4 = correct)
gt_mult <- c(`1` = NA, `2` = 0.25, `3` = 0.8, `4` = 1, `5` = 1.25, `6` = 2)

## true (simulated) reported cases for one replicate
true_cases_rep <- function(disease, rep_id) {
  f <- here("data", paste0(disease, "_sim_data_rep", rep_id, ".rds"))
  if (!file.exists(f)) return(NULL)
  readRDS(f) |>
    dplyr::filter(variable == "reported_cases") |>
    dplyr::rename(true_value = value) |>
    dplyr::select(date, true_value)
}

## score one raw fit -> per-horizon log-CRPS, or NULL
score_fit <- function(disease, rep_id, gt, inc, tp) {
  stub <- paste0(disease, "_hz_rep", rep_id, "_", rt_opts)
  suf  <- paste0("_gt", gt, "_inc", inc, "_tp", tp, ".rds")
  sf <- file.path(raw_dir, paste0(stub, "_samples", suf))
  idf <- file.path(raw_dir, paste0(stub, "_id", suf))
  df  <- file.path(raw_dir, paste0(stub, "_diagnostics", suf))
  if (!file.exists(sf) || !file.exists(idf)) return(NULL)

  if (file.exists(df)) {
    d <- readRDS(df)
    ok <- !is.na(d$max_rhat) && d$max_rhat <= 1.05 &&
      (is.na(d$num_divergent) || d$num_divergent <= 100)
    if (!isTRUE(ok)) return(NULL)
  }

  samples <- readRDS(sf); id <- readRDS(idf)
  if (is.null(samples) || nrow(samples) == 0) return(NULL)
  samples <- samples |> dplyr::filter(sample <= N_SAMPLES)

  tc <- true_cases_rep(disease, rep_id)
  if (is.null(tc)) return(NULL)

  id_dedup <- id |> dplyr::distinct(result_list, .keep_all = TRUE) |>
    dplyr::select(result_list, forecast_origin)

  scored <- samples |>
    dplyr::filter(type == "forecast") |>
    dplyr::left_join(id_dedup, by = "result_list") |>
    dplyr::left_join(tc, by = "date") |>
    dplyr::filter(!is.na(true_value)) |>
    dplyr::mutate(horizon = as.numeric(date - forecast_origin) / 7) |>
    dplyr::filter(horizon %in% seq_len(MAX_WK))
  if (nrow(scored) == 0) return(NULL)

  fc <- tryCatch(scoringutils::as_forecast_sample(
    data = scored,
    forecast_unit = c("date", "type", "result_list", "model", "horizon"),
    observed = "true_value", predicted = "prediction", sample_id = "sample"),
    error = function(e) NULL)
  if (is.null(fc)) return(NULL)

  sc <- fc |>
    scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1, label = "log") |>
    scoringutils::score()
  sc |>
    dplyr::filter(scale == "log") |>
    dplyr::transmute(result_list, horizon, log_crps = crps,
                     disease = disease, gt = gt, inc = inc, rep = rep_id)
}

## discover all replicate/gt/tp combinations from the raw filenames
rows <- list()
for (disease in diseases) {
  files <- list.files(raw_dir,
    pattern = paste0("^", disease, "_hz_rep[0-9]+_", rt_opts,
                     "_samples_gt[0-9]+_inc[0-9]+_tp[0-9]+\\.rds$"))
  if (length(files) == 0) { message("  no hz fits for ", disease); next }
  keys <- stringr::str_match(files,
    paste0("_hz_rep([0-9]+)_.*_samples_gt([0-9]+)_inc([0-9]+)_tp([0-9]+)\\.rds$"))
  for (i in seq_len(nrow(keys))) {
    sc <- score_fit(disease,
                    as.integer(keys[i, 2]), as.integer(keys[i, 3]),
                    as.integer(keys[i, 4]), as.integer(keys[i, 5]))
    if (!is.null(sc)) rows[[length(rows) + 1]] <- sc
  }
  message("  ", disease, ": scored ", length(files), " fits")
}

dat <- dplyr::bind_rows(rows)
if (nrow(dat) == 0) stop("No horizon fits scored - run the sweep first.")
dat$disease <- factor(dat$disease, levels = diseases)
dat$time    <- dat$result_list
dat$gt_mult <- gt_mult[as.character(dat$gt)]
readr::write_csv(dat, here("results", "horizon_perorigin_logcrps.csv"))
message("Horizon per-origin scores: ", nrow(dat), " rows")
print(as.data.frame(table(dat$disease, dat$gt, dat$horizon)))

## GAMM: log-CRPS as a smooth tensor over (GT level, horizon) per disease, with a
## per-timepoint smooth and a replicate random effect. te(gt, horizon) lets the GT
## effect grow (or not) with horizon.
library(mgcv)
dat$repf <- factor(dat$rep)
m <- bam(log_crps ~ te(gt, horizon, by = disease, k = c(5, 5)) + disease +
           s(time, by = disease, k = 4) + s(repf, bs = "re"),
         data = dat, method = "fREML", discrete = TRUE)
saveRDS(m, here("results", "horizon_gamm.rds"))
cat("\n=== Horizon GAMM smooth terms ===\n"); print(summary(m)$s.table)

## Marginal effect: predicted log-CRPS vs horizon at each GT level, per disease,
## averaged over replicates (population random effect = 0), median time. 95% CIs.
med_time <- stats::median(dat$time)
ref_rep  <- levels(dat$repf)[1]
grid <- expand.grid(disease = factor(diseases, levels = diseases),
                    gt = 1:6, horizon = 1:MAX_WK, KEEP.OUT.ATTRS = FALSE)
grid$time <- med_time
grid$repf <- factor(ref_rep, levels = levels(dat$repf))
pr <- predict(m, newdata = grid, se.fit = TRUE,
              exclude = paste0("s(repf)"))  # marginalise out the replicate RE
grid$fit <- pr$fit
grid$lower <- pr$fit - 1.96 * pr$se.fit
grid$upper <- pr$fit + 1.96 * pr$se.fit
grid$gt_mult <- gt_mult[as.character(grid$gt)]
readr::write_csv(grid, here("results", "horizon_marginal.csv"))
cat("\n=== Horizon sensitivity (marginal log-CRPS vs horizon by GT level) ===\n")
print(as.data.frame(grid[grid$gt %in% c(1, 4, 6), ]), digits = 3)

## Figure: log-CRPS vs horizon, one line per GT misspecification level, per disease.
grid$gt_lab <- factor(grid$gt, levels = 1:6,
  labels = c("none", "0.25x", "0.8x", "correct", "1.25x", "2x"))
p <- ggplot2::ggplot(grid, ggplot2::aes(x = horizon, y = fit,
       colour = gt_lab, fill = gt_lab, group = gt_lab)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper),
                       alpha = 0.12, colour = NA) +
  ggplot2::geom_line(linewidth = 0.7) +
  ggplot2::facet_wrap(~ disease, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = 1:MAX_WK) +
  ggplot2::scale_colour_viridis_d(end = 0.95) +
  ggplot2::scale_fill_viridis_d(end = 0.95) +
  ggplot2::labs(x = "forecast horizon (weeks)",
                y = "marginal log-scale CRPS (95% CI)",
                colour = "GT misspec.", fill = "GT misspec.",
                title = "Horizon sweep: does GT misspecification degrade medium-horizon forecasts?") +
  ggplot2::theme_bw()
ggplot2::ggsave(here("results", "fig_horizon_sensitivity.png"), p,
                width = 11, height = 4.5, dpi = 150)
message("Saved: results/fig_horizon_sensitivity.png")
