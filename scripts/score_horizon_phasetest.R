## Horizon-sweep PHASE TEST on the trivial constant-Rt scenarios. const_low (R = 0.8,
## sustained decline) vs const_hi (R = 1.2, sustained growth) give a clean phase
## contrast at matched |R - 1| with no trajectory-shape confound: every forecast origin
## in const_low is declining, every one in const_hi is growing. If the medium-horizon
## GT-misspecification direction tracks epidemic phase, the GT slope of log-CRPS should
## differ (ideally flip sign) between the two scenarios.
##
## Reads results/raw/<disease>_hz_<scen>_rep<r>_latest_*_gt<g>_inc4_tp<t>.rds, scores
## 1-6 wk log-CRPS against data/<disease>_sim_data_<scen>_rep<r>.rds, then fits the GT
## slope (log GT multiplier, degenerate level 1 dropped) per (disease, scenario, horizon).

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

rt_opts  <- "latest"
diseases <- c("ebola", "covid", "cholera")
scens    <- c(const_low = "declining", const_hi = "growing")
N_SAMPLES <- 3000L
MAX_WK   <- 6L
raw_dir  <- here("results", "raw")
gt_mult  <- c(`1` = NA, `2` = 0.25, `3` = 0.8, `4` = 1, `5` = 1.25, `6` = 2)

true_cases_rep <- function(disease, scen, rep_id) {
  f <- here("data", paste0(disease, "_sim_data_", scen, "_rep", rep_id, ".rds"))
  if (!file.exists(f)) return(NULL)
  readRDS(f) |> dplyr::filter(variable == "reported_cases") |>
    dplyr::rename(true_value = value) |> dplyr::select(date, true_value)
}

score_fit <- function(disease, scen, rep_id, gt, tp) {
  stub <- paste0(disease, "_hz_", scen, "_rep", rep_id, "_", rt_opts)
  suf  <- paste0("_gt", gt, "_inc4_tp", tp, ".rds")
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
  tc <- true_cases_rep(disease, scen, rep_id); if (is.null(tc)) return(NULL)
  id_dedup <- id |> dplyr::distinct(result_list, .keep_all = TRUE) |>
    dplyr::select(result_list, forecast_origin)
  scored <- samples |> dplyr::filter(type == "forecast") |>
    dplyr::left_join(id_dedup, by = "result_list") |>
    dplyr::left_join(tc, by = "date") |>
    dplyr::filter(!is.na(true_value)) |>
    dplyr::mutate(horizon = as.numeric(date - forecast_origin) / 7) |>
    dplyr::filter(horizon %in% seq_len(MAX_WK))
  if (nrow(scored) == 0) return(NULL)
  fc <- tryCatch(scoringutils::as_forecast_sample(data = scored,
    forecast_unit = c("date", "type", "result_list", "model", "horizon"),
    observed = "true_value", predicted = "prediction", sample_id = "sample"),
    error = function(e) NULL)
  if (is.null(fc)) return(NULL)
  sc <- fc |>
    scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1, label = "log") |>
    scoringutils::score()
  sc |> dplyr::filter(scale == "log") |>
    dplyr::transmute(horizon, log_crps = crps,
                     disease = disease, scen = scen, gt = gt, rep = rep_id)
}

rows <- list()
for (disease in diseases) for (scen in names(scens)) {
  files <- list.files(raw_dir, pattern = paste0("^", disease, "_hz_", scen,
    "_rep[0-9]+_", rt_opts, "_samples_gt[0-9]+_inc4_tp[0-9]+\\.rds$"))
  if (length(files) == 0) { message("  no fits ", disease, " ", scen); next }
  keys <- stringr::str_match(files,
    paste0("_hz_", scen, "_rep([0-9]+)_.*_samples_gt([0-9]+)_inc4_tp([0-9]+)\\.rds$"))
  for (i in seq_len(nrow(keys))) {
    sc <- score_fit(disease, scen, as.integer(keys[i,2]), as.integer(keys[i,3]),
                    as.integer(keys[i,4]))
    if (!is.null(sc)) rows[[length(rows)+1]] <- sc
  }
  message("  ", disease, " ", scen, ": ", length(files), " fits")
}
dat <- dplyr::bind_rows(rows)
if (nrow(dat) == 0) stop("No phase-test fits scored yet.")
dat$phase <- scens[dat$scen]
readr::write_csv(dat, here("results", "horizon_phasetest_perorigin.csv"))

## mean log-CRPS per (disease, scen, gt, horizon), then GT slope on log multiplier
agg <- dat |> dplyr::group_by(disease, scen, phase, gt, horizon) |>
  dplyr::summarise(mlc = mean(log_crps), n = dplyr::n(), .groups = "drop")
agg$gt_mult <- gt_mult[as.character(agg$gt)]

slopes <- agg |> dplyr::filter(!is.na(gt_mult)) |>
  dplyr::group_by(disease, scen, phase, horizon) |>
  dplyr::summarise(gt_slope = unname(coef(lm(mlc ~ log(gt_mult)))[2]),
                   crps_correct = mlc[gt == 4][1], .groups = "drop")
readr::write_csv(slopes, here("results", "horizon_phasetest_slopes.csv"))

cat("\n=== GT slope by scenario (+ve = longer GT worse), horizons 4 & 6 ===\n")
for (h in c(4, 6)) {
  cat("\n-- horizon", h, "wk --\n")
  s <- slopes[slopes$horizon == h, c("disease","phase","gt_slope")]
  print(as.data.frame(reshape(s, idvar="disease", timevar="phase", direction="wide")),
        digits = 3, row.names = FALSE)
}

## Figure: GT slope vs horizon, growing vs declining, per disease. Divergence between
## the two lines (esp. a sign flip) is the phase signature.
slopes$disease <- factor(slopes$disease, levels = diseases)
p <- ggplot2::ggplot(slopes, ggplot2::aes(horizon, gt_slope, colour = phase, group = phase)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_line(linewidth = 0.8) + ggplot2::geom_point(size = 2) +
  ggplot2::facet_wrap(~ disease, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = 1:MAX_WK) +
  ggplot2::scale_colour_manual(values = c(declining = "#2c7fb8", growing = "#d95f0e")) +
  ggplot2::labs(x = "forecast horizon (weeks)",
                y = "GT slope of log-CRPS (+ve = longer GT worse)",
                colour = "phase (const R)",
                title = "Phase test: GT-misspecification direction under sustained growth vs decline") +
  ggplot2::theme_bw()
ggplot2::ggsave(here("results", "fig_horizon_phasetest.png"), p, width = 11, height = 4.5, dpi = 150)
message("Saved: results/fig_horizon_phasetest.png")
