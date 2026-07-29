## Q1 sensitivity: per-origin log-scale CRPS across the GT x INC misspecification
## grid for the simulated scenarios (trivial trajectories + disease-like resim),
## each scored against its simulated ground truth. GAMM with GT/INC misspecification
## smooths; sensitivity reported as marginal log-CRPS vs level with 95% CIs.

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

rt_opts   <- "latest"
diseases  <- c("ebola", "covid", "cholera")
N_SAMPLES <- 2000L

## scenario -> sim-data suffix (resim uses the generic <disease>_sim_data)
scen_suffix <- c(const_low = "const_low", const_high = "const_hi",
                 inc = "inc", dec = "dec", resim = NA)

## per (gt, inc, timepoint, horizon) log-scale CRPS for one (disease, scenario)
score_grid <- function(samples, id, diagnostics, true_cases) {
  if (is.null(samples) || is.null(id) || nrow(samples) == 0) return(NULL)
  if (!is.null(diagnostics)) {
    keep <- diagnostics |>
      dplyr::filter(!is.na(max_rhat), max_rhat <= 1.05,
                    is.na(num_divergent) | num_divergent <= 100)
    samples <- samples |> dplyr::semi_join(keep, by = c("gt", "inc", "result_list"))
  }
  if (nrow(samples) == 0) return(NULL)
  samples <- samples |> dplyr::filter(sample <= N_SAMPLES)

  id_dedup <- id |>
    dplyr::distinct(gt, inc, result_list, .keep_all = TRUE) |>
    dplyr::select(gt, inc, result_list, forecast_origin)

  scored <- samples |>
    dplyr::filter(type == "forecast") |>
    dplyr::left_join(id_dedup, by = c("gt", "inc", "result_list")) |>
    dplyr::left_join(true_cases, by = "date") |>
    dplyr::filter(!is.na(true_value)) |>
    dplyr::mutate(horizon = as.numeric(date - forecast_origin) / 7) |>
    dplyr::filter(horizon %in% c(1, 2))
  if (nrow(scored) == 0) return(NULL)

  fc <- tryCatch(scoringutils::as_forecast_sample(
    data = scored,
    forecast_unit = c("date", "type", "gt", "inc", "result_list", "model", "horizon"),
    observed = "true_value", predicted = "prediction", sample_id = "sample"),
    error = function(e) NULL)
  if (is.null(fc)) return(NULL)

  sc <- fc |>
    scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1, label = "log") |>
    scoringutils::score()
  sc |>
    dplyr::filter(scale == "log") |>
    dplyr::transmute(gt, inc, result_list, horizon, log_crps = crps)
}

rows <- list()
for (disease in diseases) {
  for (scen in names(scen_suffix)) {
    sid   <- tryCatch(read_latest(here("results", "agg"), paste0(disease, "_", scen, "_", rt_opts, "_id")),          error = function(e) NULL)
    ssamp <- tryCatch(read_latest(here("results", "agg"), paste0(disease, "_", scen, "_", rt_opts, "_samples")),     error = function(e) NULL)
    sdiag <- tryCatch(read_latest(here("results", "agg"), paste0(disease, "_", scen, "_", rt_opts, "_diagnostics")), error = function(e) NULL)
    if (is.null(ssamp) || is.null(sid)) { message("  skip ", disease, " ", scen, " (no agg)"); next }

    suf  <- scen_suffix[[scen]]
    simf <- if (is.na(suf)) paste0(disease, "_sim_data") else paste0(disease, "_sim_data_", suf)
    sim_data <- tryCatch(read_latest(here("data"), simf), error = function(e) NULL)
    if (is.null(sim_data)) { message("  no sim data ", disease, " ", scen); next }
    true_cases <- sim_data |>
      dplyr::filter(variable == "reported_cases") |>
      dplyr::rename(true_value = value) |>
      dplyr::select(date, true_value)

    sc <- score_grid(ssamp, sid, sdiag, true_cases)
    if (!is.null(sc)) rows[[paste(disease, scen)]] <- sc |>
      dplyr::mutate(disease = disease, trajectory = scen)
  }
}

dat <- dplyr::bind_rows(rows)
dat$disease    <- factor(dat$disease, levels = diseases)
dat$trajectory <- factor(dat$trajectory)
dat$horizon    <- factor(dat$horizon)
dat$time       <- dat$result_list
readr::write_csv(dat, here("results", "q1_perorigin_logcrps.csv"))
message("Q1 per-origin scores: ", nrow(dat), " rows")
print(as.data.frame(table(dat$disease, dat$trajectory)))

## GAMM: sensitivity of log-CRPS to GT and INC misspecification, by disease
library(mgcv)
m <- bam(log_crps ~ s(gt, by = disease, k = 4) + s(inc, by = disease, k = 4) +
           trajectory + s(time, by = disease, k = 4) + horizon,
         data = dat, method = "fREML")
saveRDS(m, here("results", "q1_gamm.rds"))
cat("\n=== Q1 GAMM smooth terms ===\n"); print(summary(m)$s.table)

## Marginal sensitivity: predicted log-CRPS vs GT level (INC held correct=4) and vs
## INC level (GT held correct=4), per disease, at horizon 1, median time. 95% CIs.
ref_traj <- levels(dat$trajectory)[1]
med_time <- stats::median(dat$time)
pred_axis <- function(axis) {
  g <- expand.grid(disease = factor(diseases, levels = diseases),
                   level = 1:6, KEEP.OUT.ATTRS = FALSE)
  g$gt  <- if (axis == "gt") g$level else 4
  g$inc <- if (axis == "inc") g$level else 4
  g$time <- med_time
  g$horizon <- factor(1, levels = levels(dat$horizon))
  g$trajectory <- factor(ref_traj, levels = levels(dat$trajectory))
  pr <- predict(m, newdata = g, se.fit = TRUE)
  g$axis <- axis
  g$fit <- pr$fit; g$lower <- pr$fit - 1.96 * pr$se.fit; g$upper <- pr$fit + 1.96 * pr$se.fit
  g[, c("disease", "axis", "level", "fit", "lower", "upper")]
}
marg <- rbind(pred_axis("gt"), pred_axis("inc"))
readr::write_csv(marg, here("results", "q1_sensitivity_marginal.csv"))
cat("\n=== Q1 sensitivity (marginal log-CRPS vs level, 95% CI) ===\n")
print(as.data.frame(marg), digits = 3)

## Figure: sensitivity curves (log-CRPS vs misspecification level), by disease x axis
axis_lab <- c(gt = "generation time", inc = "incubation")
marg$axis <- factor(axis_lab[marg$axis], levels = axis_lab)
p <- ggplot2::ggplot(marg, ggplot2::aes(x = level, y = fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggplot2::geom_line() +
  ggplot2::geom_vline(xintercept = 4, linetype = "dashed", colour = "grey50") +
  ggplot2::facet_grid(axis ~ disease) +
  ggplot2::scale_x_continuous(breaks = 1:6,
    labels = c("none", "0.25", "0.8", "correct", "1.25", "2")) +
  ggplot2::labs(x = "misspecification level (dashed = correct)",
                y = "marginal log-scale CRPS (95% CI)",
                title = "Q1: sensitivity of forecast skill to delay misspecification") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
ggplot2::ggsave(here("results", "fig_q1_sensitivity.png"), p, width = 10, height = 6, dpi = 150)
message("Saved: results/fig_q1_sensitivity.png")
