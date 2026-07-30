## Q1 Rt sensitivity: per-origin CRPS of the reproduction-number forecasts across the
## GT x INC misspecification grid for the simulated scenarios, scored against the
## synthetic true Rt (the trajectory that generated each dataset; sim_data
## variable=="R"). Synthetic-only — the case studies have no ground-truth Rt.
## GAMM with GT/INC misspecification smooths; marginal Rt-CRPS vs level with 95% CIs.

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))   # startenddates
source(here("R", "funcs_data.R"))

rt_opts   <- "latest"
diseases  <- c("ebola", "covid", "cholera")
N_SAMPLES <- 2000L

scen_suffix <- c(const_low = "const_low", const_high = "const_hi",
                 inc = "inc", dec = "dec", resim = NA)

## True Rt trajectory that generated each scenario's data (the sim data itself
## stores only infections/reported_cases). const/inc/dec are the known patterns from
## 05b/05c over each disease's date range; resim is the fitted rt_<disease>.rds.
build_true_rt <- function(disease, scen) {
  if (scen == "resim") {
    rt <- tryCatch(readRDS(here("data", paste0("rt_", disease, ".rds"))), error = function(e) NULL)
    if (is.null(rt)) return(NULL)
    rcol <- if ("R" %in% names(rt)) "R" else if ("median" %in% names(rt)) "median" else return(NULL)
    return(data.frame(date = rt$date, true_R = rt[[rcol]]))
  }
  sd <- startenddates[[disease]]
  dates <- seq.Date(sd[["startdate"]], sd[["enddate"]], by = 1)
  n <- length(dates)
  R <- switch(scen,
              const_low  = rep(0.8, n),
              const_high = rep(1.2, n),
              inc        = seq(0.8, 1.2, length.out = n),
              dec        = rev(seq(0.8, 1.2, length.out = n)),
              return(NULL))
  data.frame(date = dates, true_R = R)
}

## per (gt, inc, timepoint, horizon) Rt CRPS (natural scale — Rt is O(1)) for one
## (disease, scenario)
score_rt_grid <- function(res_R, id, diagnostics, true_rt) {
  if (is.null(res_R) || is.null(id) || nrow(res_R) == 0) return(NULL)
  if (!is.null(diagnostics)) {
    keep <- diagnostics |>
      dplyr::filter(!is.na(max_rhat), max_rhat <= 1.05,
                    is.na(num_divergent) | num_divergent <= 100)
    res_R <- res_R |> dplyr::semi_join(keep, by = c("gt", "inc", "result_list"))
  }
  if (nrow(res_R) == 0) return(NULL)
  res_R <- res_R |> dplyr::filter(sample <= N_SAMPLES)

  id_dedup <- id |>
    dplyr::distinct(gt, inc, result_list, .keep_all = TRUE) |>
    dplyr::select(gt, inc, result_list, forecast_origin)

  scored <- res_R |>
    dplyr::filter(type == "forecast") |>
    dplyr::left_join(id_dedup, by = c("gt", "inc", "result_list")) |>
    dplyr::left_join(true_rt, by = "date") |>
    dplyr::filter(!is.na(true_R)) |>
    dplyr::mutate(horizon = as.numeric(date - forecast_origin) / 7) |>
    dplyr::filter(horizon %in% c(1, 2))
  if (nrow(scored) == 0) return(NULL)

  fc <- tryCatch(scoringutils::as_forecast_sample(
    data = scored,
    forecast_unit = c("date", "type", "gt", "inc", "result_list", "model", "horizon"),
    observed = "true_R", predicted = "value", sample_id = "sample"),
    error = function(e) NULL)
  if (is.null(fc)) return(NULL)

  sc <- scoringutils::score(fc)
  sc |> dplyr::transmute(gt, inc, result_list, horizon, rt_crps = crps)
}

rows <- list()
for (disease in diseases) {
  for (scen in names(scen_suffix)) {
    sid  <- tryCatch(read_latest(here("results", "agg"), paste0(disease, "_", scen, "_", rt_opts, "_id")),          error = function(e) NULL)
    sR   <- tryCatch(read_latest(here("results", "agg"), paste0(disease, "_", scen, "_", rt_opts, "_R")),           error = function(e) NULL)
    sdiag<- tryCatch(read_latest(here("results", "agg"), paste0(disease, "_", scen, "_", rt_opts, "_diagnostics")), error = function(e) NULL)
    if (is.null(sR) || is.null(sid)) { message("  skip ", disease, " ", scen, " (no agg R)"); next }

    true_rt <- build_true_rt(disease, scen)
    if (is.null(true_rt) || nrow(true_rt) == 0) { message("  no true Rt ", disease, " ", scen); next }

    sc <- score_rt_grid(sR, sid, sdiag, true_rt)
    if (!is.null(sc)) rows[[paste(disease, scen)]] <- sc |>
      dplyr::mutate(disease = disease, trajectory = scen)
  }
}

dat <- dplyr::bind_rows(rows)
if (nrow(dat) == 0) stop("No Rt scores produced")
dat$disease    <- factor(dat$disease, levels = diseases)
dat$trajectory <- factor(dat$trajectory)
dat$horizon    <- factor(dat$horizon)
dat$time       <- dat$result_list
readr::write_csv(dat, here("results", "q1_rt_perorigin_crps.csv"))
message("Q1 Rt per-origin scores: ", nrow(dat), " rows")

library(mgcv)
m <- bam(rt_crps ~ s(gt, by = disease, k = 4) + s(inc, by = disease, k = 4) +
           trajectory + s(time, by = disease, k = 4) + horizon,
         data = dat, method = "fREML")
saveRDS(m, here("results", "q1_rt_gamm.rds"))
cat("\n=== Q1 Rt GAMM smooth terms ===\n"); print(summary(m)$s.table)

ref_traj <- levels(dat$trajectory)[1]
med_time <- stats::median(dat$time)
pred_axis <- function(axis) {
  g <- expand.grid(disease = factor(diseases, levels = diseases), level = 1:6, KEEP.OUT.ATTRS = FALSE)
  g$gt  <- if (axis == "gt") g$level else 4
  g$inc <- if (axis == "inc") g$level else 4
  g$time <- med_time
  g$horizon <- factor(1, levels = levels(dat$horizon))
  g$trajectory <- factor(ref_traj, levels = levels(dat$trajectory))
  pr <- predict(m, newdata = g, se.fit = TRUE)
  data.frame(disease = g$disease, axis = axis, level = g$level,
             fit = pr$fit, lower = pr$fit - 1.96 * pr$se.fit, upper = pr$fit + 1.96 * pr$se.fit)
}
marg <- rbind(pred_axis("gt"), pred_axis("inc"))
readr::write_csv(marg, here("results", "q1_rt_sensitivity_marginal.csv"))
cat("\n=== Q1 Rt sensitivity (marginal Rt-CRPS vs level, 95% CI) ===\n")
print(marg, digits = 3)

axis_lab <- c(gt = "generation time", inc = "incubation")
marg$axis <- factor(axis_lab[marg$axis], levels = axis_lab)
p <- ggplot2::ggplot(marg, ggplot2::aes(x = level, y = fit)) +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2) +
  ggplot2::geom_line() +
  ggplot2::geom_vline(xintercept = 4, linetype = "dashed", colour = "grey50") +
  ggplot2::facet_grid(axis ~ disease, scales = "free_y") +
  ggplot2::scale_x_continuous(breaks = 1:6, labels = c("none","0.25","0.8","correct","1.25","2")) +
  ggplot2::labs(x = "misspecification level (dashed = correct)",
                y = "marginal Rt CRPS (95% CI)",
                title = "Q1: sensitivity of Rt estimation to delay misspecification") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
ggplot2::ggsave(here("results", "fig_q1_rt_sensitivity.png"), p, width = 10, height = 6, dpi = 150)
message("Saved: results/fig_q1_rt_sensitivity.png")
