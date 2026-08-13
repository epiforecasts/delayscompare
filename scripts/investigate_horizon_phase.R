## Mechanism probe for the horizon sweep: is the DIRECTION of GT-misspecification
## degradation set by the epidemic phase at the forecast origin? Hypothesis: the model
## absorbs a wrong GT into Rt for the in-sample fit, but the forecast propagates the
## extrapolated Rt through the wrong GT, and the sign of the resulting case-forecast
## bias depends on whether the epidemic is growing or declining over the forecast window.
##
## For each (disease, timepoint) we compute (a) the phase = mean true R over the 42d
## forecast window, and (b) the GT slope = OLS coefficient of mean log-CRPS on GT level
## across the 6 GT levels, at a given horizon. If the hypothesis holds, the GT slope
## crosses zero as the phase crosses R = 1.

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

rt_opts  <- "latest"
diseases <- c("ebola", "covid", "cholera")
raw_dir  <- here("results", "raw")

dat <- readr::read_csv(here("results", "horizon_perorigin_logcrps.csv"),
                       show_col_types = FALSE)

## per (disease, timepoint) forecast-origin date, from any available id file
origin_for_tp <- function(disease, tp) {
  fs <- list.files(raw_dir, full.names = TRUE,
    pattern = paste0("^", disease, "_hz_rep[0-9]+_", rt_opts,
                     "_id_gt[0-9]+_inc4_tp", tp, "\\.rds$"))
  if (length(fs) == 0) return(NA)
  id <- readRDS(fs[1])
  as.Date(id$forecast_origin[1])
}

## mean true R over [origin, origin+42d] from the simulating trajectory
phase_R <- function(rt, origin) {
  w <- rt[rt$date >= origin & rt$date <= origin + 42, ]
  if (nrow(w) == 0) return(NA)
  mean(w$R, na.rm = TRUE)
}

rt_col <- function(rt) if ("R" %in% names(rt)) "R" else "median"

rows <- list()
for (disease in diseases) {
  rtd <- readRDS(here("data", paste0("rt_", disease, ".rds")))
  rt  <- data.frame(date = rtd$date, R = rtd[[rt_col(rtd)]])
  sub <- dat[dat$disease == disease, ]
  ## mean log-CRPS over replicates per (timepoint, gt, horizon)
  agg <- sub |>
    dplyr::group_by(time, gt, horizon) |>
    dplyr::summarise(mlc = mean(log_crps), .groups = "drop")
  for (tp in sort(unique(agg$time))) {
    org <- origin_for_tp(disease, tp)
    if (is.na(org)) next
    pr <- phase_R(rt, org)
    for (h in sort(unique(agg$horizon))) {
      cell <- agg[agg$time == tp & agg$horizon == h, ]
      ## drop the degenerate level 1 (GT multiplier 0) and use log GT multiplier
      ## so the slope is a clean elasticity of log-CRPS to GT scaling
      cell$gt_mult <- c(NA, 0.25, 0.8, 1, 1.25, 2)[cell$gt]
      cell <- cell[!is.na(cell$gt_mult), ]
      if (nrow(cell) < 3) next
      slope <- unname(coef(lm(mlc ~ log(gt_mult), data = cell))[2])
      rows[[length(rows) + 1]] <- data.frame(
        disease = disease, tp = tp, horizon = h,
        origin = org, phase_R = pr, gt_slope = slope,
        crps_correct = cell$mlc[cell$gt == 4])
    }
  }
}
res <- dplyr::bind_rows(rows)
res$disease <- factor(res$disease, levels = diseases)
res$phase <- ifelse(res$phase_R > 1, "growing", "declining")
readr::write_csv(res, here("results", "horizon_phase_slopes.csv"))

cat("=== GT slope by phase (horizon 4 & 6), positive = longer GT worse ===\n")
for (h in c(4, 6)) {
  cat("\n-- horizon", h, "weeks --\n")
  s <- res[res$horizon == h, ]
  print(aggregate(gt_slope ~ disease + phase, s, function(x) round(mean(x), 4)))
}
cat("\n=== correlation of GT slope with phase_R (horizon 6), per disease ===\n")
s6 <- res[res$horizon == 6, ]
print(by(s6, s6$disease, function(d)
  round(cor(d$phase_R, d$gt_slope, use = "complete.obs"), 3)))

## Figure: GT slope vs phase (mean true R over window), horizon 6, per origin.
## A phase-dependent sign shows as points crossing gt_slope = 0 near phase_R = 1.
p <- ggplot2::ggplot(s6, ggplot2::aes(phase_R, gt_slope, colour = disease)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_vline(xintercept = 1, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_point(size = 2.5) +
  ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "lm", se = FALSE,
                       colour = "black", linewidth = 0.5) +
  ggplot2::scale_colour_viridis_d(end = 0.9) +
  ggplot2::labs(x = "epidemic phase: mean true R over 42d forecast window",
                y = "GT slope of log-CRPS (per level; +ve = longer GT worse)",
                title = "Horizon sweep: does GT-misspecification direction track epidemic phase? (6-wk)") +
  ggplot2::theme_bw()
ggplot2::ggsave(here("results", "fig_horizon_phase.png"), p, width = 8, height = 5, dpi = 150)
message("Saved: results/fig_horizon_phase.png")
