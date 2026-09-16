## Turning-point / curvature test for the horizon sweep, resim arm. Hypothesis: the GT-
## misspecification penalty scales with how much the reproduction number CHANGES over the
## forecast window (dR/dt), not with the R level and not specifically with R crossing 1.
## Constant R (any level) gives GT-invariant forecasts (shown by the const phase test);
## here we test the resim origins on the same axis, using the change in the TRUE R
## (rt_<disease>.rds, exact) over each forecast window rather than a noisy case-curvature
## proxy.
##
## Per (disease, forecast origin): dR = R(origin+42) - R(origin) and the max-min R range
## over the window, from the true simulating Rt. GT slope per origin = OLS of mean
## log-CRPS on log GT multiplier (level 1 dropped), per horizon. Plot |GT slope| vs |dR|.

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

rt_opts  <- "latest"
diseases <- c("ebola", "covid", "cholera")
raw_dir  <- here("results", "raw")
gt_mult  <- c(`1` = NA, `2` = 0.25, `3` = 0.8, `4` = 1, `5` = 1.25, `6` = 2)

dat <- readr::read_csv(here("results", "horizon_perorigin_logcrps.csv"),
                       show_col_types = FALSE)

rt_col <- function(rt) if ("R" %in% names(rt)) "R" else "median"
true_R <- function(disease) {
  rt <- readRDS(here("data", paste0("rt_", disease, ".rds")))
  data.frame(date = rt$date, R = rt[[rt_col(rt)]])
}
## true R interpolated to a given date
R_at <- function(rt, d) {
  if (d <= min(rt$date)) return(rt$R[1])
  if (d >= max(rt$date)) return(rt$R[nrow(rt)])
  approx(as.numeric(rt$date), rt$R, as.numeric(d))$y
}

origin_for_tp <- function(disease, tp) {
  fs <- list.files(raw_dir, full.names = TRUE,
    pattern = paste0("^", disease, "_hz_rep[0-9]+_", rt_opts,
                     "_id_gt[0-9]+_inc4_tp", tp, "\\.rds$"))
  if (length(fs) == 0) return(NA)
  as.Date(readRDS(fs[1])$forecast_origin[1])
}

rows <- list()
for (disease in diseases) {
  rt <- true_R(disease)
  sub <- dat[dat$disease == disease, ]
  agg <- sub |> dplyr::group_by(time, gt, horizon) |>
    dplyr::summarise(mlc = mean(log_crps), .groups = "drop")
  for (tp in sort(unique(agg$time))) {
    org <- origin_for_tp(disease, tp); if (is.na(org)) next
    win <- rt[rt$date >= org & rt$date <= org + 42, ]
    if (nrow(win) < 2) next
    dR       <- R_at(rt, org + 42) - R_at(rt, org)   # net change in true R over window
    dR_range <- diff(range(win$R))                   # peak-to-trough R swing over window
    crosses1 <- as.integer(min(win$R) < 1 & max(win$R) > 1)
    for (h in sort(unique(agg$horizon))) {
      cell <- agg[agg$time == tp & agg$horizon == h, ]
      cell$gm <- gt_mult[as.character(cell$gt)]; cell <- cell[!is.na(cell$gm), ]
      if (nrow(cell) < 3) next
      slope <- unname(coef(lm(mlc ~ log(gm), data = cell))[2])
      rows[[length(rows) + 1]] <- data.frame(disease = disease, tp = tp, horizon = h,
        origin = org, dR = dR, dR_abs = abs(dR), dR_range = dR_range,
        crosses1 = crosses1, gt_slope = slope, gt_slope_abs = abs(slope))
    }
  }
}
res <- dplyr::bind_rows(rows)
res$disease <- factor(res$disease, levels = diseases)
readr::write_csv(res, here("results", "horizon_turningpoint.csv"))

s6 <- res[res$horizon == 6, ]
cat("=== resim: |change in true R over 42d window| per origin (h6) ===\n")
print(aggregate(dR_range ~ disease, s6, function(x) round(quantile(x, c(0,.5,1)), 3)))
cat("\n=== |GT slope| by |dR| tertile (h6, all diseases) ===\n")
s6$dR_bin <- cut(s6$dR_range, quantile(s6$dR_range, c(0,.33,.67,1)),
                 include.lowest = TRUE, labels = c("R~flat", "R changing", "R changing a lot"))
print(aggregate(gt_slope_abs ~ dR_bin, s6, function(x) round(c(mean = mean(x), n = length(x)), 4)))
cat("\ncor(|GT slope|, |dR over window|) at h6: ",
    round(cor(s6$dR_range, s6$gt_slope_abs, use = "complete.obs"), 3), "\n")
cat("cor within ebola+covid only:            ",
    round(with(s6[s6$disease != "cholera", ], cor(dR_range, gt_slope_abs)), 3), "\n")

## Figure: |GT slope| vs change in true R over the forecast window (the resim analog of
## const/bend/turn). Colour by disease; ring the origins where R crosses 1.
p <- ggplot2::ggplot(s6, ggplot2::aes(dR_range, gt_slope_abs)) +
  ggplot2::geom_smooth(method = "lm", se = TRUE, colour = "black", linewidth = 0.5) +
  ggplot2::geom_point(ggplot2::aes(colour = disease, shape = factor(crosses1)), size = 2.6) +
  ggplot2::scale_shape_manual(values = c(`0` = 16, `1` = 17),
                              labels = c("R stays one side of 1", "R crosses 1")) +
  ggplot2::scale_colour_viridis_d(end = 0.9) +
  ggplot2::labs(x = "change in true R over 42d forecast window (peak-to-trough)",
                y = "|GT slope of log-CRPS| at 6 weeks", shape = "",
                title = "Resim: GT sensitivity scales with how much R changes, not with R=1") +
  ggplot2::theme_bw()
ggplot2::ggsave(here("results", "fig_horizon_turningpoint.png"), p, width = 8.5, height = 5, dpi = 150)
message("Saved: results/fig_horizon_turningpoint.png")
