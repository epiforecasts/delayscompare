## GAMM analysis of the turning-point result, replacing the two-stage per-origin
## slope + bar-chart summary with a joint mixed model that estimates the delay penalty
## as a function of how much R changes over the forecast window, with proper CIs.
## Modelled on eval-by-method's mgcv::bam GAMM (log response, random-effect smooths for
## the confounders). Two models:
##   Model 1 (flagship): forecast log-CRPS on the GT axis, synthetic turning-point +
##     disease-like resim pooled onto one continuous dR axis. A varying-coefficient
##     term s(dR, by = lmult) gives the penalty per unit log-GT-multiplier as a smooth
##     function of dR: near zero where R is steady, rising where R changes.
##   Model 2 (four cells): synthetic flat/bend/turn only, one per (outcome x axis),
##     y ~ scen * lmult + random effects, giving the per-trajectory misspecification
##     slope with a model-based CI (the honest version of the bar chart).
## lmult = log delay multiplier (degenerate one-day level dropped). Response is log-CRPS
## with a Gaussian family, consistent with the project's other GAMMs (score_horizon_gamm).

library(here)
suppressMessages({library(dplyr); library(mgcv); library(ggplot2)})

mult   <- c(`1` = NA, `2` = 0.25, `3` = 0.8, `4` = 1, `5` = 1.25, `6` = 2)
diseases <- c("ebola", "covid", "cholera")

## --- true-R change over an arbitrary window, per disease (for the resim) ---
rt_col <- function(rt) if ("R" %in% names(rt)) "R" else "median"
true_R <- setNames(lapply(diseases, function(d) {
  rt <- readRDS(here("data", paste0("rt_", d, ".rds")))
  data.frame(date = as.Date(rt$date), R = rt[[rt_col(rt)]]) |> arrange(date)
}), diseases)
dR_window <- function(d, origin, days) {              # peak-to-trough R swing over window
  rt <- true_R[[d]]; w <- rt[rt$date >= origin & rt$date <= origin + days, ]
  if (nrow(w) < 2) return(NA_real_); diff(range(w$R))
}

## --- marginal effect of a unit change in lmult, with CI, via the lpmatrix ---
## (difference of two linear predictors; correct SE from the coefficient covariance)
marg <- function(fit, hi, lo) {
  Xh <- predict(fit, hi, type = "lpmatrix"); Xl <- predict(fit, lo, type = "lpmatrix")
  Xd <- Xh - Xl; V <- vcov(fit)
  est <- as.numeric(Xd %*% coef(fit)); se <- sqrt(rowSums((Xd %*% V) * Xd))
  data.frame(est = est, lower = est - 1.96 * se, upper = est + 1.96 * se)
}

## ================= assemble the pooled GT x forecast data (Model 1) =================
## synthetic: GT axis is inc == 4; forecast score at 6 wk; dR over the forecast window
syn <- read.csv(here("results", "turningpoint_perorigin.csv")) |>
  filter(inc == 4, gt != 1) |>
  transmute(source = "synthetic", disease, scen,
            origin_id = paste("syn", disease, scen, origin, sep = "_"),
            rep_id = paste("syn", disease, scen, rep, sep = "_"),
            lmult = log(mult[as.character(gt)]), dR = dR_fc, horizon = 6,
            y = fc_crps) |>
  filter(is.finite(y))

## resim: GT axis (inc == 4), forecasts at horizons 1-6; dR over [origin, origin+7h]
origins <- read.csv(here("results", "horizon_turningpoint.csv")) |>
  distinct(disease, tp, origin) |> mutate(origin = as.Date(origin))
resim <- read.csv(here("results", "horizon_perorigin_logcrps.csv")) |>
  filter(inc == 4, gt != 1) |>
  rename(tp = time) |> left_join(origins, by = c("disease", "tp")) |>
  filter(!is.na(origin)) |>
  rowwise() |> mutate(dR = dR_window(disease, origin, 7 * horizon)) |> ungroup() |>
  filter(!is.na(dR)) |>
  transmute(source = "resim", disease, scen = "resim",
            origin_id = paste("res", disease, tp, sep = "_"),
            rep_id = paste("res", disease, rep, sep = "_"),
            lmult = log(mult[as.character(gt)]), dR, horizon, y = log_crps) |>
  filter(is.finite(y))

d1 <- bind_rows(syn, resim) |>
  mutate(across(c(source, disease, origin_id, rep_id), factor))
message("Model 1 data: ", nrow(d1), " rows (", sum(d1$source == "synthetic"),
        " synthetic, ", sum(d1$source == "resim"), " resim); dR range ",
        paste(round(range(d1$dR), 2), collapse = "-"))

m1 <- bam(y ~ s(dR, k = 5) + s(dR, by = lmult, k = 5) + s(horizon, k = 4) +
            source + s(disease, bs = "re") + s(origin_id, bs = "re") + s(rep_id, bs = "re"),
          data = d1, method = "fREML", discrete = TRUE)
saveRDS(m1, here("results", "gamm_turningpoint_m1.rds"))

## penalty slope (d log-CRPS / d lmult) as a function of dR, at reference horizon/groups
## cap the grid at the well-supported range (sparse extreme-dR tail is smooth artifact)
dR_hi <- as.numeric(quantile(d1$dR, 0.95))
gridR <- data.frame(dR = seq(0, dR_hi, length.out = 60), horizon = 6,
                    source = factor("resim", levels = levels(d1$source)),
                    disease = factor("covid", levels = levels(d1$disease)),
                    origin_id = factor(levels(d1$origin_id)[1], levels = levels(d1$origin_id)),
                    rep_id = factor(levels(d1$rep_id)[1], levels = levels(d1$rep_id)))
slopeR <- cbind(dR = gridR$dR,
                marg(m1, transform(gridR, lmult = 1), transform(gridR, lmult = 0)))
readr::write_csv(slopeR, here("results", "gamm_penalty_vs_dR.csv"))
cat("\n=== Model 1: GT penalty slope vs dR (forecast, 6wk ref) ===\n")
print(slopeR[c(1, 20, 40, 60), ], row.names = FALSE, digits = 3)

## ================= Model 2: synthetic flat/bend/turn, four cells =================
tpall <- read.csv(here("results", "turningpoint_perorigin.csv"))
cells <- list(
  c("forecast", "GT"),    c("forecast", "delay"),
  c("Rnowcast", "GT"),    c("Rnowcast", "delay"))
scen_slopes <- list()
for (cl in cells) {
  outc <- cl[1]; axis <- cl[2]
  dd <- tpall |>
    filter(if (axis == "GT") inc == 4 else gt == 4) |>
    mutate(lev = if (axis == "GT") gt else inc) |>
    filter(lev != 1) |>
    transmute(disease, scen = factor(scen, levels = c("flat", "bend", "turn")), rep, origin,
              lmult = log(mult[as.character(lev)]),
              y = if (outc == "forecast") fc_crps else R_crps,
              origin_id = factor(paste(disease, scen, origin, sep = "_")),
              rep_id = factor(paste(disease, rep, sep = "_")),
              disease = factor(disease)) |>
    filter(is.finite(y))
  m <- bam(y ~ scen * lmult + s(disease, bs = "re") + s(origin_id, bs = "re") +
             s(rep_id, bs = "re"), data = dd, method = "fREML", discrete = TRUE)
  ## per-scenario slope in lmult, with CI
  base <- dd[1, ]
  sl <- do.call(rbind, lapply(levels(dd$scen), function(s) {
    hi <- transform(base, scen = factor(s, levels = levels(dd$scen)), lmult = 1)
    lo <- transform(base, scen = factor(s, levels = levels(dd$scen)), lmult = 0)
    cbind(outcome = outc, axis = axis, scen = s, marg(m, hi, lo))
  }))
  scen_slopes[[paste(outc, axis)]] <- sl
}
S2 <- bind_rows(scen_slopes)
S2$scen <- factor(S2$scen, levels = c("flat", "bend", "turn"))
readr::write_csv(S2, here("results", "gamm_scen_slopes.csv"))
cat("\n=== Model 2: per-trajectory misspecification slope (95% CI) ===\n")
print(S2, row.names = FALSE, digits = 3)

## ================= figures =================
p1 <- ggplot(slopeR, aes(dR, est)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#7570b3") +
  geom_line(colour = "#7570b3", linewidth = 0.8) +
  labs(x = "change in true R over the forecast window",
       y = "generation-time penalty\n(d log-CRPS per unit log-multiplier)",
       title = "GT misspecification penalty rises with how much R changes (GAMM, 6-wk forecast)") +
  theme_bw()
ggsave(here("results", "fig_gamm_penalty_vs_dR.png"), p1, width = 8, height = 5, dpi = 150)

p2 <- ggplot(S2, aes(scen, est, colour = scen)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_grid(outcome ~ axis, scales = "free_y") +
  scale_colour_manual(values = c(flat = "#1b9e77", bend = "#d95f02", turn = "#7570b3"),
                      guide = "none") +
  labs(x = "trajectory", y = "misspecification slope (d log-CRPS per unit log-multiplier, 95% CI)",
       title = "Per-trajectory delay penalty with model-based CIs") +
  theme_bw()
ggsave(here("results", "fig_gamm_scen_slopes.png"), p2, width = 8, height = 6, dpi = 150)
message("Saved: fig_gamm_penalty_vs_dR.png, fig_gamm_scen_slopes.png")
