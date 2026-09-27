## Forest-plot comparison of the ΔR (change-in-R) effect across every analysis in
## the "generative world" claim, WITHOUT pooling raw data into one super-model
## (see docs/results.md for why: condition means different things across analyses,
## and synthetic/resim vs real case-study data serve different epistemic roles --
## mechanism isolation vs field validation). Each source keeps its own model and
## its own data; here we extract one genuinely comparable summary per source --
## a standardised slope (beta, 95% CI): dR and the outcome are each rescaled to
## their OWN source's mean/SD first, so the coefficient reads as "SD change in
## outcome per SD change in dR", the same footing regardless of the source's
## native units -- and plot them side by side, grouped by data family.

library(here)
suppressMessages({library(dplyr); library(ggplot2); library(mgcv)})

## Standardised slope + 95% CI: rescale x and y to mean 0/SD 1 using that
## source's OWN data, then fit the (weighted, where relevant) linear slope.
beta_ci <- function(x, y, weights = NULL) {
  zx <- as.numeric(scale(x)); zy <- as.numeric(scale(y))
  fit <- if (is.null(weights)) lm(zy ~ zx) else lm(zy ~ zx, weights = weights)
  co <- summary(fit)$coefficients["zx", ]
  data.frame(beta = co["Estimate"],
             lower = co["Estimate"] - 1.96 * co["Std. Error"],
             upper = co["Estimate"] + 1.96 * co["Std. Error"])
}

rows <- list()

## --- 1. Turning-point + resim pooled GAMM (synthetic + resim; known ground truth) ---
## Model 1 of score_gamm_turningpoint.R: GT penalty on 6wk forecast CRPS as a smooth
## function of dR (s(dR, by=lmult)). This is the one source that ISN'T raw per-row
## data: its own beta comes from the fitted penalty curve (gamm_penalty_vs_dR.csv:
## dR, est, lower, upper, weighted by inverse-variance), not from regressing
## individual observations. Same standardised-slope quantity as the other rows,
## but a different kind of input underneath -- flagged in the label, not hidden.
pc <- read.csv(here("results", "gamm_penalty_vs_dR.csv"))
pc$se <- (pc$upper - pc$lower) / (2 * 1.96)
bc <- beta_ci(pc$dR, pc$est, weights = 1 / pc$se^2)
rows[["Turning-point + resim GAMM\n(GT penalty on forecast, 6wk; curve-derived beta)"]] <- data.frame(
  family = "synthetic/resim", beta = bc$beta, lower = bc$lower, upper = bc$upper, reliable = TRUE)

## --- 2. Resim-only, simple check (no varying-coefficient machinery) ---
## |GT slope| vs true-R range over the 6wk forecast window, pooled across diseases,
## from investigate_turningpoints.R. Raw per-origin data, genuine standardised slope.
## A much simpler estimator, included to show the flagship result isn't an artifact
## of the GAMM's flexibility. (Column is dR_range, true-R-based; an earlier, noisier
## case-curvature-proxy version of this script used `curv_range` -- if you see that
## name anywhere, the underlying CSV is stale and needs regenerating, as happened here.)
ht <- read.csv(here("results", "horizon_turningpoint.csv"))
ht6 <- ht[ht$horizon == 6, ]
bc <- beta_ci(ht6$dR_range, ht6$gt_slope_abs)
rows[["Resim only, simple check\n(|GT slope| vs ΔR, pooled)"]] <- data.frame(
  family = "synthetic/resim", beta = bc$beta, lower = bc$lower, upper = bc$upper, reliable = TRUE)

## --- 3. Q2 real case-study, per disease (unknown ground truth; dR = posterior- ---
## median R from the original full-data fit, not a known truth). Tests a different
## question -- does log-CRPS itself scale with dR, pooling across all five learn/
## impose conditions -- not the interaction the synthetic side tests. Refit here
## with a LINEAR dR term (the main Q2 models use a smooth) purely to get one
## comparable coefficient; the smooth version remains what's reported as primary.
## IMPORTANT: no s(date) control here. date_num and dR are correlated in real
## outbreak data (transmission naturally settles as calendar time progresses;
## checked directly: cor(dR, date_num) = -0.20 ebola, 0.15 cholera), so including
## both in one model let date's smooth partially absorb and distort dR's own
## coefficient (a suppression artifact: the date-adjusted dR slope came out
## *negative* for both ebola and cholera, opposite the raw relationship and
## opposite every synthetic/resim result, until this was checked and removed).
## The main Q2 model (score_q2_gamm_weekly.R) keeps s(date) because its purpose is
## different: absorbing generic nuisance variation so the *condition* effect
## isn't confounded, a question that does not require dR's own sign to be
## interpretable, and that was separately checked to be robust either way. This
## script's purpose is the reverse -- reading dR's own effect -- so it uses the
## simpler, unconditional relationship, on the same footing as the resim "simple
## check" row above.
## Standardised per disease using that disease's OWN dR/log_crps baseline (mean,
## SD) -- ebola/cholera/covid span disjoint calendar periods and dR/CRPS ranges,
## so a pooled-across-disease baseline would compare each disease to a scale it
## never actually occupies.
q2 <- read.csv(here("results", "q2_perorigin_logcrps_weekly.csv"))
for (d in c("ebola", "cholera", "covid")) {
  dd <- q2[q2$disease == d & !is.na(q2$dR), ]
  bc <- beta_ci(dd$dR, dd$log_crps)
  label <- paste0("Q2 real case-study: ", d, "\n(log-CRPS, all conditions pooled, unadjusted)")
  rows[[label]] <- data.frame(
    family = "real case-study", beta = bc$beta, lower = bc$lower, upper = bc$upper,
    reliable = d != "covid")   # covid: survivorship bias, flagged not excluded
}

dat <- bind_rows(rows, .id = "source")
dat$source <- factor(dat$source, levels = rev(dat$source))
readr::write_csv(dat, here("results", "forest_dr_effects.csv"))
cat("\n=== ΔR effect (standardised slope beta, 95% CI) by source ===\n")
print(as.data.frame(dat), digits = 3, row.names = FALSE)

p <- ggplot(dat, aes(beta, source, colour = reliable)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(xmin = lower, xmax = upper), orientation = "y") +
  facet_grid(family ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey60"),
                      labels = c(`TRUE` = "reliable", `FALSE` = "survivorship bias"),
                      name = NULL) +
  labs(x = "Standardised ΔR slope (SD change in outcome per SD change in dR; each source's own baseline)", y = NULL,
       title = "Does the change-in-R effect point the same way across every analysis?",
       subtitle = paste("Not a pooled model -- each source's own data/model, standardised slope per source.",
                         "Comparable in magnitude across rows, except the top row (curve-derived, see caption).",
                         sep = "\n")) +
  theme_bw() + theme(strip.text.y = element_text(angle = 0), legend.position = "bottom")
ggsave(here("results", "fig_forest_dr_effects.png"), p, width = 10, height = 5.8, dpi = 150)
message("Saved: results/fig_forest_dr_effects.png")
