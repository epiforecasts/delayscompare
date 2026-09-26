## Forest-plot comparison of the ΔR (change-in-R) effect across every analysis in
## the "generative world" claim, WITHOUT pooling raw data into one super-model
## (see docs/results.md for why: condition means different things across analyses,
## and synthetic/resim vs real case-study data serve different epistemic roles --
## mechanism isolation vs field validation). Each source keeps its own model and
## its own scale; here we extract one comparable linear summary (slope + 95% CI)
## per source and plot them side by side, grouped by data family.

library(here)
suppressMessages({library(dplyr); library(ggplot2); library(mgcv)})

rows <- list()

## --- 1. Turning-point + resim pooled GAMM (synthetic + resim; known ground truth) ---
## Model 1 of score_gamm_turningpoint.R: GT penalty on 6wk forecast CRPS as a smooth
## function of dR (s(dR, by=lmult)). No linear coefficient to pull directly, so we
## take a weighted-least-squares linear summary through the predicted curve
## (gamm_penalty_vs_dR.csv: dR, est, lower, upper), a standard way to get a
## representative "average slope" from a smooth relationship for comparison.
pc <- read.csv(here("results", "gamm_penalty_vs_dR.csv"))
pc$se <- (pc$upper - pc$lower) / (2 * 1.96)
fit_pc <- lm(est ~ dR, data = pc, weights = 1 / se^2)
co <- summary(fit_pc)$coefficients["dR", ]
rows[["Turning-point + resim GAMM\n(GT penalty on forecast, 6wk)"]] <- data.frame(
  family = "synthetic/resim", slope = co["Estimate"],
  lower = co["Estimate"] - 1.96 * co["Std. Error"],
  upper = co["Estimate"] + 1.96 * co["Std. Error"], reliable = TRUE)

## --- 2. Resim-only, simple check (no varying-coefficient machinery) ---
## |GT slope| vs true-R range over the 6wk forecast window, pooled across diseases,
## from investigate_turningpoints.R. A much simpler estimator, included to show the
## flagship result isn't an artifact of the GAMM's flexibility.
ht <- read.csv(here("results", "horizon_turningpoint.csv"))
ht6 <- ht[ht$horizon == 6, ]
fit_ht <- lm(gt_slope_abs ~ curv_range, data = ht6)
co <- summary(fit_ht)$coefficients["curv_range", ]
rows[["Resim only, simple check\n(|GT slope| vs ΔR, pooled)"]] <- data.frame(
  family = "synthetic/resim", slope = co["Estimate"],
  lower = co["Estimate"] - 1.96 * co["Std. Error"],
  upper = co["Estimate"] + 1.96 * co["Std. Error"], reliable = TRUE)

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
q2 <- read.csv(here("results", "q2_perorigin_logcrps_weekly.csv"))
for (d in c("ebola", "cholera", "covid")) {
  dd <- q2[q2$disease == d & !is.na(q2$dR), ]
  co <- summary(lm(log_crps ~ dR, data = dd))$coefficients["dR", ]
  label <- paste0("Q2 real case-study: ", d, "\n(log-CRPS, all conditions pooled, unadjusted)")
  rows[[label]] <- data.frame(
    family = "real case-study", slope = co["Estimate"],
    lower = co["Estimate"] - 1.96 * co["Std. Error"],
    upper = co["Estimate"] + 1.96 * co["Std. Error"],
    reliable = d != "covid")   # covid: survivorship bias, flagged not excluded
}

dat <- bind_rows(rows, .id = "source")
dat$source <- factor(dat$source, levels = rev(dat$source))
readr::write_csv(dat, here("results", "forest_dr_effects.csv"))
cat("\n=== ΔR effect (linear slope, 95% CI) by source ===\n")
print(as.data.frame(dat), digits = 3, row.names = FALSE)

p <- ggplot(dat, aes(slope, source, colour = reliable)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(xmin = lower, xmax = upper), orientation = "y") +
  facet_grid(family ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey60"),
                      labels = c(`TRUE` = "reliable", `FALSE` = "survivorship bias"),
                      name = NULL) +
  labs(x = "ΔR effect (each source's own scale/units; see caption)", y = NULL,
       title = "Does the change-in-R effect point the same way across every analysis?",
       subtitle = "Not a pooled model -- each source's own model, one linear summary each. Not directly comparable in scale across rows.") +
  theme_bw() + theme(strip.text.y = element_text(angle = 0), legend.position = "bottom")
ggsave(here("results", "fig_forest_dr_effects.png"), p, width = 8.5, height = 5.5, dpi = 150)
message("Saved: results/fig_forest_dr_effects.png")
