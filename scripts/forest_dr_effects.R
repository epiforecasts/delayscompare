## Forest-plot comparison of the ΔR (change-in-R) effect across every analysis in
## the "generative world" claim, WITHOUT pooling raw data into one super-model
## (see docs/results.md for why: condition means different things across analyses,
## and synthetic/resim vs real case-study data serve different epistemic roles --
## mechanism isolation vs field validation). Each source keeps its own model and
## its own data; here we extract one genuinely comparable summary per source --
## a Pearson correlation coefficient (r, 95% CI), unitless and bounded to
## [-1, 1] regardless of the source's own scale -- and plot them side by side,
## grouped by data family.

library(here)
suppressMessages({library(dplyr); library(ggplot2); library(mgcv)})

## Pearson r + 95% CI via Fisher's z-transform. Unlike a raw slope, r is unitless
## and bounded to [-1, 1] regardless of the underlying variables' scale, so it
## genuinely supports comparing magnitude across sources with different units --
## a raw slope forest plot only supports comparing sign/significance.
cor_ci <- function(x, y, weights = NULL) {
  if (is.null(weights)) {
    r <- cor(x, y); n <- length(x)
  } else {
    ## weighted correlation, for the one source that isn't raw per-row data
    mx <- weighted.mean(x, weights); my <- weighted.mean(y, weights)
    r <- sum(weights * (x - mx) * (y - my)) /
      sqrt(sum(weights * (x - mx)^2) * sum(weights * (y - my)^2))
    n <- length(x)   # grid points, not independent observations -- flagged in the label
  }
  z <- atanh(r); se <- 1 / sqrt(n - 3)
  data.frame(r = r, lower = tanh(z - 1.96 * se), upper = tanh(z + 1.96 * se))
}

rows <- list()

## --- 1. Turning-point + resim pooled GAMM (synthetic + resim; known ground truth) ---
## Model 1 of score_gamm_turningpoint.R: GT penalty on 6wk forecast CRPS as a smooth
## function of dR (s(dR, by=lmult)). This is the one source that ISN'T raw per-row
## data: its own "r" comes from the fitted penalty curve (gamm_penalty_vs_dR.csv: dR,
## est, lower, upper, weighted by inverse-variance), not from correlating individual
## observations. Same [-1,1] scale as the other rows, but a different kind of
## quantity underneath -- flagged in the label, not hidden.
pc <- read.csv(here("results", "gamm_penalty_vs_dR.csv"))
pc$se <- (pc$upper - pc$lower) / (2 * 1.96)
rc <- cor_ci(pc$dR, pc$est, weights = 1 / pc$se^2)
rows[["Turning-point + resim GAMM\n(GT penalty on forecast, 6wk; curve-derived r)"]] <- data.frame(
  family = "synthetic/resim", r = rc$r, lower = rc$lower, upper = rc$upper, reliable = TRUE)

## --- 2. Resim-only, simple check (no varying-coefficient machinery) ---
## |GT slope| vs true-R range over the 6wk forecast window, pooled across diseases,
## from investigate_turningpoints.R. Raw per-origin data, genuine Pearson r. A much
## simpler estimator, included to show the flagship result isn't an artifact of the
## GAMM's flexibility. (Column is dR_range, true-R-based; an earlier, noisier
## case-curvature-proxy version of this script used `curv_range` -- if you see that
## name anywhere, the underlying CSV is stale and needs regenerating, as happened here.)
ht <- read.csv(here("results", "horizon_turningpoint.csv"))
ht6 <- ht[ht$horizon == 6, ]
rc <- cor_ci(ht6$dR_range, ht6$gt_slope_abs)
rows[["Resim only, simple check\n(|GT slope| vs ΔR, pooled)"]] <- data.frame(
  family = "synthetic/resim", r = rc$r, lower = rc$lower, upper = rc$upper, reliable = TRUE)

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
  rc <- cor_ci(dd$dR, dd$log_crps)
  label <- paste0("Q2 real case-study: ", d, "\n(log-CRPS, all conditions pooled, unadjusted)")
  rows[[label]] <- data.frame(
    family = "real case-study", r = rc$r, lower = rc$lower, upper = rc$upper,
    reliable = d != "covid")   # covid: survivorship bias, flagged not excluded
}

dat <- bind_rows(rows, .id = "source")
dat$source <- factor(dat$source, levels = rev(dat$source))
readr::write_csv(dat, here("results", "forest_dr_effects.csv"))
cat("\n=== ΔR effect (Pearson r, 95% CI) by source ===\n")
print(as.data.frame(dat), digits = 3, row.names = FALSE)

p <- ggplot(dat, aes(r, source, colour = reliable)) +
  geom_vline(xintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(xmin = lower, xmax = upper), orientation = "y") +
  facet_grid(family ~ ., scales = "free_y", space = "free_y") +
  scale_colour_manual(values = c(`TRUE` = "black", `FALSE` = "grey60"),
                      labels = c(`TRUE` = "reliable", `FALSE` = "survivorship bias"),
                      name = NULL) +
  coord_cartesian(xlim = c(-1, 1)) +
  labs(x = "Pearson r between ΔR and outcome (unitless, comparable across rows)", y = NULL,
       title = "Does the change-in-R effect point the same way across every analysis?",
       subtitle = paste("Not a pooled model -- each source's own data/model, correlation coefficient per source.",
                         "All rows on the same [-1, 1] scale, except the top row (curve-derived, see caption).",
                         sep = "\n")) +
  theme_bw() + theme(strip.text.y = element_text(angle = 0), legend.position = "bottom")
ggsave(here("results", "fig_forest_dr_effects.png"), p, width = 10, height = 5.8, dpi = 150)
message("Saved: results/fig_forest_dr_effects.png")
