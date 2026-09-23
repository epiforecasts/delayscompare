## GAMM version of the weekly learn-vs-impose comparison: does estimating the
## generation time (vs imposing the literature value) change forecast CRPS, with a
## real effect size and CI rather than eyeballing overlapping lines? `condition` is
## a categorical fixed effect (literature/estimated); an origin-level random effect
## absorbs the (large) origin-to-origin forecast difficulty common to both arms of
## the pair, so the condition contrast is estimated within-origin. A second model
## tests whether the CRPS gap scales with how far the posterior moved from the
## literature prior (the "does more learning matter more" question).

library(here)
suppressMessages({library(dplyr); library(tidyr); library(mgcv); library(ggplot2)})

dat <- read.csv(here("results", "casestudy_learn_vs_impose_weekly.csv"))
dat$disease <- factor(dat$disease, levels = c("ebola", "covid", "cholera"))
dat$origin_id <- factor(paste(dat$disease, dat$tp, sep = "_"))
dat$origin <- as.Date(dat$origin)
dat$gt_gap <- abs(dat$gt_posterior_mean - dat$gt_literature)   # how far the posterior moved

## long format: one row per (origin, condition)
long <- bind_rows(
  dat |> transmute(disease, origin_id, origin, tp, gt_gap,
                   condition = "literature", log_crps = crps_literature),
  dat |> transmute(disease, origin_id, origin, tp, gt_gap,
                   condition = "estimated", log_crps = crps_learn)
) |> filter(is.finite(log_crps))
long$condition <- factor(long$condition, levels = c("literature", "estimated"))
message("Paired long-format rows: ", nrow(long), " (",
        sum(long$condition == "literature"), " literature, ",
        sum(long$condition == "estimated"), " estimated)")

## --- Model 1: condition x disease, paired by origin ---
m1 <- bam(log_crps ~ condition * disease + s(disease, origin_id, bs = "re"),
          data = long, method = "fREML", discrete = TRUE)
saveRDS(m1, here("results", "gamm_learn_vs_impose_m1.rds"))

## marginal condition effect per disease (estimated - literature), with CI via lpmatrix
marg <- function(fit, hi, lo) {
  Xh <- predict(fit, hi, type = "lpmatrix"); Xl <- predict(fit, lo, type = "lpmatrix")
  Xd <- Xh - Xl; V <- vcov(fit)
  est <- as.numeric(Xd %*% coef(fit)); se <- sqrt(rowSums((Xd %*% V) * Xd))
  data.frame(est = est, lower = est - 1.96 * se, upper = est + 1.96 * se)
}
ref <- long[1, ]
grid <- expand.grid(disease = levels(long$disease))
grid$origin_id <- factor(levels(long$origin_id)[1], levels = levels(long$origin_id))
hi <- transform(grid, condition = factor("estimated", levels = levels(long$condition)))
lo <- transform(grid, condition = factor("literature", levels = levels(long$condition)))
eff <- cbind(disease = grid$disease, marg(m1, hi, lo))
readr::write_csv(eff, here("results", "gamm_learn_vs_impose_effect.csv"))
cat("\n=== Model 1: estimated - literature log-CRPS, by disease (95% CI) ===\n")
print(eff, digits = 3)

## --- Model 2: does the CRPS gap scale with how far the posterior moved? ---
## (estimated-condition rows only; gt_gap = |posterior - literature| at that origin)
long2 <- long |> filter(condition == "estimated") |>
  left_join(dat |> transmute(origin_id, delta_crps = crps_learn - crps_literature),
            by = "origin_id")
m2 <- bam(delta_crps ~ s(gt_gap, by = disease, k = 4) + s(disease, bs = "re"),
          data = long2, method = "fREML", discrete = TRUE)
saveRDS(m2, here("results", "gamm_learn_vs_impose_m2.rds"))
cat("\n=== Model 2: does |posterior - literature| predict the CRPS gap? ===\n")
print(summary(m2)$s.table)

## --- figure: Model 1 marginal effect per disease ---
p1 <- ggplot(eff, aes(disease, est)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_pointrange(aes(ymin = lower, ymax = upper), colour = "#7570b3", linewidth = 0.8) +
  labs(x = NULL, y = "estimated - literature (log-CRPS, 95% CI)",
       title = "Does estimating the generation time change forecast CRPS?",
       subtitle = "GAMM, paired by origin (95% CI); positive = estimating is worse") +
  theme_bw()
ggsave(here("results", "fig_gamm_learn_vs_impose_effect.png"), p1, width = 6.5, height = 5, dpi = 150)

## --- figure: Model 2 smooth, CRPS gap vs posterior movement ---
gridg <- expand.grid(disease = levels(long2$disease),
                     gt_gap = seq(0, quantile(long2$gt_gap, 0.95, na.rm = TRUE), length.out = 40))
pr <- predict(m2, newdata = gridg, se.fit = TRUE, exclude = "s(disease)")
gridg$fit <- pr$fit; gridg$lower <- pr$fit - 1.96 * pr$se.fit; gridg$upper <- pr$fit + 1.96 * pr$se.fit
p2 <- ggplot(gridg, aes(gt_gap, fit)) +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = 0.2, fill = "#d95f02") +
  geom_line(colour = "#d95f02") +
  facet_wrap(~ disease, scales = "free_x") +
  labs(x = "|estimated posterior - literature value| (days)",
       y = "estimated - literature (log-CRPS)",
       title = "Does the CRPS gap scale with how far the posterior moved?") +
  theme_bw()
ggsave(here("results", "fig_gamm_learn_vs_impose_gap.png"), p2, width = 9, height = 4, dpi = 150)
message("Saved: fig_gamm_learn_vs_impose_effect.png, fig_gamm_learn_vs_impose_gap.png")
