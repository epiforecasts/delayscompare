## Does the delay choice matter at turning points? Model the sensitivity of the
## forecast to the delay, the energy distance between the forecast under each
## misspecified delay and the forecast under the correct delay at the same origin.
## No observed outcome enters the energy distance: it compares two forecasts with each
## other. Nothing is being scored against what happened, so no forecast can be
## rewarded for anticipating the outcome, and the distance can be modelled against
## any feature of the trajectory, including the change in R still to come.
##
## One model per trajectory family, across diseases and both delay axes:
##   log(energy distance) ~ misspecification level (per axis)
##                        + change in true R over the 42 days after the origin (per axis)
##                        + change in true R over the window before the origin (per axis)
##                        + disease random intercept and slopes + origin random effect
## The two R changes separate a change still ahead of the origin from one already in
## the fitting window. Effects are reported as the factor by which a change in R of
## 0.3 multiplies the energy distance.

library(here)
suppressMessages({library(dplyr); library(mgcv)})

files <- list.files(here("results"), pattern = "^turningpoint_proper_(ebola|covid|cholera)_.*_rep[0-9]+\\.csv$",
                    full.names = TRUE)
files <- files[file.size(files) > 0]
d <- bind_rows(lapply(files, readr::read_csv, show_col_types = FALSE)) |>
  filter(axis != "correct") |>
  mutate(family = ifelse(scen == "resim", "disease-like", "step"),
         disease = factor(disease),
         origin_id = factor(paste(disease, scen, rep, tp, sep = "_")),
         level_lab = factor(paste(axis, ifelse(is.na(level), "one-day GT or no delay",
                                               as.character(level)), sep = ": ")),
         ahead_GT = ifelse(axis == "GT", dR_fc, 0), ahead_delay = ifelse(axis == "delay", dR_fc, 0),
         behind_GT = ifelse(axis == "GT", dR_est, 0), behind_delay = ifelse(axis == "delay", dR_est, 0))

STEP <- 0.3
fit_sensitivity <- function(fam) {
  dd <- d |> filter(family == fam) |> mutate(origin_id = droplevels(origin_id),
                                             level_lab = droplevels(level_lab))
  slopes <- c("ahead_GT", "ahead_delay", "behind_GT", "behind_delay")
  slopes <- slopes[sapply(slopes, function(t) any(dd[[t]] != 0))]
  form <- as.formula(paste("log(ed) ~ level_lab +", paste(slopes, collapse = " + "),
                           "+ s(disease, bs = 're') +",
                           paste0("s(disease, ", slopes, ", bs = 're')", collapse = " + "),
                           "+ s(origin_id, bs = 're')"))
  m <- bam(form, data = dd, method = "fREML", discrete = TRUE)
  ## each term alone, plus a change spanning the origin (ahead and behind together),
  ## which matters where the two are correlated, as on the smooth trajectories
  combos <- c(as.list(stats::setNames(slopes, slopes)),
              list(`ahead+behind_GT` = c("ahead_GT", "behind_GT"),
                   `ahead+behind_delay` = c("ahead_delay", "behind_delay")))
  combos <- combos[sapply(combos, function(v) all(v %in% slopes))]
  b <- coef(m); V <- vcov(m)
  bind_rows(lapply(names(combos), function(nm) {
    w <- stats::setNames(rep(0, length(b)), names(b)); w[combos[[nm]]] <- 1
    est <- sum(w * b); se <- sqrt(as.numeric(t(w) %*% V %*% w))
    data.frame(family = fam, term = nm, factor_per_0.3 = exp(STEP * est),
               lower = exp(STEP * (est - 1.96 * se)), upper = exp(STEP * (est + 1.96 * se)),
               n = nrow(dd))
  }))
}

eff <- bind_rows(fit_sensitivity("step"), fit_sensitivity("disease-like"))
readr::write_csv(eff, here("results", "gamm_turningpoint_sensitivity.csv"))
cat("\n=== factor by which a change in true R of 0.3 multiplies the energy distance ===\n")
print(eff |> mutate(across(c(factor_per_0.3, lower, upper), ~ signif(.x, 3))), row.names = FALSE)
