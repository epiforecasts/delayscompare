## Forecast sensitivity to the delay against a change in transmission that a forecaster
## can see at the origin. The true change in R is latent, so instead each origin is
## described by the observed change in growth: the mean weekly growth of reported cases
## over the last four weeks before the origin minus that over the four weeks before
## those, |(lW0 - lW-4)/4 - (lW-4 - lW-8)/4| with lWk = log(W_k + 1) and W_0 the case
## total in the week ending at the origin. The span of four weeks matches the time
## after a change at which sensitivity peaks on the step trajectories.
##
##   log(energy distance) ~ misspecification level (per axis)
##                        + smooth in the observed growth change (per axis)
##                        + disease random effect + origin random effect
## fitted per trajectory family. The energy distance compares two forecasts and uses
## no outcome, so any covariate is admissible; this one is also available in real time.

library(here)
suppressMessages({library(dplyr); library(mgcv)})

files <- list.files(here("results"), pattern = "^turningpoint_proper_(ebola|covid|cholera)_.*_rep[0-9]+\\.csv$",
                    full.names = TRUE)
files <- files[file.size(files) > 0]
d <- bind_rows(lapply(files, readr::read_csv, show_col_types = FALSE)) |>
  filter(axis != "correct") |>
  mutate(family = ifelse(scen == "resim", "disease-like", "step"),
         disease = factor(disease), axis_f = factor(axis),
         origin_id = factor(paste(disease, scen, rep, tp, sep = "_")),
         level_lab = factor(paste(axis, ifelse(is.na(level), "one-day GT or no delay",
                                               as.character(level)), sep = ": ")))

## observed growth change per origin, from reported cases up to the origin only
origins <- d |> distinct(disease, scen, rep, tp, origin)
obs_change <- function(disease, scen, rep, origin) {
  infix <- if (scen == "resim") "" else paste0("_", scen)
  x <- readRDS(here("data", paste0(disease, "_sim_data", infix, "_rep", rep, ".rds")))
  x <- x[x$variable == "reported_cases", ]
  day <- as.numeric(as.Date(x$date) - as.Date(origin))
  lW <- sapply(0:-8, function(k) {
    v <- x$value[day %in% (7 * (k - 1) + 1:7)]
    if (length(v) == 7) log1p(sum(v)) else NA_real_
  })
  ## lW[1] is week 0 (ending at the origin), lW[5] week -4, lW[9] week -8
  abs((lW[1] - lW[5]) / 4 - (lW[5] - lW[9]) / 4)
}
origins$obs_change <- mapply(obs_change, as.character(origins$disease), origins$scen,
                             origins$rep, as.character(origins$origin))
d <- d |> left_join(origins |> select(disease, scen, rep, tp, obs_change),
                    by = c("disease", "scen", "rep", "tp"))
message("origins without eight observed weeks (dropped): ",
        sum(is.na(origins$obs_change)), " of ", nrow(origins))

cat("\n=== how the observed growth change tracks the true change in R before the origin ===\n")
print(origins |> left_join(d |> distinct(disease, scen, rep, tp, dR_est), by = c("disease", "scen", "rep", "tp")) |>
        mutate(family = ifelse(scen == "resim", "disease-like", "step")) |>
        group_by(family) |>
        summarise(cor_with_true_past_change = cor(obs_change, dR_est, use = "complete.obs"),
                  median_obs_change = median(obs_change, na.rm = TRUE),
                  q90 = quantile(obs_change, 0.9, na.rm = TRUE)) |> as.data.frame())

fit_observed <- function(fam) {
  dd <- d |> filter(family == fam, !is.na(obs_change)) |>
    mutate(origin_id = droplevels(origin_id), level_lab = droplevels(level_lab),
           axis_f = droplevels(axis_f))
  m <- bam(log(ed) ~ level_lab + s(obs_change, by = axis_f, k = 5) +
             s(disease, bs = "re") + s(origin_id, bs = "re"), data = dd, method = "fREML")
  ## curve per axis relative to its value at no observed change
  qs <- quantile(dd$obs_change, c(0.5, 0.75, 0.9, 0.95))
  grid <- expand.grid(obs_change = c(0, unname(qs)), axis_f = levels(dd$axis_f))
  grid$level_lab <- dd$level_lab[1]; grid$disease <- dd$disease[1]; grid$origin_id <- dd$origin_id[1]
  X <- predict(m, grid, type = "lpmatrix", exclude = c("s(disease)", "s(origin_id)"))
  out <- list()
  for (ax in levels(dd$axis_f)) {
    rows <- which(grid$axis_f == ax)
    X0 <- X[rows[1], , drop = FALSE]
    for (r in rows[-1]) {
      Xd <- X[r, , drop = FALSE] - X0
      est <- as.numeric(Xd %*% coef(m)); se <- sqrt(as.numeric(Xd %*% vcov(m) %*% t(Xd)))
      out[[length(out) + 1]] <- data.frame(family = fam, axis = ax,
        quantile = names(qs)[match(grid$obs_change[r], qs)], obs_change = grid$obs_change[r],
        factor = exp(est), lower = exp(est - 1.96 * se), upper = exp(est + 1.96 * se))
    }
  }
  bind_rows(out)
}

eff <- bind_rows(fit_observed("step"), fit_observed("disease-like"))
readr::write_csv(eff, here("results", "gamm_turningpoint_observed.csv"))
cat("\n=== sensitivity at a given observed growth change, relative to none ===\n")
print(eff |> mutate(across(c(obs_change, factor, lower, upper), ~ signif(.x, 3))), row.names = FALSE)
