## One score GAMM per trajectory family, across diseases and both delay axes,
## estimating the overall effect of delay misspecification on log-scale CRPS instead of
## one effect per disease and level. For each axis (generation interval, observation
## delay) misspecification enters as
##   over:   log of the multiplier when it is above 1 (delay assumed too long)
##   under:  minus the log of the multiplier when it is below 1 (assumed too short)
##   oneday: the degenerate level (one-day generation interval, or no observation delay)
## each interacting with the state of transmission known at the origin (step
## trajectories: change already in the fitting window or not; disease-like: tercile of
## the change in true R over the 42 days before the origin). The correct delay is the
## shared reference for both axes. Disease enters as a random intercept and random
## slopes on the misspecification terms, so the fixed effects are averages over
## diseases; an origin random effect pairs the conditions at each origin.

library(here)
suppressMessages({library(dplyr); library(tidyr); library(mgcv)})

files <- list.files(here("results"), pattern = "^turningpoint_proper_(ebola|covid|cholera)_.*_rep[0-9]+\\.csv$",
                    full.names = TRUE)
d <- bind_rows(lapply(files, readr::read_csv, show_col_types = FALSE)) |>
  mutate(family = ifelse(scen == "resim", "disease-like", "step"),
         origin_id = factor(paste(disease, scen, rep, tp, sep = "_")),
         disease = factor(disease),
         lmult = ifelse(axis == "correct", 0, log(level)),
         oneday = as.numeric(axis != "correct" & is.na(level)),
         over = ifelse(oneday == 1, 0, pmax(lmult, 0)),
         under = ifelse(oneday == 1, 0, pmax(-lmult, 0))) |>
  group_by(family, disease) |>
  mutate(state = if (first(family) == "step") {
    ifelse(dR_est > 0, "change behind", "no change yet")
  } else {
    as.character(cut(dR_est, quantile(dR_est, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     include.lowest = TRUE, labels = c("low", "mid", "high")))
  }) |> ungroup()

terms <- c("over_GT", "under_GT", "oneday_GT", "over_delay", "under_delay", "oneday_delay")
for (ax in c("GT", "delay")) for (t in c("over", "under", "oneday"))
  d[[paste0(t, "_", ax)]] <- ifelse(d$axis == ax, d[[t]], 0)

## score = "crps_log" fits the log-scale CRPS over weekly horizons (horizon as a fixed
## effect); any other score column is one value per origin and condition.
fit_overall <- function(fam, score = "crps_log") {
  dd <- d |> filter(family == fam) |>
    mutate(state = factor(state), origin_id = droplevels(origin_id))
  present <- terms[sapply(terms, function(t) any(dd[[t]] != 0))]
  by_h <- score == "crps_log"
  long <- if (by_h) {
    dd |> select(disease, origin_id, state, all_of(present), starts_with("crps_log_h")) |>
      pivot_longer(starts_with("crps_log_h"), names_to = "horizon", values_to = "y",
                   names_prefix = "crps_log_h") |>
      mutate(horizon = factor(horizon))
  } else {
    dd |> mutate(y = .data[[score]], horizon = factor("1")) |>
      select(disease, origin_id, state, all_of(present), horizon, y)
  }
  re_terms <- c("s(disease)", paste0("s(disease,", present, ")"), "s(origin_id)")
  form <- as.formula(paste(
    "y ~ state * (", paste(present, collapse = " + "), ")", if (by_h) "+ horizon", "+",
    "s(disease, bs = 're') +",
    paste0("s(disease, ", present, ", bs = 're')", collapse = " + "),
    "+ s(origin_id, bs = 're')"))
  m <- bam(form, data = long, method = "fREML", discrete = TRUE)

  ## effect of a doubling, a halving and of the degenerate level, within each state,
  ## plus the difference between the observation delay and the generation interval
  base <- data.frame(state = levels(long$state), horizon = factor("1", levels = levels(long$horizon)),
                     disease = long$disease[1], origin_id = long$origin_id[1])
  for (t in present) base[[t]] <- 0
  setting <- list(`doubling (too long)` = c("over", log(2)), `halving (too short)` = c("under", log(2)),
                  `one-day GT or no delay` = c("oneday", 1))
  X0 <- predict(m, base, type = "lpmatrix", exclude = re_terms)
  Xd_of <- function(term, value) {
    nd <- base; nd[[term]] <- value
    predict(m, nd, type = "lpmatrix", exclude = re_terms) - X0
  }
  summarise_contrast <- function(Xd, axis_lab, nm) {
    est <- as.numeric(Xd %*% coef(m)); se <- sqrt(rowSums((Xd %*% vcov(m)) * Xd))
    data.frame(family = fam, score = score, axis = axis_lab, state = base$state, contrast = nm,
               est = est, lower = est - 1.96 * se, upper = est + 1.96 * se)
  }
  out <- list()
  for (nm in names(setting)) {
    tt <- setting[[nm]][1]; val <- as.numeric(setting[[nm]][2])
    Xs <- list()
    for (ax in c("GT", "delay")) {
      term <- paste0(tt, "_", ax)
      if (!term %in% present) next
      Xs[[ax]] <- Xd_of(term, val)
      out[[length(out) + 1]] <- summarise_contrast(Xs[[ax]], ax, nm)
    }
    if (length(Xs) == 2)
      out[[length(out) + 1]] <- summarise_contrast(Xs$delay - Xs$GT, "delay minus GT", nm)
  }
  bind_rows(out)
}

eff <- bind_rows(fit_overall("step"), fit_overall("disease-like"))
readr::write_csv(eff, here("results", "gamm_turningpoint_overall.csv"))
cat("\n=== overall effect on log-scale CRPS, averaged over diseases (positive = worse) ===\n")
print(eff |> mutate(across(c(est, lower, upper), ~ signif(.x, 2))), row.names = FALSE)

## Turning points: the same model on the growth-rate scores. The threshold-weighted
## scores only count forecast and observed growth paths that depart from the growth
## known at the origin. The share of an effect carried by the weighted score, set
## against the share of the score itself carried by it under the correct delay, shows
## whether misspecification costs more at turns than elsewhere.
growth_scores <- c("es_g", grep("^(twes_g|twcrps_g|owes_g)_", names(d), value = TRUE))
eff_g <- bind_rows(lapply(c("step", "disease-like"), function(fam)
  bind_rows(lapply(growth_scores, function(sc) fit_overall(fam, sc)))))
readr::write_csv(eff_g, here("results", "gamm_turningpoint_overall_growth.csv"))

score_share <- d |> filter(axis == "correct") |>
  group_by(family, state) |>
  summarise(across(all_of(growth_scores[-1]), ~ mean(.x) / mean(es_g)), .groups = "drop") |>
  pivot_longer(-c(family, state), names_to = "score", values_to = "score_share")
eff_share <- eff_g |> filter(axis %in% c("GT", "delay")) |>
  left_join(eff_g |> filter(score == "es_g", axis %in% c("GT", "delay")) |>
              select(family, axis, state, contrast, es_g_est = est),
            by = c("family", "axis", "state", "contrast")) |>
  filter(score != "es_g") |>
  mutate(effect_share = est / es_g_est) |>
  left_join(score_share, by = c("family", "state", "score"))
readr::write_csv(eff_share, here("results", "gamm_turningpoint_overall_turning_share.csv"))

cat("\n=== unweighted growth energy score: overall effect (positive = worse) ===\n")
print(eff_g |> filter(score == "es_g") |> select(-score) |>
        mutate(across(c(est, lower, upper), ~ signif(.x, 2))), row.names = FALSE)
cat("\n=== threshold-weighted growth energy score (c = 0.1): effect, its share of the",
    "unweighted effect, and the share of the score itself ===\n")
print(eff_share |> filter(score == "twes_g_010") |>
        select(family, axis, state, contrast, est, lower, upper, effect_share, score_share) |>
        mutate(across(c(est, lower, upper, effect_share, score_share), ~ signif(.x, 2))),
      row.names = FALSE)
