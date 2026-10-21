## Score GAMM for the turning-point scores (score_turningpoint_proper.R), following the
## analysis plan's primary comparison: the misspecification level is a categorical
## effect against the correct delay, and an origin random effect absorbs the forecast
## difficulty shared by all conditions at that origin, so each level's effect is
## estimated within origin. Fitted separately per trajectory family (step, disease-
## like), delay axis and disease, over all origins.
##   primary:   log-scale CRPS at weekly horizons 1-6, with horizon as a fixed effect
##   secondary: the growth-rate scores, one value per origin and condition

library(here)
suppressMessages({library(dplyr); library(tidyr); library(mgcv)})

files <- list.files(here("results"), pattern = "^turningpoint_proper_(ebola|covid|cholera)_.*_rep[0-9]+\\.csv$",
                    full.names = TRUE)
d <- bind_rows(lapply(files, readr::read_csv, show_col_types = FALSE)) |>
  mutate(family = ifelse(scen == "resim", "disease-like", "step"),
         origin_id = factor(paste(disease, scen, rep, tp, sep = "_")),
         level = factor(ifelse(axis == "correct", "1", ifelse(is.na(level), "one-day", as.character(level))),
                        levels = c("1", "0.25", "0.8", "1.25", "2", "one-day")))
secondary <- c("es_g", grep("^(twcrps_g|twes_g|owes_g|owvs_g)_", names(d), value = TRUE))

level_effects <- function(fit) {
  co <- summary(fit)$p.table
  co <- co[grepl("^level", rownames(co)), , drop = FALSE]
  data.frame(level = sub("^level", "", rownames(co)), est = co[, 1],
             lower = co[, 1] - 1.96 * co[, 2], upper = co[, 1] + 1.96 * co[, 2])
}

cells <- d |> filter(axis != "correct") |> distinct(family, axis, disease)
out <- list()
for (i in seq_len(nrow(cells))) {
  cl <- cells[i, ]
  dd <- d |> filter(family == cl$family, disease == cl$disease, axis %in% c("correct", cl$axis)) |>
    mutate(level = droplevels(level), origin_id = droplevels(origin_id))

  prim <- dd |> select(origin_id, level, starts_with("crps_log_h")) |>
    pivot_longer(starts_with("crps_log_h"), names_to = "horizon", values_to = "log_crps",
                 names_prefix = "crps_log_h") |>
    mutate(horizon = factor(horizon))
  m <- bam(log_crps ~ level + horizon + s(origin_id, bs = "re"), data = prim, method = "fREML")
  out[[length(out) + 1]] <- cbind(cl, score = "log-scale CRPS", level_effects(m))

  for (s in secondary) {
    dd$y <- dd[[s]]
    m <- bam(y ~ level + s(origin_id, bs = "re"), data = dd, method = "fREML")
    out[[length(out) + 1]] <- cbind(cl, score = s, level_effects(m))
  }
}
eff <- bind_rows(out)
rownames(eff) <- NULL
readr::write_csv(eff, here("results", "gamm_turningpoint_proper.csv"))

cat("\n=== primary: log-scale CRPS, effect of each level vs the correct delay (positive = worse) ===\n")
print(eff |> filter(score == "log-scale CRPS") |> arrange(family, axis, disease, level) |>
        mutate(across(c(est, lower, upper), ~ signif(.x, 2))), row.names = FALSE)

cat("\n=== turning emphasis: weighted growth scores against the unweighted growth score ===\n")
print(eff |> filter(score %in% c("es_g", "twes_g_010", "owes_g_010", "owvs_g_010", "twcrps_g_010")) |>
        select(family, axis, disease, level, score, est) |>
        pivot_wider(names_from = score, values_from = est) |>
        arrange(family, axis, disease, level) |>
        mutate(across(where(is.numeric), ~ signif(.x, 2))) |> as.data.frame(), row.names = FALSE)

## Misspecification effects by the state of transmission known at the origin: for the
## step trajectories, whether the change is already inside the fitting window; for the
## disease-like ones, the tercile of the change in true R over the 42 days before the
## origin. Splitting by a label available when the forecast is made keeps the skill
## comparison free of the forecaster's dilemma, unlike splitting by trajectory
## (flat/bend/turn) or by the change still to come.
d_state <- d |>
  group_by(family, disease) |>
  mutate(state = if (first(family) == "step") {
    ifelse(dR_est > 0, "change behind", "no change yet")
  } else {
    as.character(cut(dR_est, quantile(dR_est, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     include.lowest = TRUE, labels = c("low past change", "mid past change",
                                                       "high past change")))
  }) |> ungroup()

cells_s <- d_state |> filter(axis != "correct") |> distinct(family, axis, disease, state)
out_s <- list()
for (i in seq_len(nrow(cells_s))) {
  cl <- cells_s[i, ]
  dd <- d_state |> filter(family == cl$family, disease == cl$disease, state == cl$state,
                          axis %in% c("correct", cl$axis)) |>
    mutate(level = droplevels(level), origin_id = droplevels(origin_id))
  if (nlevels(dd$origin_id) < 5 || nlevels(dd$level) < 2) next
  prim <- dd |> select(origin_id, level, starts_with("crps_log_h")) |>
    pivot_longer(starts_with("crps_log_h"), names_to = "horizon", values_to = "log_crps",
                 names_prefix = "crps_log_h") |>
    mutate(horizon = factor(horizon))
  m <- bam(log_crps ~ level + horizon + s(origin_id, bs = "re"), data = prim, method = "fREML")
  out_s[[length(out_s) + 1]] <- cbind(cl, n_origins = nlevels(dd$origin_id), level_effects(m))
}
eff_s <- bind_rows(out_s)
rownames(eff_s) <- NULL
readr::write_csv(eff_s, here("results", "gamm_turningpoint_proper_by_state.csv"))
cat("\n=== log-scale CRPS effect vs correct delay, by state known at the origin (positive = worse) ===\n")
print(eff_s |> arrange(family, axis, disease, state, level) |>
        mutate(across(c(est, lower, upper), ~ signif(.x, 2)),
               excl0 = ifelse(lower > 0, "worse", ifelse(upper < 0, "better", ""))) |>
        as.data.frame(), row.names = FALSE)
