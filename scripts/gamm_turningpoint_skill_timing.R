## How the cost of delay misspecification in forecast skill evolves around a change in
## R. Step trajectories only: the change happens on a day fixed by the scenario design,
## so the time from the change to each origin is known in advance and does not depend
## on the observed outcome. Every origin is scored and none is selected on what
## happened, so the scores stay proper.
##
##   log-scale CRPS ~ misspecification terms (over, under, oneday per axis), each with
##                    its own coefficient per scenario and origin time
##                  + horizon + disease random effect + origin random effect
## Origins sit at eleven fixed times relative to the change, so time enters as a
## factor rather than a smooth: the data change abruptly once the change reaches
## them, and a smooth would spread that jump across the change. Flat trajectories at
## the same origin times give the time-matched baseline.

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
suppressMessages({library(dplyr); library(tidyr); library(mgcv)})

CHANGE <- 105
K <- 6
files <- list.files(here("results"), pattern = "^turningpoint_proper_(ebola|covid|cholera)_(flat|bend|turn)_rep[0-9]+\\.csv$",
                    full.names = TRUE)
files <- files[file.size(files) > 0]
d <- bind_rows(lapply(files, readr::read_csv, show_col_types = FALSE)) |>
  mutate(origin_id = factor(paste(disease, scen, rep, tp, sep = "_")),
         disease = factor(disease),
         start = as.Date(sapply(as.character(disease), function(x) as.character(startenddates[[x]][["startdate"]]))),
         days_since = as.numeric(as.Date(origin) - start) - CHANGE,
         lmult = ifelse(axis == "correct", 0, log(level)),
         oneday = as.numeric(axis != "correct" & is.na(level)),
         over = ifelse(oneday == 1, 0, pmax(lmult, 0)),
         under = ifelse(oneday == 1, 0, pmax(-lmult, 0)))

terms <- c("over_GT", "under_GT", "oneday_GT", "over_delay", "under_delay", "oneday_delay")
for (ax in c("GT", "delay")) for (t in c("over", "under", "oneday"))
  d[[paste0(t, "_", ax)]] <- ifelse(d$axis == ax, d[[t]], 0)
terms <- terms[sapply(terms, function(t) any(d[[t]] != 0))]
d <- d |> mutate(cell = factor(paste(scen, days_since, sep = ":")))

long <- d |>
  select(disease, origin_id, cell, all_of(terms), starts_with("crps_log_h")) |>
  pivot_longer(starts_with("crps_log_h"), names_to = "horizon", values_to = "y",
               names_prefix = "crps_log_h") |>
  mutate(horizon = factor(horizon))

form <- as.formula(paste(
  "y ~ cell + horizon +", paste0(terms, ":cell", collapse = " + "),
  "+ s(disease, bs = 're') + s(origin_id, bs = 're')"))
m <- bam(form, data = long, method = "fREML", discrete = TRUE)
saveRDS(m, here("results", "gamm_turningpoint_skill_timing.rds"))

## effect of a doubling, a halving and of the degenerate level on log CRPS per
## scenario and origin time, and for bend and turn its excess over flat at that time
setting <- list(`doubling (too long)` = c("over", log(2)), `halving (too short)` = c("under", log(2)),
                `one-day GT or no delay` = c("oneday", 1))
grid <- d |> distinct(scen, days_since, cell) |> arrange(scen, days_since)
base <- grid |> mutate(horizon = factor("1", levels = levels(long$horizon)),
                       disease = long$disease[1], origin_id = long$origin_id[1])
for (v in terms) base[[v]] <- 0
re_terms <- c("s(disease)", "s(origin_id)")
X0 <- predict(m, base, type = "lpmatrix", exclude = re_terms)
flat_row <- match(paste("flat", grid$days_since), paste(grid$scen, grid$days_since))
out <- list()
for (nm in names(setting)) for (ax in c("GT", "delay")) {
  t <- paste0(setting[[nm]][1], "_", ax)
  if (!t %in% terms) next
  nd <- base; nd[[t]] <- as.numeric(setting[[nm]][2])
  Xd <- predict(m, nd, type = "lpmatrix", exclude = re_terms) - X0
  Xx <- Xd - Xd[flat_row, , drop = FALSE]
  ci <- function(X) {
    est <- as.numeric(X %*% coef(m)); se <- sqrt(rowSums((X %*% vcov(m)) * X))
    cbind(est = est, lower = est - 1.96 * se, upper = est + 1.96 * se)
  }
  e <- ci(Xd); x <- ci(Xx)
  out[[length(out) + 1]] <- data.frame(axis = ax, contrast = nm, scen = grid$scen,
    days_since = grid$days_since, est = e[, "est"], lower = e[, "lower"], upper = e[, "upper"],
    excess_over_flat = x[, "est"], excess_lower = x[, "lower"], excess_upper = x[, "upper"])
}
eff <- bind_rows(out)
readr::write_csv(eff, here("results", "gamm_turningpoint_skill_timing.csv"))

cat("\n=== effect on log-scale CRPS (positive = worse), and excess over flat at the same time ===\n")
print(eff |> filter(days_since >= -35) |>
        mutate(across(est:excess_upper, ~ round(.x, 2))) |>
        arrange(contrast, axis, scen, days_since), row.names = FALSE)

p <- ggplot2::ggplot(eff, ggplot2::aes(days_since, est, colour = scen)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
  ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
  ggplot2::geom_pointrange(ggplot2::aes(ymin = lower, ymax = upper),
                           position = ggplot2::position_dodge(width = 5), size = 0.2) +
  ggplot2::geom_line(position = ggplot2::position_dodge(width = 5)) +
  ggplot2::facet_grid(contrast ~ axis, scales = "free_y") +
  ggplot2::labs(x = "days from the change in R to the forecast origin",
                y = "change in log-scale CRPS from misspecification", colour = NULL) +
  ggplot2::theme_bw()
ggplot2::ggsave(here("results", "fig_turningpoint_skill_timing.png"), p, width = 8, height = 7, dpi = 150)
