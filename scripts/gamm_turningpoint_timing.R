## When, relative to a change in R, is the forecast most sensitive to the delay?
## Step trajectories only: the change happens on a known day, so each origin has a
## time since the change (negative before it, positive after). The energy distance
## between the forecast under a misspecified and under the correct delay compares
## two forecasts and uses no observed outcome, so it can be modelled against this
## time freely.
##
##   log(energy distance) ~ misspecification level (per axis)
##                        + smooth in time since the change, per axis and size of
##                          change (bend: 1.3 to 1.0; turn: 1.3 to 0.7)
##                        + disease random effect + origin random effect
## The smooths enter only for bend and turn origins (numeric by-variables), so flat
## origins set the baseline and each curve is the log factor by which sensitivity
## exceeds that of a flat trajectory at the same misspecification level.

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
suppressMessages({library(dplyr); library(mgcv)})

CHANGE <- 105
files <- list.files(here("results"), pattern = "^turningpoint_proper_(ebola|covid|cholera)_(flat|bend|turn)_rep[0-9]+\\.csv$",
                    full.names = TRUE)
files <- files[file.size(files) > 0]
d <- bind_rows(lapply(files, readr::read_csv, show_col_types = FALSE)) |>
  filter(axis != "correct") |>
  mutate(disease = factor(disease),
         origin_id = factor(paste(disease, scen, rep, tp, sep = "_")),
         level_lab = factor(paste(axis, ifelse(is.na(level), "one-day GT or no delay",
                                               as.character(level)), sep = ": ")),
         start = as.Date(sapply(as.character(disease), function(x) as.character(startenddates[[x]][["startdate"]]))),
         days_since = as.numeric(as.Date(origin) - start) - CHANGE)
for (ax in c("GT", "delay")) for (sc in c("bend", "turn"))
  d[[paste0("chg_", ax, "_", sc)]] <- as.numeric(d$axis == ax & d$scen == sc)

m <- bam(log(ed) ~ level_lab +
           s(days_since, by = chg_GT_bend, k = 6) + s(days_since, by = chg_GT_turn, k = 6) +
           s(days_since, by = chg_delay_bend, k = 6) + s(days_since, by = chg_delay_turn, k = 6) +
           s(disease, bs = "re") + s(origin_id, bs = "re"),
         data = d, method = "fREML")
saveRDS(m, here("results", "gamm_turningpoint_timing.rds"))

## curves: factor relative to a flat trajectory, by days since the change
grid <- expand.grid(days_since = seq(-100, 100, by = 2), axis = c("GT", "delay"),
                    scen = c("bend", "turn"), stringsAsFactors = FALSE)
for (ax in c("GT", "delay")) for (sc in c("bend", "turn"))
  grid[[paste0("chg_", ax, "_", sc)]] <- as.numeric(grid$axis == ax & grid$scen == sc)
grid$level_lab <- d$level_lab[1]; grid$disease <- d$disease[1]; grid$origin_id <- d$origin_id[1]
keep <- grep("^s\\(days_since\\)", sapply(m$smooth, `[[`, "label"))
Xp <- predict(m, grid, type = "lpmatrix")
cols <- unlist(lapply(m$smooth[keep], function(s) s$first.para:s$last.para))
Xs <- Xp; Xs[, setdiff(seq_len(ncol(Xp)), cols)] <- 0
est <- as.numeric(Xs %*% coef(m)); se <- sqrt(rowSums((Xs %*% vcov(m)) * Xs))
grid <- grid |> mutate(factor = exp(est), lower = exp(est - 1.96 * se), upper = exp(est + 1.96 * se)) |>
  select(axis, scen, days_since, factor, lower, upper)
## keep only times where origins exist, so the curves are not extrapolated
obs_range <- d |> filter(scen != "flat") |> group_by(scen) |>
  summarise(lo = min(days_since), hi = max(days_since))
grid <- grid |> left_join(obs_range, by = "scen") |> filter(days_since >= lo, days_since <= hi) |>
  select(-lo, -hi)
readr::write_csv(grid, here("results", "gamm_turningpoint_timing.csv"))

cat("\n=== sensitivity relative to a flat trajectory, by days since the change ===\n")
print(grid |> filter(days_since %% 14 == 0) |>
        mutate(across(c(factor, lower, upper), ~ signif(.x, 2))) |> as.data.frame(), row.names = FALSE)
cat("\n=== where each curve peaks ===\n")
print(grid |> group_by(axis, scen) |> slice_max(factor, n = 1) |>
        mutate(across(c(factor, lower, upper), ~ signif(.x, 2))) |> as.data.frame(), row.names = FALSE)

## origins actually observed relative to the change, per disease
cat("\n=== days since the change at which origins sit (bend/turn) ===\n")
print(d |> filter(scen != "flat") |> distinct(disease, days_since) |> group_by(disease) |>
        summarise(days = paste(sort(unique(days_since)), collapse = " ")) |> as.data.frame())

p <- ggplot2::ggplot(grid, ggplot2::aes(days_since, factor, colour = scen, fill = scen)) +
  ggplot2::geom_hline(yintercept = 1, linetype = 2) +
  ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.2, colour = NA) +
  ggplot2::geom_line() +
  ggplot2::scale_y_log10() +
  ggplot2::facet_wrap(~ axis) +
  ggplot2::labs(x = "days from the change in R to the forecast origin",
                y = "sensitivity to the delay, relative to a flat trajectory",
                colour = NULL, fill = NULL) +
  ggplot2::theme_bw()
ggplot2::ggsave(here("results", "fig_turningpoint_timing.png"), p, width = 8, height = 4, dpi = 150)
