## Summarise score_turningpoint_proper.R output, as specified in the analysis plan
## ("Turning-point evaluation without conditioning on the outcome").
##   (1) sensitivity: energy distance from the correct-delay forecast. No outcome
##       enters it, so it is summarised by trajectory and by where the change sits
##       relative to the origin (step trajectories) or by tercile of R change
##       (disease-like replicates).
##   (2) skill: paired difference, misspecified minus correct at the same origin,
##       averaged over ALL origins. Positive means the misspecified delay forecasts
##       worse. Primary: log-scale CRPS at each weekly horizon. Secondary: threshold-
##       and outcome-weighted scores on weekly growth rates, and their unweighted
##       energy score.

library(here)
library(dplyr)

files <- list.files(here("results"), pattern = "^turningpoint_proper_(ebola|covid|cholera)_.*\\.csv$",
                    full.names = TRUE)
all_rows <- bind_rows(lapply(files, readr::read_csv, show_col_types = FALSE))
message(nrow(all_rows), " rows, ",
        nrow(distinct(all_rows, disease, scen, rep, tp)), " origins")

primary_cols <- c(paste0("crps_log_h", 1:6), "crps_log_mean")
score_cols <- c(primary_cols, "es_g",
                grep("^(twcrps_g|twes_g|owes_g|owvs_g)_", names(all_rows), value = TRUE))

## (2) paired differences against the correct forecast at the same origin
pair_up <- function(d) {
  ref <- d |> filter(axis == "correct") |>
    select(disease, scen, rep, tp, all_of(score_cols)) |>
    rename_with(~ paste0(.x, "_ref"), all_of(score_cols))
  d |> filter(axis != "correct") |>
    inner_join(ref, by = c("disease", "scen", "rep", "tp")) |>
    mutate(level_lab = ifelse(is.na(level), "one-day", format(level)))
}
diff_summary <- function(p, ...) {
  long <- bind_rows(lapply(score_cols, function(s)
    p |> transmute(..., score = s, diff = .data[[s]] - .data[[paste0(s, "_ref")]])))
  long |> group_by(..., score) |>
    summarise(n = n(), mean = mean(diff), lower = mean - 1.96 * sd(diff) / sqrt(n()),
              upper = mean + 1.96 * sd(diff) / sqrt(n()), .groups = "drop")
}

## step trajectories
step <- all_rows |> filter(scen != "resim") |>
  mutate(window = factor(case_when(dR_fc > 0 ~ "change ahead", dR_est > 0 ~ "change behind",
                                   TRUE ~ "no change"),
                         levels = c("no change", "change ahead", "change behind")),
         scen = factor(scen, levels = c("flat", "bend", "turn")))

mis <- step |> filter(axis %in% c("GT", "delay"), !is.na(level))
sens <- bind_rows(
  mis |> group_by(axis, stratum = as.character(scen)) |>
    summarise(n = n(), se = sd(ed) / sqrt(n()), ed = mean(ed), .groups = "drop") |>
    mutate(by = "trajectory"),
  mis |> group_by(axis, stratum = as.character(window)) |>
    summarise(n = n(), se = sd(ed) / sqrt(n()), ed = mean(ed), .groups = "drop") |>
    mutate(by = "window"))
cat("\n=== (1) step trajectories: energy distance from the correct-delay forecast ===\n")
print(as.data.frame(sens), digits = 3)
readr::write_csv(sens, here("results", "turningpoint_proper_sensitivity.csv"))

step_pairs <- pair_up(step)
skill <- diff_summary(step_pairs, disease, axis, level_lab)
cat("\n=== (2) step trajectories: primary (log-scale CRPS, mean over horizons) ===\n")
print(as.data.frame(filter(skill, score == "crps_log_mean")), digits = 2)
skill_pooled <- diff_summary(filter(step_pairs, !is.na(level)), disease, axis)
cat("\n=== (2) step trajectories: all scores, pooled over the four non-degenerate levels ===\n")
print(as.data.frame(skill_pooled), digits = 2)
readr::write_csv(skill, here("results", "turningpoint_proper_skill.csv"))
readr::write_csv(skill_pooled, here("results", "turningpoint_proper_skill_pooled.csv"))

p <- ggplot2::ggplot(filter(sens, by == "window"), ggplot2::aes(stratum, ed, fill = stratum)) +
  ggplot2::geom_col(width = 0.7) +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = pmax(0, ed - se), ymax = ed + se), width = 0.25) +
  ggplot2::facet_wrap(~ axis, scales = "free_y") +
  ggplot2::scale_fill_manual(values = c(`no change` = "#1b9e77", `change ahead` = "#d95f02",
                                        `change behind` = "#7570b3"), guide = "none") +
  ggplot2::labs(x = NULL, y = "energy distance from the correct-delay forecast",
                title = "How much the delay choice moves the 6-week forecast",
                subtitle = "Step trajectories; no outcome used") +
  ggplot2::theme_bw()
ggplot2::ggsave(here("results", "fig_turningpoint_proper_sensitivity.png"), p,
                width = 8, height = 4.5, dpi = 150)

## disease-like replicates (generation-interval axis only)
resim <- all_rows |> filter(scen == "resim")
if (nrow(resim)) {
  ter <- function(x) cut(x, quantile(x, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                         include.lowest = TRUE, labels = c("low", "mid", "high"))
  rs <- resim |> filter(axis == "GT", !is.na(level)) |>
    group_by(disease) |> mutate(fwd = ter(dR_fc), back = ter(dR_est)) |> ungroup()
  rs_sens <- bind_rows(
    rs |> group_by(window = "forward", tercile = fwd) |>
      summarise(n = n(), se = sd(ed) / sqrt(n()), ed = mean(ed), .groups = "drop"),
    rs |> group_by(window = "backward", tercile = back) |>
      summarise(n = n(), se = sd(ed) / sqrt(n()), ed = mean(ed), .groups = "drop"))
  cat("\n=== (1) disease-like: energy distance by tercile of R change after/before origin ===\n")
  print(as.data.frame(rs_sens), digits = 3)
  readr::write_csv(rs_sens, here("results", "resim_proper_sensitivity.csv"))

  rs_pairs <- pair_up(resim)
  rskill <- diff_summary(rs_pairs, disease, level_lab)
  cat("\n=== (2) disease-like: primary (log-scale CRPS, mean over horizons) ===\n")
  print(as.data.frame(filter(rskill, score == "crps_log_mean")), digits = 2)
  rskill_pooled <- diff_summary(filter(rs_pairs, !is.na(level)), disease)
  cat("\n=== (2) disease-like: all scores, pooled over the four non-degenerate levels ===\n")
  print(as.data.frame(rskill_pooled), digits = 2)
  readr::write_csv(rskill, here("results", "resim_proper_skill.csv"))
  readr::write_csv(rskill_pooled, here("results", "resim_proper_skill_pooled.csv"))
}
