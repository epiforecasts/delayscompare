## Case-study illustration for the "delays matter only when transmission is changing"
## result, on REAL data where the true generation interval is unknown. Instead of a
## penalty against a known truth, we measure how much the forecast score SPREADS across
## the delay conditions (the 6x6 GT x INC grid) at each forecast origin: a wide spread
## means the delay choice matters there, a narrow spread means it is irrelevant. We plot
## that spread over time above the epidemic curve, so the spread can be read against the
## turning points of the outbreak.
##
## Forecast CRPS (log scale) vs observed cases, horizons 1-2 wk, per (origin, gt, inc).
## Output: fig_casestudy_overtime_<disease>.png and casestudy_overtime.csv.

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("scripts", "datacollect_casestudy.R"))

rt_opts  <- "latest"
diseases <- c("ebola", "covid", "cholera")
obs_of   <- casestudydata  # list(ebola=, covid=, cholera=) with date, confirm

score_disease <- function(disease) {
  agg <- here("results", "agg")
  s  <- readRDS(file.path(agg, paste0(disease, "_casestudy_", rt_opts, "_samples.rds")))
  s  <- s |> dplyr::filter(sample <= 1000)   # subsample for scoring speed
  id <- readRDS(file.path(agg, paste0(disease, "_casestudy_", rt_opts, "_id.rds")))
  obs <- obs_of[[disease]] |> dplyr::transmute(date = as.Date(date), true_value = confirm) |>
    dplyr::filter(!is.na(true_value))

  id_dedup <- id |> dplyr::distinct(result_list, .keep_all = TRUE) |>
    dplyr::select(result_list, forecast_origin)
  origins <- id_dedup |> dplyr::mutate(forecast_origin = as.Date(forecast_origin))

  scored <- s |> dplyr::filter(type == "forecast") |>
    dplyr::left_join(origins, by = "result_list") |>
    dplyr::left_join(obs, by = "date") |>
    dplyr::filter(!is.na(true_value)) |>
    dplyr::mutate(h = as.numeric(date - forecast_origin) / 7) |>
    dplyr::filter(h > 0, h <= 2)
  if (nrow(scored) == 0) return(NULL)

  fc <- scoringutils::as_forecast_sample(scored,
    forecast_unit = c("date", "result_list", "gt", "inc", "forecast_origin"),
    observed = "true_value", predicted = "prediction", sample_id = "sample") |>
    scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1, label = "log") |>
    scoringutils::score()
  per_cell <- fc |> dplyr::filter(scale == "log") |>
    dplyr::group_by(result_list, forecast_origin, gt, inc) |>
    dplyr::summarise(crps = mean(crps), .groups = "drop")

  ## spread across the delay grid per origin: median and range of CRPS
  per_origin <- per_cell |> dplyr::group_by(result_list, forecast_origin) |>
    dplyr::summarise(med = median(crps), lo = min(crps), hi = max(crps),
                     iqr = IQR(crps), .groups = "drop") |>
    dplyr::mutate(disease = disease, forecast_origin = as.Date(forecast_origin))
  list(per_origin = per_origin, obs = obs)
}

library(ggplot2)
## score every disease first, persist the CSV, then draw figures (so a plotting
## failure never loses the scores)
scored_all <- list()
for (d in diseases) {
  r <- tryCatch(score_disease(d), error = function(e) { message("skip ", d, ": ", conditionMessage(e)); NULL })
  if (!is.null(r)) scored_all[[d]] <- r
}
readr::write_csv(dplyr::bind_rows(lapply(scored_all, `[[`, "per_origin")),
                 here("results", "casestudy_overtime.csv"))
message("Saved: casestudy_overtime.csv")

for (d in names(scored_all)) {
  r <- scored_all[[d]]
  po <- r$per_origin
  obs <- r$obs |> dplyr::mutate(date = as.Date(date)) |>
    dplyr::filter(date >= min(po$forecast_origin) - 30, date <= max(po$forecast_origin) + 30)
  fig <- tryCatch({
    p_top <- ggplot(po, aes(forecast_origin, med)) +
      geom_ribbon(aes(ymin = lo, ymax = hi), fill = "#d95f02", alpha = 0.25) +
      geom_line(colour = "#d95f02") + geom_point(colour = "#d95f02", size = 2) +
      labs(y = "forecast log-CRPS\n(band = range over delay grid)", x = NULL,
           title = paste0(d, ": delay choice matters where the curve turns")) +
      theme_bw() + theme(axis.text.x = element_blank())
    p_bot <- ggplot(obs, aes(date, true_value)) +
      geom_col(fill = "grey55", width = 1) +
      geom_vline(data = po, aes(xintercept = as.numeric(forecast_origin)),
                 linetype = "dotted", colour = "#d95f02", alpha = 0.7) +
      labs(y = "reported cases", x = NULL) + theme_bw()
    patchwork::wrap_plots(p_top, p_bot, ncol = 1, heights = c(1, 1.1))
  }, error = function(e) { message("fig fail ", d, ": ", conditionMessage(e)); NULL })
  if (!is.null(fig)) {
    ggsave(here("results", paste0("fig_casestudy_overtime_", d, ".png")), fig,
           width = 8, height = 6, dpi = 150)
    message("Saved: fig_casestudy_overtime_", d, ".png")
  }
}
