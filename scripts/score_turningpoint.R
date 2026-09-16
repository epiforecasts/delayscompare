## Score the controlled turning-point experiment on BOTH outcomes:
##   (1) forecast skill  - log-CRPS of forecast cases at 6 wk vs the true series
##   (2) R estimation    - CRPS of the estimated nowcast R at the origin vs true R
## for BOTH misspecification axes (GT: gt=1..6, inc=4; delay: gt=4, inc=1..6), on the
## flat / bend / turn trajectories. For each forecast origin we know the TRUE R exactly
## (step profile, change at day 105), so we compute the change in R over the relevant
## window (forecast: [origin, origin+42]; estimation: [origin-84, origin]) and regress
## the per-origin error slope on it. Hypothesis: both slopes ~0 on flat, and rise with
## |dR| on bend and turn alike (i.e. delays bite where R CHANGES, not only where R=1).

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))

rt_opts  <- "latest"
diseases <- c("ebola", "covid", "cholera")
scens    <- c("flat", "bend", "turn")
raw_dir  <- here("results", "raw")
gt_mult  <- c(`1` = NA, `2` = 0.25, `3` = 0.8, `4` = 1, `5` = 1.25, `6` = 2)
CHANGE   <- 105
FC_H     <- 6L   # forecast horizon (weeks) at which to read the forecast slope

## true R step profile: value before/after the change day (measured from trajectory start)
ab_of <- list(flat = c(1.10, 1.10), bend = c(1.30, 1.00), turn = c(1.30, 0.70))
true_R_at <- function(scen, day) ifelse(day < CHANGE, ab_of[[scen]][1], ab_of[[scen]][2])

start_of <- function(disease) startenddates[[disease]][["startdate"]]

## one raw fit -> forecast log-CRPS (6 wk) and R-nowcast CRPS, plus the two dR values
score_fit <- function(disease, scen, rep_id, gt, inc, tp) {
  stub <- paste0(disease, "_hz_", scen, "_rep", rep_id, "_", rt_opts)
  suf  <- paste0("_gt", gt, "_inc", inc, "_tp", tp, ".rds")
  sf <- file.path(raw_dir, paste0(stub, "_samples", suf))
  Rf <- file.path(raw_dir, paste0(stub, "_R", suf))
  idf <- file.path(raw_dir, paste0(stub, "_id", suf))
  df  <- file.path(raw_dir, paste0(stub, "_diagnostics", suf))
  if (!all(file.exists(sf, Rf, idf))) return(NULL)
  if (file.exists(df)) {
    d <- readRDS(df)
    if (!(!is.na(d$max_rhat) && d$max_rhat <= 1.05 &&
          (is.na(d$num_divergent) || d$num_divergent <= 100))) return(NULL)
  }
  id <- readRDS(idf); origin <- as.Date(id$forecast_origin[1])
  start <- start_of(disease)
  day0  <- as.numeric(origin - start)
  dR_fc  <- abs(true_R_at(scen, day0 + 42) - true_R_at(scen, day0))
  dR_est <- abs(true_R_at(scen, day0)      - true_R_at(scen, day0 - 84))

  ## forecast case CRPS at FC_H weeks
  true_cases <- readRDS(here("data", paste0(disease, "_sim_data_", scen, "_rep", rep_id, ".rds"))) |>
    dplyr::filter(variable == "reported_cases") |>
    dplyr::rename(true_value = value) |> dplyr::select(date, true_value)
  fc <- readRDS(sf) |> dplyr::filter(type == "forecast") |>
    dplyr::left_join(true_cases, by = "date") |> dplyr::filter(!is.na(true_value)) |>
    dplyr::mutate(h = as.numeric(date - origin) / 7) |> dplyr::filter(h == FC_H)
  fc_crps <- NA_real_
  if (nrow(fc) > 0) {
    f <- tryCatch(scoringutils::as_forecast_sample(fc,
      forecast_unit = c("date"), observed = "true_value",
      predicted = "prediction", sample_id = "sample") |>
      scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1, label = "log") |>
      scoringutils::score(), error = function(e) NULL)
    if (!is.null(f)) fc_crps <- f$crps[f$scale == "log"][1]
  }

  ## R nowcast CRPS at the origin (last estimation date) vs true R there
  Rest <- readRDS(Rf) |> dplyr::filter(type != "forecast", date == origin)
  R_crps <- NA_real_
  if (nrow(Rest) > 0) {
    trueR <- true_R_at(scen, day0)
    Rest$obs <- trueR
    rf <- tryCatch(scoringutils::as_forecast_sample(Rest,
      forecast_unit = c("date"), observed = "obs",
      predicted = "value", sample_id = "sample") |> scoringutils::score(),
      error = function(e) NULL)
    if (!is.null(rf)) R_crps <- rf$crps[1]
  }

  data.frame(disease = disease, scen = scen, rep = rep_id, gt = gt, inc = inc,
             origin = origin, dR_fc = dR_fc, dR_est = dR_est,
             fc_crps = fc_crps, R_crps = R_crps)
}

## discover all fits
rows <- list()
for (disease in diseases) for (scen in scens) {
  files <- list.files(raw_dir, pattern = paste0("^", disease, "_hz_", scen,
    "_rep[0-9]+_", rt_opts, "_samples_gt[0-9]+_inc[0-9]+_tp[0-9]+\\.rds$"))
  if (!length(files)) next
  k <- stringr::str_match(files,
    paste0("_hz_", scen, "_rep([0-9]+)_.*_samples_gt([0-9]+)_inc([0-9]+)_tp([0-9]+)\\.rds$"))
  for (i in seq_len(nrow(k))) {
    r <- score_fit(disease, scen, as.integer(k[i,2]), as.integer(k[i,3]),
                   as.integer(k[i,4]), as.integer(k[i,5]))
    if (!is.null(r)) rows[[length(rows)+1]] <- r
  }
  message("  ", disease, " ", scen, ": ", length(files), " fits")
}
dat <- dplyr::bind_rows(rows)
if (nrow(dat) == 0) stop("No turning-point fits scored yet.")
readr::write_csv(dat, here("results", "turningpoint_perorigin.csv"))

## mean error per (disease, scen, origin, gt, inc), then slope over each axis
mean_cell <- function(df, ycol) df |> dplyr::group_by(disease, scen, origin, gt, inc) |>
  dplyr::summarise(y = mean(.data[[ycol]], na.rm = TRUE),
                   dR_fc = dplyr::first(dR_fc), dR_est = dplyr::first(dR_est), .groups = "drop")

## GT axis: gt=1..6 at inc=4 (drop degenerate gt=1); delay axis: inc=1..6 at gt=4
axis_slopes <- function(df, ycol, dRcol) {
  m <- mean_cell(df, ycol)
  gt_s <- m |> dplyr::filter(inc == 4) |> dplyr::mutate(lev = gt_mult[as.character(gt)]) |>
    dplyr::filter(!is.na(lev)) |> dplyr::group_by(disease, scen, origin) |>
    dplyr::summarise(slope = if (dplyr::n() >= 3) unname(coef(lm(y ~ log(lev)))[2]) else NA,
                     dR = dplyr::first(.data[[dRcol]]), axis = "GT", .groups = "drop")
  del_s <- m |> dplyr::filter(gt == 4) |> dplyr::mutate(lev = gt_mult[as.character(inc)]) |>
    dplyr::filter(!is.na(lev)) |> dplyr::group_by(disease, scen, origin) |>
    dplyr::summarise(slope = if (dplyr::n() >= 3) unname(coef(lm(y ~ log(lev)))[2]) else NA,
                     dR = dplyr::first(.data[[dRcol]]), axis = "delay", .groups = "drop")
  dplyr::bind_rows(gt_s, del_s)
}

fc_slopes <- axis_slopes(dat, "fc_crps",  "dR_fc")  |> dplyr::mutate(outcome = "forecast (6wk)")
R_slopes  <- axis_slopes(dat, "R_crps",   "dR_est") |> dplyr::mutate(outcome = "R nowcast")
sl <- dplyr::bind_rows(fc_slopes, R_slopes)
sl$scen <- factor(sl$scen, levels = scens)
readr::write_csv(sl, here("results", "turningpoint_slopes.csv"))

cat("\n=== mean |error slope| by scenario x axis x outcome (flat should be ~0) ===\n")
print(aggregate(abs(slope) ~ scen + axis + outcome, sl, function(x) round(mean(x, na.rm = TRUE), 4)))

## Figure: error slope vs |dR over window|, for each outcome x axis, coloured by scenario
p <- ggplot2::ggplot(sl[!is.na(sl$slope), ], ggplot2::aes(dR, slope, colour = scen)) +
  ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "grey60") +
  ggplot2::geom_point(size = 2, alpha = 0.8) +
  ggplot2::geom_smooth(ggplot2::aes(group = 1), method = "lm", se = TRUE,
                       colour = "black", linewidth = 0.4) +
  ggplot2::facet_grid(outcome ~ axis, scales = "free_y") +
  ggplot2::scale_colour_manual(values = c(flat = "#1b9e77", bend = "#d95f02", turn = "#7570b3")) +
  ggplot2::labs(x = "change in true R over the relevant window (|dR|)",
                y = "per-origin error slope (delay misspecification)", colour = "trajectory",
                title = "Delays bite only where R changes - both R estimation and forecasts, both axes") +
  ggplot2::theme_bw()
ggplot2::ggsave(here("results", "fig_turningpoint.png"), p, width = 9, height = 6.5, dpi = 150)
message("Saved: results/fig_turningpoint.png")
