## Compute mean log-CRPS for the three regime-2 conditions per disease:
##   (i)   no informed input  : casestudy gt=1, inc=1
##   (ii)  imposed literature : casestudy gt=4, inc=4 (correct cell)
##   (iii) estimated from data: weightprior=FALSE, both varied
## R-hat filter applied to drop non-converged fits; mean CRPS taken across
## per-timepoint forecast horizons.

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("scripts", "datacollect_casestudy.R"))

rt_opts <- "latest"
diseases <- c("ebola", "covid", "cholera")

case_col <- function(d){
  if ("confirm" %in% names(d)) "confirm" else "cases"
}

score_block <- function(samples, id, diagnostics, true_cases, filter_rhat = TRUE, case_data = NULL){
  if (is.null(samples) || is.null(id)) return(NULL)
  if (filter_rhat && !is.null(diagnostics)) {
    keep <- diagnostics |>
      dplyr::filter(!is.na(max_rhat), max_rhat <= 1.05,
                    is.na(num_divergent) | num_divergent <= 100)
    samples <- samples |> dplyr::semi_join(keep, by = "result_list")
  }
  if (nrow(samples) == 0) return(NULL)
  id_dedup <- id |> dplyr::distinct(result_list, .keep_all = TRUE)
  if (!"forecast_origin" %in% names(id_dedup) && !is.null(case_data)) {
    start_date <- min(case_data$date)
    end_date <- max(case_data$date)
    target_dates <- seq.Date(from=start_date, to=end_date, by="4 weeks")
    scen_tps <- sapply(target_dates, function(target){
      avail <- case_data$date[case_data$date >= target]
      if(length(avail) > 0) min(avail) else NA
    }) |> as.Date(origin = "1970-01-01")
    scen_tps <- scen_tps[!is.na(scen_tps)][-1]
    if(length(scen_tps) > 8) scen_tps <- scen_tps[1:8]
    tp_map <- tibble::tibble(timepoint = seq_along(scen_tps), forecast_origin = scen_tps)
    id_dedup <- id_dedup |> dplyr::left_join(tp_map, by = "timepoint")
  }
  scored <- samples |>
    dplyr::filter(type == "forecast") |>
    dplyr::left_join(id_dedup, by = "result_list") |>
    dplyr::left_join(true_cases, by = "date") |>
    dplyr::filter(!is.na(true_value)) |>
    dplyr::mutate(horizon = as.numeric(date - forecast_origin) / 7) |>
    dplyr::filter(horizon %in% c(1, 2))
  if (nrow(scored) == 0) return(NULL)
  fc <- tryCatch(scoringutils::as_forecast_sample(
    data = scored,
    forecast_unit = c("date", "type", "result_list", "model", "timepoint", "horizon"),
    observed = "true_value", predicted = "prediction", sample_id = "sample"),
    error = function(e) NULL)
  if (is.null(fc)) return(NULL)
  sc <- fc |>
    scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1, label = "log") |>
    scoringutils::score()
  res <- sc |>
    dplyr::group_by(scale, horizon) |>
    dplyr::summarise(crps = mean(crps, na.rm = TRUE), .groups = "drop")
  if (nrow(res) == 0) NULL else res
}

results <- list()

for (disease in diseases){
  cd <- casestudydata[[disease]]
  true_cases <- cd |> dplyr::rename(true_value = !!rlang::sym(case_col(cd))) |>
    dplyr::select(date, true_value)

  ## casestudy aggregated files: cells indexed by gt (1..6) and inc (1..6).
  ## "no informed input" cell is gt=1 (one-day) and inc=1 (no delay).
  ## "imposed literature" cell is gt=4 (correct mean) and inc=4 (correct mean).
  cs_id   <- read_latest(here("results", "agg"),
                         paste0(disease, "_casestudy_", rt_opts, "_id"))
  cs_samp <- read_latest(here("results", "agg"),
                         paste0(disease, "_casestudy_", rt_opts, "_samples"))
  cs_diag <- tryCatch(read_latest(here("results", "agg"),
                                  paste0(disease, "_casestudy_", rt_opts, "_diagnostics")),
                      error = function(e) NULL)

  for (cell in list(c("no_anything", 1, 1),
                    c("no_GT",       1, 4),
                    c("no_delays",   4, 1),
                    c("imposed",     4, 4))){
    label <- cell[[1]]; gt <- as.integer(cell[[2]]); inc <- as.integer(cell[[3]])
    id_sub   <- cs_id   |> dplyr::filter(gt == !!gt, inc == !!inc)
    samp_sub <- cs_samp |> dplyr::filter(gt == !!gt, inc == !!inc)
    diag_sub <- if (!is.null(cs_diag)) cs_diag |> dplyr::filter(gt == !!gt, inc == !!inc) else NULL
    ## impose/omit conditions: discard non-converged fits (R-hat filter on)
    sc <- score_block(samp_sub, id_sub, diag_sub, true_cases, filter_rhat = TRUE)
    if (!is.null(sc)) {
      results[[paste(disease, label)]] <- tibble::tibble(disease = disease, condition = label) |>
        dplyr::cross_join(sc)
    }
  }

  ## estimated GT (delays held at literature) — vary="gt", weight_prior=FALSE
  ## estimated delays (GT held at literature) — vary="inc", weight_prior=FALSE
  for (cond in list(c("est_GT", "gt"), c("est_delays", "inc"), c("est_both", "both"))){
    label <- cond[[1]]; v <- cond[[2]]
    prefix <- paste0("res_", disease, "_widepriors_FALSE_", v, "_", rt_opts, "_all_")
    est_id   <- tryCatch(read_latest(here("results", "widepriors"), paste0(prefix, "id")),
                         error = function(e) NULL)
    est_samp <- tryCatch(read_latest(here("results", "widepriors"), paste0(prefix, "samples")),
                         error = function(e) NULL)
    est_diag <- tryCatch(read_latest(here("results", "widepriors"), paste0(prefix, "diagnostics")),
                         error = function(e) NULL)
    ## learn conditions: report non-convergence, do not discard (R-hat filter off)
    sc <- score_block(est_samp, est_id, est_diag, true_cases, filter_rhat = FALSE, case_data = cd)
    if (!is.null(sc)) {
      results[[paste(disease, label)]] <- tibble::tibble(disease = disease, condition = label) |>
        dplyr::cross_join(sc)
    }
  }
}

out <- dplyr::bind_rows(results)
readr::write_csv(out, here("results", "q2_casestudy_scores.csv"))
message("Saved Q2 scores to results/q2_casestudy_scores.csv")
print(out, n = 50)
