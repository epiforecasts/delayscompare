## Compute mean log-CRPS for the three regime-2 conditions per disease:
##   (i)   no informed input  : casestudy gt=1, inc=1
##   (ii)  imposed literature : casestudy gt=4, inc=4 (correct cell)
##   (iii) estimated from data: weightprior=FALSE, both varied
## R^ filter applied as in fig_weightprior.R; mean CRPS taken across
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

score_block <- function(samples, id, diagnostics, true_cases){
  if (is.null(samples) || is.null(id)) return(NA_real_)
  if (!is.null(diagnostics)) {
    keep <- diagnostics |>
      dplyr::filter(!is.na(max_rhat), max_rhat <= 1.05)
    samples <- samples |> dplyr::semi_join(keep, by = "result_list")
  }
  if (nrow(samples) == 0) return(NA_real_)
  id_dedup <- id |> dplyr::distinct(result_list, .keep_all = TRUE)
  scored <- samples |>
    dplyr::filter(type == "forecast") |>
    dplyr::left_join(id_dedup, by = "result_list") |>
    dplyr::left_join(true_cases, by = "date") |>
    dplyr::filter(!is.na(true_value))
  if (nrow(scored) == 0) return(NA_real_)
  fc <- tryCatch(scoringutils::as_forecast_sample(
    data = scored,
    forecast_unit = c("date", "type", "result_list", "model", "timepoint"),
    observed = "true_value", predicted = "prediction", sample_id = "sample"),
    error = function(e) NULL)
  if (is.null(fc)) return(NA_real_)
  sc <- fc |>
    scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1, label = "log") |>
    dplyr::filter(scale == "log") |>
    scoringutils::score()
  res <- sc |>
    dplyr::group_by(timepoint) |>
    dplyr::filter(date == max(date)) |>
    dplyr::ungroup() |>
    dplyr::summarise(crps = mean(crps, na.rm = TRUE)) |>
    dplyr::pull(crps)
  if (length(res) == 0) NA_real_ else res
}

results <- list()

for (disease in diseases){
  cd <- casestudydata[[disease]]
  true_cases <- cd |> dplyr::rename(true_value = !!rlang::sym(case_col(cd))) |>
    dplyr::select(date, true_value)

  ## casestudy aggregated files: cells indexed by gt (1..6) and inc (1..6).
  ## "no informed input" cell is gt=1 (one-day) and inc=1 (no delay).
  ## "imposed literature" cell is gt=4 (correct mean) and inc=4 (correct mean).
  cs_id   <- read_latest(here("results/casestudy"),
                         paste0("res_", disease, "_casestudy_", rt_opts, "_all_id"))
  cs_samp <- read_latest(here("results/casestudy"),
                         paste0("res_", disease, "_casestudy_", rt_opts, "_all_samples"))
  cs_diag <- tryCatch(read_latest(here("results/casestudy"),
                                  paste0("res_", disease, "_casestudy_", rt_opts, "_all_diagnostics")),
                      error = function(e) NULL)

  for (cell in list(c("no_input", 1, 1), c("imposed", 4, 4))){
    label <- cell[[1]]; gt <- as.integer(cell[[2]]); inc <- as.integer(cell[[3]])
    id_sub   <- cs_id   |> dplyr::filter(gt == !!gt, inc == !!inc)
    samp_sub <- cs_samp |> dplyr::filter(gt == !!gt, inc == !!inc)
    diag_sub <- if (!is.null(cs_diag)) cs_diag |> dplyr::filter(gt == !!gt, inc == !!inc) else NULL
    crps <- score_block(samp_sub, id_sub, diag_sub, true_cases)
    results[[paste(disease, label)]] <- tibble::tibble(disease = disease, condition = label, crps = crps)
  }

  ## estimated-from-data: weightprior=FALSE, vary=both
  prefix <- paste0("res_", disease, "_weightprior_FALSE_both_", rt_opts, "_all_")
  est_id   <- tryCatch(read_latest(here("results/weightprior"), paste0(prefix, "id")),
                       error = function(e) NULL)
  est_samp <- tryCatch(read_latest(here("results/weightprior"), paste0(prefix, "samples")),
                       error = function(e) NULL)
  est_diag <- tryCatch(read_latest(here("results/weightprior"), paste0(prefix, "diagnostics")),
                       error = function(e) NULL)
  crps <- score_block(est_samp, est_id, est_diag, true_cases)
  results[[paste(disease, "estimated")]] <- tibble::tibble(disease = disease, condition = "estimated", crps = crps)
}

out <- dplyr::bind_rows(results)
print(out, n = 50)
