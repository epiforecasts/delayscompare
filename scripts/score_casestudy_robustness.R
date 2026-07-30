## Supplement robustness (analysis plan decision #7): the two excluded case-study
## cells (covid gt1/inc5/tp5, gt1/inc6/tp5) lose one forecast origin, so those cells
## average over a different origin set than the rest. Here we recompute the case-study
## per-cell log-CRPS on the COMMON set of origins that converged across ALL cells
## (drop any origin missing from any cell) and confirm the gt x inc ranking is
## unchanged relative to the per-cell-available-origins version.

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("scripts", "datacollect_casestudy.R"))

rt_opts   <- "latest"
diseases  <- c("ebola", "covid", "cholera")
N_SAMPLES <- 2000L
case_col  <- function(d) if ("confirm" %in% names(d)) "confirm" else "cases"

## per (gt, inc, origin, horizon) log-scale CRPS for one disease's whole grid
score_grid <- function(samples, id, diagnostics, true_cases) {
  keep <- diagnostics |>
    dplyr::filter(!is.na(max_rhat), max_rhat <= 1.05,
                  is.na(num_divergent) | num_divergent <= 100)
  samples <- samples |>
    dplyr::semi_join(keep, by = c("gt", "inc", "result_list")) |>
    dplyr::filter(sample <= N_SAMPLES)
  if (nrow(samples) == 0) return(NULL)
  id_dedup <- id |> dplyr::distinct(gt, inc, result_list, .keep_all = TRUE) |>
    dplyr::select(gt, inc, result_list, forecast_origin)
  scored <- samples |>
    dplyr::filter(type == "forecast") |>
    dplyr::left_join(id_dedup, by = c("gt", "inc", "result_list")) |>
    dplyr::left_join(true_cases, by = "date") |>
    dplyr::filter(!is.na(true_value)) |>
    dplyr::mutate(horizon = as.numeric(date - forecast_origin) / 7) |>
    dplyr::filter(horizon %in% c(1, 2))
  if (nrow(scored) == 0) return(NULL)
  fc <- scoringutils::as_forecast_sample(scored,
    forecast_unit = c("date", "type", "gt", "inc", "result_list", "model", "horizon"),
    observed = "true_value", predicted = "prediction", sample_id = "sample")
  sc <- fc |>
    scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1, label = "log") |>
    scoringutils::score()
  sc |> dplyr::filter(scale == "log") |>
    dplyr::transmute(gt, inc, origin = result_list, log_crps = crps)
}

summ <- list()
for (disease in diseases) {
  id   <- read_latest(here("results", "agg"), paste0(disease, "_casestudy_", rt_opts, "_id"))
  samp <- read_latest(here("results", "agg"), paste0(disease, "_casestudy_", rt_opts, "_samples"))
  diag <- read_latest(here("results", "agg"), paste0(disease, "_casestudy_", rt_opts, "_diagnostics"))
  cd <- casestudydata[[disease]]
  true_cases <- cd |> dplyr::rename(true_value = !!rlang::sym(case_col(cd))) |>
    dplyr::select(date, true_value)

  sc <- score_grid(samp, id, diag, true_cases)
  if (is.null(sc)) next
  sc <- sc |> dplyr::group_by(gt, inc, origin) |>
    dplyr::summarise(log_crps = mean(log_crps), .groups = "drop")  # avg over horizons

  ## common origins = present for every (gt,inc) cell
  n_cells <- dplyr::n_distinct(paste(sc$gt, sc$inc))
  common <- sc |> dplyr::count(origin) |> dplyr::filter(n == n_cells) |> dplyr::pull(origin)

  avail_rank <- sc |> dplyr::group_by(gt, inc) |>
    dplyr::summarise(crps = mean(log_crps), .groups = "drop")
  common_rank <- sc |> dplyr::filter(origin %in% common) |>
    dplyr::group_by(gt, inc) |>
    dplyr::summarise(crps = mean(log_crps), .groups = "drop")
  cmp <- dplyr::inner_join(avail_rank, common_rank, by = c("gt", "inc"),
                           suffix = c("_avail", "_common"))
  rho <- suppressWarnings(cor(cmp$crps_avail, cmp$crps_common, method = "spearman"))
  n_dropped_origins <- dplyr::n_distinct(sc$origin) - length(common)
  summ[[disease]] <- data.frame(disease = disease, n_cells = n_cells,
                                origins_total = dplyr::n_distinct(sc$origin),
                                origins_common = length(common),
                                origins_dropped = n_dropped_origins,
                                spearman_rho = round(rho, 4))
  summ[[paste0(disease, "_cmp")]] <- cmp |> dplyr::mutate(disease = disease)
}

tab <- dplyr::bind_rows(summ[diseases])
cat("\n=== Case-study ranking robustness: available vs common-origin per-cell log-CRPS ===\n")
print(tab, row.names = FALSE)
readr::write_csv(tab, here("results", "casestudy_robustness_summary.csv"))

## scatter: available vs common-origin per-cell CRPS, per disease (identity = unchanged)
cmp_all <- dplyr::bind_rows(summ[paste0(diseases, "_cmp")])
p <- ggplot2::ggplot(cmp_all, ggplot2::aes(crps_avail, crps_common)) +
  ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
  ggplot2::geom_point(alpha = 0.7) +
  ggplot2::facet_wrap(~ disease, scales = "free") +
  ggplot2::labs(x = "per-cell log-CRPS (available origins)",
                y = "per-cell log-CRPS (common origins)",
                title = "Supplement: case-study ranking robust to common-origin restriction") +
  ggplot2::theme_bw()
ggplot2::ggsave(here("results", "fig_casestudy_robustness.png"), p, width = 10, height = 4, dpi = 150)
message("Saved: results/fig_casestudy_robustness.png")
