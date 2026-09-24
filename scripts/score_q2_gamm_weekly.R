## Q2 joint GAMM, updated: the `impose` and `learn_gt` conditions now use the dense
## weekly-origin runs (25-30 origins for ebola/cholera, 7-8 for covid) instead of the
## original 6-8 monthly origins; `no_input`, `learn_delay` and `learn_both` still use
## the monthly runs (never re-run weekly, `learn_delay`/`learn_both` are not
## identifiable for covid regardless of density, §1). All five conditions are scored
## with the same `score_origins()` pipeline used throughout Q2 (2000-sample
## equalisation, 1-2wk horizon, scoringutils log-CRPS), so switching a condition's
## data source does not change how it is scored, only how many origins back it.
##
## Origins are keyed by actual calendar date, not raw timepoint index: a monthly
## timepoint 3 and a weekly timepoint 3 for the same disease are different dates, so
## a shared s(time) smooth needs real dates, not the index that numbers them.

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("scripts", "datacollect_casestudy.R"))

rt_opts   <- "latest"
diseases  <- c("ebola", "covid", "cholera")
N_SAMPLES <- 2000L

case_col <- function(d) if ("confirm" %in% names(d)) "confirm" else "cases"

## unchanged from score_q2_gamm.R: per-origin log-scale CRPS for one condition,
## given combined (all-origins) samples/id/diagnostics data frames
score_origins <- function(samples, id, diagnostics, true_cases,
                          filter_rhat, case_data = NULL) {
  if (is.null(samples) || is.null(id) || nrow(samples) == 0) return(NULL)
  if (filter_rhat && !is.null(diagnostics)) {
    keep <- diagnostics |>
      dplyr::filter(!is.na(max_rhat), max_rhat <= 1.05,
                    is.na(num_divergent) | num_divergent <= 100)
    samples <- samples |> dplyr::semi_join(keep, by = "result_list")
  }
  if (nrow(samples) == 0) return(NULL)
  samples <- samples |> dplyr::filter(sample <= N_SAMPLES)

  id_dedup <- id |> dplyr::distinct(result_list, .keep_all = TRUE)
  if (!"forecast_origin" %in% names(id_dedup) && !is.null(case_data)) {
    start_date <- min(case_data$date); end_date <- max(case_data$date)
    target_dates <- seq.Date(start_date, end_date, by = "4 weeks")
    scen_tps <- sapply(target_dates, function(t) {
      av <- case_data$date[case_data$date >= t]; if (length(av)) min(av) else NA
    }) |> as.Date(origin = "1970-01-01")
    scen_tps <- scen_tps[!is.na(scen_tps)][-1]
    if (length(scen_tps) > 8) scen_tps <- scen_tps[1:8]
    id_dedup <- id_dedup |>
      dplyr::left_join(tibble::tibble(timepoint = seq_along(scen_tps),
                                      forecast_origin = scen_tps), by = "timepoint")
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
    forecast_unit = c("date", "type", "result_list", "model", "timepoint",
                      "horizon", "forecast_origin"),
    observed = "true_value", predicted = "prediction", sample_id = "sample"),
    error = function(e) NULL)
  if (is.null(fc)) return(NULL)

  sc <- fc |>
    scoringutils::transform_forecasts(fun = scoringutils::log_shift,
                                      offset = 1, label = "log") |>
    scoringutils::score()
  sc |>
    dplyr::filter(scale == "log") |>
    dplyr::transmute(timepoint, horizon, forecast_origin, log_crps = crps)
}

## bind the per-origin weekly raw files for one disease into the combined
## samples/id/diagnostics shape score_origins() expects
bind_weekly <- function(files_samples, files_id, files_diag) {
  rd <- function(fs) if (length(fs)) dplyr::bind_rows(lapply(fs, readRDS)) else NULL
  list(samples = rd(files_samples), id = rd(files_id), diagnostics = rd(files_diag))
}

## Weekly-preferred, monthly-fallback scorer for one learn-arm condition
## (vary = "gt"/"inc"/"both"). Uses the dense per-origin weekly runner's output
## when present (05e/06f, `results/res_<disease>_widepriors_FALSE_<vary>_..._tp*-*`),
## and falls back to the original monthly aggregate (`results/widepriors/`) when it
## is not, so the joint model densifies condition-by-condition as weekly arrays are
## launched, without code changes elsewhere.
score_learn_condition <- function(disease, vary, cd, true_cases, filter_rhat) {
  gf <- list.files(here("results"), pattern = paste0("^res_", disease,
    "_widepriors_FALSE_", vary, "_", rt_opts, "_samples_tp[0-9]+-[0-9]+\\.rds$"),
    full.names = TRUE)
  if (length(gf) > 0) {
    gid <- sub("_samples_", "_id_", gf); gdg <- sub("_samples_", "_diagnostics_", gf)
    b <- bind_weekly(gf, gid, gdg)
    ## weekly learn-arm id files have no forecast_origin column; derive it from
    ## the actual weekly cadence, not score_origins()'s monthly default.
    if (!is.null(b$id) && !"forecast_origin" %in% names(b$id)) {
      target_dates <- seq.Date(min(cd$date), max(cd$date), by = "1 weeks")
      scen_tps <- sapply(target_dates, function(t) {
        av <- cd$date[cd$date >= t]; if (length(av)) min(av) else NA
      }) |> as.Date(origin = "1970-01-01")
      scen_tps <- scen_tps[!is.na(scen_tps)][-1]
      b$id <- b$id |> dplyr::left_join(
        tibble::tibble(timepoint = seq_along(scen_tps), forecast_origin = scen_tps),
        by = "timepoint")
    }
    return(score_origins(b$samples, b$id, b$diagnostics, true_cases, filter_rhat = filter_rhat))
  }
  ## STOPGAP for vary="gt": raw per-origin weekly fits not available locally
  ## (cluster unreachable). Fall back to the pre-scored weekly CSV from
  ## score_casestudy_learn_vs_impose_weekly.R (same 1-2wk horizon and scoringutils
  ## log-CRPS pipeline, but a separately written scoring path, not score_origins()
  ## used everywhere else here). Flagged explicitly in the output; re-run against
  ## raw fits once the cluster is reachable and this branch stops firing on its own
  ## (gf above will then be non-empty).
  if (vary == "gt") {
    wkcsv <- here("results", "casestudy_learn_vs_impose_weekly.csv")
    if (file.exists(wkcsv)) {
      wk <- readr::read_csv(wkcsv, show_col_types = FALSE) |>
        dplyr::filter(disease == !!disease, is.finite(crps_learn))
      if (nrow(wk) > 0) {
        ## the CSV holds one CRPS averaged over the 1-2wk window, not per-horizon;
        ## duplicate across both horizon levels so the shared `+ horizon` term
        ## does not silently na.omit these rows (approximation, not real per-
        ## horizon granularity - superseded once scored from raw weekly fits)
        base <- wk |> dplyr::transmute(timepoint = tp,
                                       forecast_origin = as.Date(origin), log_crps = crps_learn,
                                       stopgap_source = "weekly_csv_fallback")
        return(dplyr::bind_rows(dplyr::mutate(base, horizon = 1),
                                dplyr::mutate(base, horizon = 2)))
      }
    }
  }
  ## fallback: monthly aggregate
  pfx <- paste0("res_", disease, "_widepriors_FALSE_", vary, "_", rt_opts, "_all_")
  ei <- tryCatch(read_latest(here("results", "widepriors"), paste0(pfx, "id")),          error = function(e) NULL)
  es <- tryCatch(read_latest(here("results", "widepriors"), paste0(pfx, "samples")),     error = function(e) NULL)
  ed <- tryCatch(read_latest(here("results", "widepriors"), paste0(pfx, "diagnostics")), error = function(e) NULL)
  score_origins(es, ei, ed, true_cases, filter_rhat = filter_rhat, case_data = cd)
}

rows <- list()
for (disease in diseases) {
  cd <- casestudydata[[disease]]
  true_cases <- cd |>
    dplyr::rename(true_value = !!rlang::sym(case_col(cd))) |>
    dplyr::select(date, true_value)

  ## --- impose (weekly, literature gt=4/inc=4) ---
  raw <- here("results", "raw")
  wf  <- list.files(raw, pattern = paste0("^", disease, "_casestudy_", rt_opts,
    "_samples_gt4_inc4_tp[0-9]+\\.rds$"), full.names = TRUE)
  wid <- sub("_samples_", "_id_", wf); wdg <- sub("_samples_", "_diagnostics_", wf)
  b <- bind_weekly(wf, wid, wdg)
  sc <- score_origins(b$samples, b$id, b$diagnostics, true_cases, filter_rhat = TRUE)
  if (!is.null(sc)) rows[[paste(disease, "impose")]] <- sc |>
    dplyr::mutate(disease = disease, condition = "impose")

  ## --- no_input (weekly if launched, else monthly, gt=1/inc=1) ---
  nf <- list.files(raw, pattern = paste0("^", disease, "_casestudy_", rt_opts,
    "_samples_gt1_inc1_tp[0-9]+\\.rds$"), full.names = TRUE)
  if (length(nf) > 0) {
    nid <- sub("_samples_", "_id_", nf); ndg <- sub("_samples_", "_diagnostics_", nf)
    b <- bind_weekly(nf, nid, ndg)
    sc <- score_origins(b$samples, b$id, b$diagnostics, true_cases, filter_rhat = TRUE)
  } else {
    cs_id   <- read_latest(here("results", "agg"), paste0(disease, "_casestudy_", rt_opts, "_id"))
    cs_samp <- read_latest(here("results", "agg"), paste0(disease, "_casestudy_", rt_opts, "_samples"))
    cs_diag <- tryCatch(read_latest(here("results", "agg"),
                                    paste0(disease, "_casestudy_", rt_opts, "_diagnostics")),
                        error = function(e) NULL)
    sc <- score_origins(cs_samp |> dplyr::filter(gt == 1, inc == 1),
                        cs_id   |> dplyr::filter(gt == 1, inc == 1),
                        if (!is.null(cs_diag)) cs_diag |> dplyr::filter(gt == 1, inc == 1) else NULL,
                        true_cases, filter_rhat = TRUE)
  }
  if (!is.null(sc)) rows[[paste(disease, "no_input")]] <- sc |>
    dplyr::mutate(disease = disease, condition = "no_input")

  ## --- learn_gt, learn_delay, learn_both (weekly if launched, else monthly) ---
  for (cond in list(c("learn_gt", "gt"), c("learn_delay", "inc"), c("learn_both", "both"))) {
    lab <- cond[[1]]; v <- cond[[2]]
    filter_rhat <- lab == "learn_gt"   # learn_delay/both: report non-convergence, don't filter (§1)
    sc <- score_learn_condition(disease, v, cd, true_cases, filter_rhat)
    if (!is.null(sc)) rows[[paste(disease, lab)]] <- sc |>
      dplyr::mutate(disease = disease, condition = lab)
  }
}

dat <- dplyr::bind_rows(rows)
dat$condition <- factor(dat$condition,
                        levels = c("no_input", "impose", "learn_gt", "learn_delay", "learn_both"))
dat$disease   <- factor(dat$disease, levels = diseases)
dat$horizon   <- factor(dat$horizon)
readr::write_csv(dat, here("results", "q2_perorigin_logcrps_weekly.csv"))
message("per-origin scores: ", nrow(dat), " rows; conditions x disease present:")
print(as.data.frame(table(dat$disease, dat$condition)))

## GAMM: log-scale CRPS ~ condition*disease + s(date, by=disease) + horizon.
## Origins are keyed by actual calendar date (numeric), not raw timepoint index, so
## a shared smooth per disease is meaningful across mixed monthly/weekly density.
library(mgcv)
dat$date_num <- as.numeric(dat$forecast_origin)
n_origin <- length(unique(dat$date_num))
k_time <- max(3L, min(10L, n_origin - 1L))
form <- log_crps ~ condition * disease + s(date_num, by = disease, k = k_time) + horizon
m <- bam(form, data = dat, method = "fREML")
saveRDS(m, here("results", "q2_gamm_weekly.rds"))
cat("\n=== GAMM parametric coefficients ===\n")
print(summary(m)$p.table)

## Condition marginal effects within disease, with 95% CIs
marg <- tryCatch({
  emm <- emmeans::emmeans(m, ~ condition | disease)
  as.data.frame(summary(emm, infer = c(TRUE, FALSE)))
}, error = function(e) {
  message("emmeans unavailable (", conditionMessage(e), "); manual reference-grid prediction")
  ## per-disease median date, not one pooled median: the three case studies span
  ## disjoint calendar years (ebola 2014, cholera 2017, covid 2021), so a single
  ## global median would extrapolate each disease's s(date, by=disease) smooth far
  ## outside its own fitted range for at least two of the three diseases, which is
  ## exactly when spline confidence bands blow up.
  med_date <- dat |> dplyr::group_by(disease) |>
    dplyr::summarise(date_num = stats::median(date_num), .groups = "drop")
  grid <- dat |> dplyr::distinct(disease, condition) |>
    dplyr::left_join(med_date, by = "disease") |>
    dplyr::mutate(horizon = factor(1, levels = levels(dat$horizon)))
  pr <- predict(m, newdata = grid, se.fit = TRUE)
  grid$emmean <- pr$fit
  grid$lower.CL <- pr$fit - 1.96 * pr$se.fit
  grid$upper.CL <- pr$fit + 1.96 * pr$se.fit
  grid
})
readr::write_csv(marg, here("results", "q2_condition_marginal_effects_weekly.csv"))
cat("\n=== condition marginal log-CRPS (with 95% CI) ===\n")
print(as.data.frame(marg), digits = 3)

## Figure: marginal effects with CIs, faceted by disease
mcol <- if ("emmean" %in% names(marg)) "emmean" else "estimate"
lcol <- if ("lower.CL" %in% names(marg)) "lower.CL" else "asymp.LCL"
ucol <- if ("upper.CL" %in% names(marg)) "upper.CL" else "asymp.UCL"
p <- ggplot2::ggplot(marg, ggplot2::aes(x = condition, y = .data[[mcol]])) +
  ggplot2::geom_point() +
  ggplot2::geom_errorbar(ggplot2::aes(ymin = .data[[lcol]], ymax = .data[[ucol]]), width = 0.2) +
  ggplot2::facet_wrap(~ disease, scales = "free_y") +
  ggplot2::labs(x = "condition", y = "marginal log-scale CRPS (95% CI)",
                title = "Q2 (weekly-updated): impose vs learn - case-study forecast skill") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
ggplot2::ggsave(here("results", "fig_q2_marginal_effects_weekly.png"), p, width = 10, height = 4, dpi = 150)
message("Saved: results/fig_q2_marginal_effects_weekly.png")
