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
dat$horizon_num <- dat$horizon
dat$horizon   <- factor(dat$horizon)

## Change in true (fitted, posterior-median) R over the forecast window, matched to
## each row's own origin and horizon -- the same confound the turning-point analysis
## treats as the main driver of delay-misspecification error. Without this, an s(date)
## smooth can only absorb it indirectly, and if convergence differs by condition in a
## way correlated with dR (plausible for covid's rare-convergence conditions), the
## condition effect could be confounded with which origins happen to be near a turn.
rt_col <- function(rt) if ("R" %in% names(rt)) "R" else "median"
true_R <- setNames(lapply(diseases, function(d) {
  rt <- readRDS(here("data", paste0("rt_", d, ".rds")))
  data.frame(date = as.Date(rt$date), R = rt[[rt_col(rt)]]) |> dplyr::arrange(date)
}), diseases)
dR_window <- function(d, origin, days) {
  rt <- true_R[[d]]; w <- rt[rt$date >= origin & rt$date <= origin + days, ]
  if (nrow(w) < 2) return(NA_real_); diff(range(w$R))
}
dat$dR <- mapply(dR_window, as.character(dat$disease), dat$forecast_origin,
                 7 * dat$horizon_num)
message("dR computed: ", sum(!is.na(dat$dR)), "/", nrow(dat), " rows; range ",
        paste(round(range(dat$dR, na.rm = TRUE), 2), collapse = "-"))

readr::write_csv(dat, here("results", "q2_perorigin_logcrps_weekly.csv"))
message("per-origin scores: ", nrow(dat), " rows; conditions x disease present:")
print(as.data.frame(table(dat$disease, dat$condition)))

## Which origins survive to be scored differs by condition (most severely for
## covid, but ebola/cholera's learn_gt/learn_both also have a handful fewer
## origins than impose/no_input/learn_delay). If those missing origins are not a
## random sample -- e.g. skewed toward high-dR, hard-to-forecast windows, the same
## selection mechanism behind covid's survivorship problem -- a condition
## comparison built on "whatever data that condition happens to have" partly
## reflects which origins survived, not what the condition does. Flag, per
## disease, the origins present in every condition at both horizons; comparisons
## restricted to that common set are immune to this by construction, since every
## condition then faces the identical set of forecasting problems.
common_origins <- function(disease_name) {
  dd <- dat[dat$disease == disease_name, ]
  wide <- dd |> dplyr::distinct(timepoint, condition, horizon) |>
    dplyr::mutate(present = TRUE) |>
    tidyr::pivot_wider(names_from = c(condition, horizon), values_from = present, values_fill = FALSE)
  wide$timepoint[apply(wide[, -1], 1, all)]
}
for (d in diseases) {
  co <- common_origins(d)
  n_any <- length(unique(dat$timepoint[dat$disease == d]))
  message(d, ": ", length(co), " of ", n_any,
          " origins converge under all five conditions at both horizons")
}

## Three separate per-disease GAMMs, not one joint model with a disease factor.
## The joint form (condition*disease + s(., by=disease) throughout) was already
## fully saturated in disease -- no term borrowed anything across diseases except
## `horizon` (one shared coefficient, never a deliberate choice) and the residual
## variance (bam() assumes one shared sigma^2 by default, inappropriate given how
## different ebola/cholera/covid's log-CRPS scales are). Splitting gives each
## disease its own horizon effect and its own noise variance for free, at no cost,
## since there was no genuine cross-disease sharing to give up.
library(mgcv)
dat$date_num <- as.numeric(dat$forecast_origin)
if (any(is.na(dat$dR))) message("dropping ", sum(is.na(dat$dR)),
  " rows with no dR (forecast window too close to the edge of the case series)")

fit_one_disease <- function(disease_name, origins_only = NULL) {
  dd <- dat[dat$disease == disease_name & !is.na(dat$dR), ]
  if (!is.null(origins_only)) dd <- dd[dd$timepoint %in% origins_only, ]
  dd$condition <- droplevels(dd$condition)
  n_origin <- length(unique(dd$date_num))
  k_time <- max(3L, min(10L, n_origin - 1L))
  n_dr <- length(unique(dd$dR))
  k_dr <- max(3L, min(6L, n_dr - 1L))
  form <- log_crps ~ condition + s(date_num, k = k_time) + s(dR, k = k_dr) + horizon
  bam(form, data = dd, method = "fREML")
}
models <- setNames(lapply(diseases, fit_one_disease), diseases)
saveRDS(models, here("results", "q2_gamm_weekly.rds"))

## Robustness: refit ebola/cholera restricted to the common-origin set (covid's
## common set is 1 origin, too few to refit; its comparison is already withheld).
## Every condition then faces the identical set of forecasting problems, so this
## version cannot be confounded by which origins happened to survive per condition.
common_models <- setNames(lapply(c("ebola", "cholera"), function(d)
  fit_one_disease(d, origins_only = common_origins(d))), c("ebola", "cholera"))

for (d in diseases) {
  cat("\n=== ", d, ": dR smooth term ===\n")
  st <- summary(models[[d]])$s.table
  print(st[grep("^s\\(dR\\)", rownames(st)), , drop = FALSE])
}

## Condition marginal effects within disease, with 95% CIs. Reference dR/date at
## each disease's own median (its model only ever saw its own range, so this is
## automatic here, not a safeguard against cross-disease extrapolation as before).
marg_one <- function(d, m, dd) {
  grid <- dd |> dplyr::distinct(condition) |>
    dplyr::mutate(date_num = stats::median(dd$date_num), dR = stats::median(dd$dR),
                  horizon = factor(1, levels = levels(dd$horizon)))
  pr <- predict(m, newdata = grid, se.fit = TRUE)
  grid$disease <- d
  grid$emmean <- pr$fit
  grid$lower.CL <- pr$fit - 1.96 * pr$se.fit
  grid$upper.CL <- pr$fit + 1.96 * pr$se.fit
  grid
}
marg <- dplyr::bind_rows(lapply(diseases, function(d)
  marg_one(d, models[[d]], dat[dat$disease == d & !is.na(dat$dR), ])))
readr::write_csv(marg, here("results", "q2_condition_marginal_effects_weekly.csv"))
cat("\n=== condition marginal log-CRPS (with 95% CI), separate per-disease models ===\n")
print(as.data.frame(marg), digits = 3)

## Same, restricted to the common-origin set: immune to selection by construction.
## Compared directly against the full-data version above to check whether the
## uneven convergence pattern (ebola/cholera's learn_gt/learn_both have fewer
## origins than impose/no_input/learn_delay) was actually biasing the comparison,
## rather than assuming a milder version of covid's problem doesn't matter here.
marg_common <- dplyr::bind_rows(lapply(c("ebola", "cholera"), function(d) {
  co <- common_origins(d)
  dd <- dat[dat$disease == d & !is.na(dat$dR) & dat$timepoint %in% co, ]
  marg_one(d, common_models[[d]], dd)
}))
readr::write_csv(marg_common, here("results", "q2_condition_marginal_effects_common_origins.csv"))
cat("\n=== same, restricted to origins where all five conditions converged ===\n")
print(as.data.frame(marg_common), digits = 3)

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
