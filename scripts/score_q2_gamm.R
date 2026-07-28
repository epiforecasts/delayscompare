## Q2 impose-vs-learn: per-origin log-scale CRPS + GAMM with condition marginal
## effects (confidence intervals, no significance testing). Case-study (real data).
##
## Conditions per disease:
##   no_input   : casestudy gt=1, inc=1 (no delay structure)
##   impose     : casestudy gt=4, inc=4 (literature)
##   learn_gt   : widepriors weight_prior=FALSE, vary=gt
##   learn_delay: widepriors weight_prior=FALSE, vary=inc
##   learn_both : widepriors weight_prior=FALSE, vary=both
##
## Posterior samples are equalised to a common count so CRPS is comparable across
## conditions. Outcome = log-scale CRPS (CRPS of log-transformed forecast/obs).

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("scripts", "datacollect_casestudy.R"))

rt_opts   <- "latest"
diseases  <- c("ebola", "covid", "cholera")
N_SAMPLES <- 2000L   # equalise all conditions to this posterior sample count

case_col <- function(d) if ("confirm" %in% names(d)) "confirm" else "cases"

## per-forecast-origin log-scale CRPS for one condition
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
  samples <- samples |> dplyr::filter(sample <= N_SAMPLES)   # equalise sample count

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

rows <- list()
for (disease in diseases) {
  cd <- casestudydata[[disease]]
  true_cases <- cd |>
    dplyr::rename(true_value = !!rlang::sym(case_col(cd))) |>
    dplyr::select(date, true_value)

  cs_id   <- read_latest(here("results", "agg"), paste0(disease, "_casestudy_", rt_opts, "_id"))
  cs_samp <- read_latest(here("results", "agg"), paste0(disease, "_casestudy_", rt_opts, "_samples"))
  cs_diag <- tryCatch(read_latest(here("results", "agg"),
                                  paste0(disease, "_casestudy_", rt_opts, "_diagnostics")),
                      error = function(e) NULL)

  ## impose-arm conditions (R-hat filter on)
  for (cell in list(c("no_input", 1, 1), c("impose", 4, 4))) {
    lab <- cell[[1]]; g <- as.integer(cell[[2]]); i <- as.integer(cell[[3]])
    sc <- score_origins(cs_samp |> dplyr::filter(gt == g, inc == i),
                        cs_id   |> dplyr::filter(gt == g, inc == i),
                        if (!is.null(cs_diag)) cs_diag |> dplyr::filter(gt == g, inc == i) else NULL,
                        true_cases, filter_rhat = TRUE)
    if (!is.null(sc)) rows[[paste(disease, lab)]] <- sc |>
      dplyr::mutate(disease = disease, condition = lab)
  }

  ## learn-arm conditions (report non-convergence, filter off)
  for (cond in list(c("learn_gt", "gt"), c("learn_delay", "inc"), c("learn_both", "both"))) {
    lab <- cond[[1]]; v <- cond[[2]]
    pfx <- paste0("res_", disease, "_widepriors_FALSE_", v, "_", rt_opts, "_all_")
    ei <- tryCatch(read_latest(here("results", "widepriors"), paste0(pfx, "id")),          error = function(e) NULL)
    es <- tryCatch(read_latest(here("results", "widepriors"), paste0(pfx, "samples")),     error = function(e) NULL)
    ed <- tryCatch(read_latest(here("results", "widepriors"), paste0(pfx, "diagnostics")), error = function(e) NULL)
    sc <- score_origins(es, ei, ed, true_cases, filter_rhat = FALSE, case_data = cd)
    if (!is.null(sc)) rows[[paste(disease, lab)]] <- sc |>
      dplyr::mutate(disease = disease, condition = lab)
  }
}

dat <- dplyr::bind_rows(rows)
dat$condition <- factor(dat$condition,
                        levels = c("no_input", "impose", "learn_gt", "learn_delay", "learn_both"))
dat$disease   <- factor(dat$disease, levels = diseases)
dat$horizon   <- factor(dat$horizon)
dat$time      <- dat$timepoint
readr::write_csv(dat, here("results", "q2_perorigin_logcrps.csv"))
message("per-origin scores: ", nrow(dat), " rows; ",
        "conditions x disease present:")
print(as.data.frame(table(dat$disease, dat$condition)))

## GAMM: log-scale CRPS ~ condition*disease + s(time, by=disease) + horizon
library(mgcv)
n_time <- length(unique(dat$time))
k_time <- max(3L, min(5L, n_time - 1L))
form <- log_crps ~ condition * disease + s(time, by = disease, k = k_time) + horizon
m <- bam(form, data = dat, method = "fREML")
saveRDS(m, here("results", "q2_gamm.rds"))
cat("\n=== GAMM parametric coefficients ===\n")
print(summary(m)$p.table)

## Condition marginal effects within disease, with 95% CIs
marg <- tryCatch({
  emm <- emmeans::emmeans(m, ~ condition | disease)
  as.data.frame(summary(emm, infer = c(TRUE, FALSE)))
}, error = function(e) {
  message("emmeans unavailable (", conditionMessage(e), "); manual reference-grid prediction")
  grid <- dat |> dplyr::distinct(disease, condition) |>
    dplyr::mutate(time = stats::median(dat$time), horizon = factor(1, levels = levels(dat$horizon)))
  pr <- predict(m, newdata = grid, se.fit = TRUE)
  grid$emmean <- pr$fit
  grid$lower.CL <- pr$fit - 1.96 * pr$se.fit
  grid$upper.CL <- pr$fit + 1.96 * pr$se.fit
  grid
})
readr::write_csv(marg, here("results", "q2_condition_marginal_effects.csv"))
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
                title = "Q2: impose vs learn — case-study forecast skill") +
  ggplot2::theme_bw() +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
ggplot2::ggsave(here("results", "fig_q2_marginal_effects.png"), p, width = 10, height = 4, dpi = 150)
message("Saved: results/fig_q2_marginal_effects.png")
