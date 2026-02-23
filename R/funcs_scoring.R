###################################
#### Shared scoring functions  ####
###################################

# Standard GT/INC level labels and ordering
level_labels <- c("1" = "no delay", "2" = "very low", "3" = "low",
                  "4" = "correct", "5" = "high", "6" = "very high")

level_order <- c("no delay", "very low", "low", "correct", "high", "very high")

#' Convert numeric gt/inc columns to labelled gen_time/inc_period factors
add_delay_labels <- function(df) {
  df |>
    mutate(
      gen_time = factor(level_labels[as.character(gt)], levels = level_order),
      inc_period = factor(level_labels[as.character(inc)], levels = level_order)
    )
}

#' Filter results to keep only converged fits based on MCMC diagnostics
#'
#' @param df Data frame with result_list, gt, inc columns (e.g. res_samples)
#' @param diagnostics Data frame from extract_diagnostics() with result_list,
#'   gt, inc, max_rhat, num_divergent, min_ess
#' @param max_rhat Maximum acceptable Rhat (default 1.05)
#' @param min_ess Minimum acceptable ESS (default NULL = no filter)
#' @param max_divergent Maximum acceptable divergent transitions (default NULL)
#' @return Filtered data frame (semi-join on converged fits)
filter_by_diagnostics <- function(df, diagnostics,
                                  max_rhat = 1.05,
                                  min_ess = NULL,
                                  max_divergent = NULL) {
  n_before <- length(unique(paste(df$result_list, df$gt, df$inc)))

  if (n_before == 0) {
    message("  Diagnostics filter: no fits to filter")
    return(df)
  }

  converged <- diagnostics |>
    filter(!is.na(max_rhat))

  if (!is.null(max_rhat)) {
    converged <- converged |> filter(max_rhat <= .env$max_rhat)
  }
  if (!is.null(min_ess)) {
    converged <- converged |> filter(min_ess >= .env$min_ess)
  }
  if (!is.null(max_divergent)) {
    converged <- converged |> filter(num_divergent <= .env$max_divergent)
  }

  df_filtered <- df |>
    semi_join(converged, by = c("result_list", "gt", "inc"))

  n_after <- length(unique(paste(df_filtered$result_list, df_filtered$gt, df_filtered$inc)))
  pct_removed <- round(100 * (1 - n_after / n_before), 1)
  message(paste0("  Diagnostics filter: removed ", n_before - n_after, "/",
                 n_before, " fits (", pct_removed, "%)"))

  df_filtered
}

#' Score case forecasts using CRPS on log scale
#'
#' @param res_samples Forecast samples (with prediction, sample, date columns)
#' @param res_id ID table with result_list, gt, inc, timepoint columns
#' @param true_cases Data frame with date and true_value columns
#' @return Scores data frame from scoringutils::score()
score_case_forecasts <- function(res_samples, res_id, true_cases) {
  # Deduplicate res_id to one row per (result_list, gt, inc) to avoid
  # many-to-many joins when result_list is not unique per timepoint
  id_dedup <- res_id |>
    distinct(result_list, gt, inc, .keep_all = TRUE)

  res_samples <- res_samples |>
    filter(type == "forecast") |>
    left_join(id_dedup, by = c("result_list", "gt", "inc")) |>
    left_join(true_cases, by = "date") |>
    filter(!is.na(true_value))

  if (nrow(res_samples) == 0) return(NULL)

  res_forecast <- tryCatch({
    as_forecast_sample(
      data = res_samples,
      forecast_unit = c("date", "type", "result_list", "gt", "inc",
                        "model", "timepoint"),
      observed = "true_value",
      predicted = "prediction",
      sample_id = "sample"
    )
  }, error = function(e) {
    message(paste("  Error creating forecast object:", e$message))
    NULL
  })

  if (is.null(res_forecast)) return(NULL)

  res_forecast |>
    transform_forecasts(fun = log_shift, offset = 1, label = "log") |>
    filter(scale == "log") |>
    score()
}

#' Score Rt forecasts using CRPS on natural scale
#'
#' @param res_R Rt samples (with value, sample, date columns)
#' @param res_id ID table
#' @param rt_truth Data frame with date and true R(t) columns
#' @return Scores data frame
score_rt_forecasts <- function(res_R, res_id, rt_truth) {
  id_dedup <- res_id |>
    distinct(result_list, gt, inc, .keep_all = TRUE)

  res_R <- res_R |>
    filter(type == "forecast") |>
    left_join(id_dedup, by = c("result_list", "gt", "inc")) |>
    left_join(rt_truth, by = "date") |>
    filter(!is.na(R))

  if (nrow(res_R) == 0) return(NULL)

  res_forecast <- tryCatch({
    as_forecast_sample(
      data = res_R,
      forecast_unit = c("date", "type", "result_list", "gt", "inc",
                        "model", "timepoint"),
      observed = "R",
      predicted = "value",
      sample_id = "sample"
    )
  }, error = function(e) {
    message(paste("  Error creating Rt forecast object:", e$message))
    NULL
  })

  if (is.null(res_forecast)) return(NULL)

  res_forecast |>
    score()
}

#' Summarise scores into mean CRPS rankings by GT x INC
#'
#' @param scores Output from score_case_forecasts() or score_rt_forecasts()
#' @return Data frame with gt, inc, gen_time, inc_period, crps, rank
summarise_rankings <- function(scores) {
  scores |>
    group_by(timepoint, gt, inc) |>
    filter(date == max(date)) |>
    ungroup() |>
    group_by(gt, inc) |>
    summarise(crps = mean(crps, na.rm = TRUE), .groups = "drop") |>
    add_delay_labels() |>
    mutate(rank = rank(crps))
}
