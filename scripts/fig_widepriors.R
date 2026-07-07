#######################################
#### Wide-prior case-study figure ####
#######################################

# Figures for the regime-2 wide-prior rerun.
# weight_prior is fixed to FALSE (learn-from-data without prior weighting);
# the prior SDs were widened to 25% relative.
# Input prefix: res_<disease>_widepriors_FALSE_<vary>_<rt>_all_*
# Read from results/widepriors/ (populated by scripts/07_resultsprocessing.R).

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "funcs_scoring.R"))
source(here("R", "lshtm_theme.R"))

rt_opts <- "latest"
wp <- "FALSE"

source(here("scripts", "datacollect_casestudy.R"))

disease_names <- c(
  "ebola" = "Ebola",
  "covid" = "COVID-19",
  "cholera" = "Cholera"
)

vary_labels <- c(
  "gt" = "Generation time",
  "inc" = "Incubation period",
  "both" = "Both"
)

all_scores <- list()

for(disease in c("ebola", "covid", "cholera")) {
  for(vary in c("gt", "inc", "both")) {

    message(paste("\n=== Processing", disease, "vary =", vary, "(wide priors) ===\n"))

    file_prefix <- paste0("res_", disease, "_widepriors_", wp, "_", vary, "_", rt_opts, "_all_")

    res_samples <- tryCatch(
      read_latest(here("results/widepriors"), paste0(file_prefix, "samples")),
      error = function(e) { message("  Missing samples"); NULL }
    )
    res_id <- tryCatch(
      read_latest(here("results/widepriors"), paste0(file_prefix, "id")),
      error = function(e) { message("  Missing id"); NULL }
    )

    if(is.null(res_samples) || is.null(res_id)) {
      message(paste("Skipping", disease, vary))
      next
    }

    diagnostics <- tryCatch(
      read_latest(here("results/widepriors"), paste0(file_prefix, "diagnostics")),
      error = function(e) { message("  No diagnostics"); NULL }
    )
    if(!is.null(diagnostics)) {
      converged <- diagnostics |>
        filter(!is.na(max_rhat), max_rhat <= 1.05)
      n_before <- length(unique(res_samples$result_list))
      res_samples <- res_samples |>
        semi_join(converged, by = "result_list")
      n_after <- length(unique(res_samples$result_list))
      message(paste0("  Rhat filter: kept ", n_after, "/", n_before, " fits"))
    }

    if(nrow(res_samples) == 0) {
      message("  No samples after filtering")
      next
    }

    case_data <- casestudydata[[disease]]
    case_col <- if("confirm" %in% names(case_data)) "confirm" else "cases"

    true_cases <- case_data |>
      rename(true_value = !!sym(case_col)) |>
      select(date, true_value)

    id_dedup <- res_id |>
      distinct(result_list, .keep_all = TRUE)

    scored <- res_samples |>
      filter(type == "forecast") |>
      left_join(id_dedup, by = "result_list") |>
      left_join(true_cases, by = "date") |>
      filter(!is.na(true_value))

    if(nrow(scored) == 0) {
      message("  No forecast rows to score")
      next
    }

    res_forecast <- tryCatch({
      as_forecast_sample(
        data = scored,
        forecast_unit = c("date", "type", "result_list", "model", "timepoint"),
        observed = "true_value",
        predicted = "prediction",
        sample_id = "sample"
      )
    }, error = function(e) {
      message(paste("  Error creating forecast object:", e$message))
      NULL
    })

    if(is.null(res_forecast)) next

    scores <- res_forecast |>
      transform_forecasts(fun = log_shift, offset = 1, label = "log") |>
      filter(scale == "log") |>
      score()

    mean_crps <- scores |>
      group_by(timepoint) |>
      filter(date == max(date)) |>
      ungroup() |>
      summarise(crps = mean(crps, na.rm = TRUE)) |>
      pull(crps)

    all_scores[[paste(disease, vary)]] <- tibble(
      disease = disease_names[disease],
      vary = vary,
      vary_label = vary_labels[vary],
      crps = mean_crps
    )

    message(paste("  Done:", disease, vary, "CRPS =", round(mean_crps, 4)))
  }
}

if(length(all_scores) == 0) stop("No wide-prior results found.")

scores_combined <- bind_rows(all_scores)

print("Wide-prior CRPS by disease and vary:")
print(scores_combined, n = 50)

#### Bar chart: wide-prior CRPS by disease x vary ####

p_bar <- ggplot(scores_combined,
                aes(x = vary_label, y = crps, fill = vary_label)) +
  geom_col(width = 0.6) +
  geom_text(aes(label = round(crps, 2)), vjust = -0.3, size = 2.5) +
  facet_wrap(~disease, scales = "free_y") +
  xlab("Delay with parametric uncertainty (wide prior)") +
  ylab("Mean CRPS (log scale)") +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "none"
  )

ggsave(
  here("results", "fig_widepriors_crps.png"),
  p_bar,
  width = 10,
  height = 5,
  dpi = 150
)

message("\n=== Wide-prior figure generation complete ===\n")
