############################
#### Weight Prior Figure ####
############################

# Compares forecast performance with weight_prior=TRUE vs FALSE
# across vary conditions (gt, inc, both) for each disease.
# The weightprior scenario uses correct delay parameters but with
# parametric uncertainty, allowing EpiNow2 to estimate delays from data.

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "funcs_scoring.R"))
source(here("R", "lshtm_theme.R"))

rt_opts <- "latest"

# Load casestudy real data for true values
source(here("scripts", "datacollect_casestudy.R"))

# Disease display names
disease_names <- c(
  "ebola" = "Ebola",
  "covid" = "COVID-19",
  "cholera" = "Cholera"
)

# Vary condition labels
vary_labels <- c(
  "gt" = "Generation time",
  "inc" = "Incubation period",
  "both" = "Both"
)

# Store all results
all_scores <- list()

# Process each disease, vary setting, and weight_prior setting
for(disease in c("ebola", "covid", "cholera")) {
  for(vary in c("gt", "inc", "both")) {
    for(wp in c("TRUE", "FALSE")) {

      message(paste("\n=== Processing", disease, "vary =", vary, "weight_prior =", wp, "===\n"))

      file_prefix <- paste0("res_", disease, "_weightprior_", wp, "_", vary, "_", rt_opts, "_all_")

      # Load processed weightprior results
      res_samples <- tryCatch(
        read_latest(here("results/weightprior"), paste0(file_prefix, "samples")),
        error = function(e) { message(paste("  Missing samples")); NULL }
      )
      res_id <- tryCatch(
        read_latest(here("results/weightprior"), paste0(file_prefix, "id")),
        error = function(e) { message(paste("  Missing id")); NULL }
      )

      if(is.null(res_samples) || is.null(res_id)) {
        message(paste("Skipping", disease, vary, wp))
        next
      }

      # Load and apply diagnostics filter (on result_list only, no gt/inc)
      diagnostics <- tryCatch(
        read_latest(here("results/weightprior"), paste0(file_prefix, "diagnostics")),
        error = function(e) { message(paste("  No diagnostics")); NULL }
      )
      if(!is.null(diagnostics)) {
        converged <- diagnostics |>
          filter(!is.na(max_rhat), max_rhat <= 1.05)
        n_before <- length(unique(res_samples$result_list))
        res_samples <- res_samples |>
          semi_join(converged, by = "result_list")
        n_after <- length(unique(res_samples$result_list))
        message(paste0("  Diagnostics filter: removed ", n_before - n_after, "/",
                       n_before, " fits (", round(100 * (1 - n_after / n_before), 1), "%)"))
      }

      if(nrow(res_samples) == 0) {
        message(paste("  No samples after filtering"))
        next
      }

      # Get true values from casestudy data
      case_data <- casestudydata[[disease]]
      if("confirm" %in% names(case_data)) {
        case_col <- "confirm"
      } else if("cases" %in% names(case_data)) {
        case_col <- "cases"
      } else {
        message(paste("  Unknown case column for", disease))
        next
      }

      true_cases <- case_data |>
        rename(true_value = !!sym(case_col)) |>
        select(date, true_value)

      # Score forecasts directly (no gt/inc structure)
      id_dedup <- res_id |>
        distinct(result_list, .keep_all = TRUE)

      scored <- res_samples |>
        filter(type == "forecast") |>
        left_join(id_dedup, by = "result_list") |>
        left_join(true_cases, by = "date") |>
        filter(!is.na(true_value))

      if(nrow(scored) == 0) {
        message(paste("  No forecast data to score"))
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

      # Mean CRPS across timepoints (using last forecast date per timepoint)
      mean_crps <- scores |>
        group_by(timepoint) |>
        filter(date == max(date)) |>
        ungroup() |>
        summarise(crps = mean(crps, na.rm = TRUE)) |>
        pull(crps)

      all_scores[[paste(disease, vary, wp)]] <- tibble(
        disease = disease_names[disease],
        vary = vary,
        vary_label = vary_labels[vary],
        weight_prior = wp,
        crps = mean_crps
      )

      message(paste("  Done:", disease, vary, wp, "CRPS =", round(mean_crps, 4)))
    }
  }
}

if(length(all_scores) == 0) {
  stop("No results found!")
}

# Combine all
scores_combined <- bind_rows(all_scores)

print("Scores by disease, vary, and weight_prior:")
print(scores_combined, n = 50)

#### Bar chart: CRPS by disease x vary x weight_prior ####

p_bar <- ggplot(scores_combined,
                aes(x = vary_label, y = crps, fill = weight_prior)) +
  geom_col(position = position_dodge(width = 0.7), width = 0.6) +
  geom_text(aes(label = round(crps, 2)),
            position = position_dodge(width = 0.7), vjust = -0.3, size = 2.5) +
  facet_wrap(~disease, scales = "free_y") +
  scale_fill_manual(
    values = c("TRUE" = "steelblue", "FALSE" = "coral"),
    labels = c("TRUE" = "weight_prior = TRUE", "FALSE" = "weight_prior = FALSE"),
    name = ""
  ) +
  xlab("Delay with parametric uncertainty") +
  ylab("Mean CRPS (log scale)") +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold"),
    legend.position = "bottom"
  )

ggsave(
  here("results", "fig_weightprior_comparison.png"),
  p_bar,
  width = 10,
  height = 5,
  dpi = 150
)

#### Difference plot: wp=TRUE minus wp=FALSE ####

scores_wide <- scores_combined |>
  select(disease, vary, vary_label, weight_prior, crps) |>
  pivot_wider(names_from = weight_prior, values_from = crps, names_prefix = "wp_") |>
  mutate(
    diff = wp_TRUE - wp_FALSE,
    pct_diff = 100 * (wp_TRUE - wp_FALSE) / wp_FALSE
  )

print("Difference (TRUE - FALSE), positive = TRUE is worse:")
print(scores_wide)

p_diff <- ggplot(scores_wide, aes(x = vary_label, y = pct_diff, fill = diff > 0)) +
  geom_col() +
  geom_hline(yintercept = 0, linetype = "dashed") +
  facet_wrap(~disease) +
  scale_fill_manual(values = c("TRUE" = "coral", "FALSE" = "steelblue"), guide = "none") +
  xlab("Delay with parametric uncertainty") +
  ylab("% change in CRPS (TRUE vs FALSE)") +
  theme_classic() +
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

ggsave(
  here("results", "fig_weightprior_diff.png"),
  p_diff,
  width = 10,
  height = 4,
  dpi = 150
)

message("\n=== Weight prior figure generation complete ===\n")
