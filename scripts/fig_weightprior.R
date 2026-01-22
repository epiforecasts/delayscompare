############################
#### Weight Prior Figure ####
############################

# Compares forecast performance with weight_prior=TRUE vs FALSE
# Tests whether weighting the delay prior affects robustness to misspecification

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "generate_scores_func.R"))
source(here("R", "lshtm_theme.R"))

rt_opts <- "latest"

# GT/INC level labels
level_labels <- c("1" = "no delay", "2" = "very low", "3" = "low",
                  "4" = "correct", "5" = "high", "6" = "very high")

# Function to add gen_time/inc_period labels from gt/inc numbers
add_delay_labels <- function(df) {
  df |>
    mutate(
      gen_time = factor(level_labels[as.character(gt)],
                        levels = c("no delay", "very low", "low", "correct", "high", "very high")),
      inc_period = factor(level_labels[as.character(inc)],
                          levels = c("no delay", "very low", "low", "correct", "high", "very high"))
    )
}

# Load casestudy real data for true values
source(here("scripts", "datacollect_casestudy.R"))

# Disease display names
disease_names <- c(
  "ebola" = "Ebola",
  "covid" = "COVID-19"
)

# Store all results
all_rankings <- list()

# Process each disease and weight_prior setting
for(disease in c("ebola", "covid")) {
  for(wp in c("TRUE", "FALSE")) {

    message(paste("\n=== Processing", disease, "weight_prior =", wp, "===\n"))

    # Load processed weightprior results
    res_samples <- tryCatch(
      read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_", wp, "_", rt_opts, "_all_samples")),
      error = function(e) { message(paste("  Missing samples")); NULL }
    )
    res_id <- tryCatch(
      read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_", wp, "_", rt_opts, "_all_id")),
      error = function(e) { message(paste("  Missing id")); NULL }
    )

    if(is.null(res_samples) || is.null(res_id)) {
      message(paste("Skipping", disease, wp))
      next
    }

    # Get true values from casestudy data
    case_data <- casestudydata[[disease]]

    # Determine the column name for cases
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

    # Filter to forecasts only
    res_samples <- res_samples |>
      filter(type == "forecast")

    # Join with id info
    res_samples <- res_samples |>
      left_join(res_id, by = c("result_list", "gt", "inc"))

    # Join with true values
    res_samples <- res_samples |>
      left_join(true_cases, by = "date")

    # Remove rows without true values
    res_samples <- res_samples |>
      filter(!is.na(true_value))

    if(nrow(res_samples) == 0) {
      message(paste("  No matching data"))
      next
    }

    message(paste("  Found", nrow(res_samples), "sample rows"))

    # Create forecast object for scoring
    res_forecast <- tryCatch({
      as_forecast_sample(
        data = res_samples,
        forecast_unit = c("date", "type", "result_list", "gt", "inc", "model", "timepoint"),
        observed = "true_value",
        predicted = "prediction",
        sample_id = "sample"
      )
    }, error = function(e) {
      message(paste("  Error creating forecast object:", e$message))
      NULL
    })

    if(is.null(res_forecast)) next

    # Log transform for scoring
    res_forecast <- res_forecast |>
      transform_forecasts(fun = log_shift, offset = 1, label = "log")

    # Score
    scores_cases <- res_forecast |>
      filter(scale == "log") |>
      score()

    # Get rankings by GT and INC
    # Add gen_time/inc_period labels
    scores_cases <- add_delay_labels(scores_cases)

    scores_summary <- scores_cases |>
      group_by(timepoint, gt, inc) |>
      filter(date == max(date)) |>
      ungroup()

    # Calculate mean CRPS by GT and INC
    rankings <- scores_summary |>
      group_by(gt, inc, gen_time, inc_period) |>
      summarise(crps = mean(crps, na.rm = TRUE), .groups = "drop") |>
      mutate(
        rank = rank(crps),
        weight_prior = wp,
        disease = disease_names[disease]
      )

    all_rankings[[paste(disease, wp)]] <- rankings
    message(paste("  Done:", nrow(rankings), "GT×INC combinations"))
  }
}

if(length(all_rankings) == 0) {
  stop("No results found!")
}

# Combine all
rankings_combined <- bind_rows(all_rankings)

# Create comparison plots
# Facet by disease, compare weight_prior TRUE vs FALSE

p_comparison <- ggplot(rankings_combined, aes(x = gen_time, y = inc_period)) +
  geom_tile(aes(fill = crps)) +
  scale_fill_viridis_c(option = "plasma", name = "CRPS\n(log scale)") +
  facet_grid(disease ~ weight_prior, labeller = labeller(weight_prior = c("TRUE" = "weight_prior = TRUE", "FALSE" = "weight_prior = FALSE"))) +
  xlab("Generation time misspecification") +
  ylab("Incubation period misspecification") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  ggtitle("Effect of weight_prior on forecast performance")

ggsave(
  here("results", "fig_weightprior_comparison.png"),
  p_comparison,
  width = 12,
  height = 8,
  dpi = 150
)

# Difference plot (TRUE - FALSE)
rankings_wide <- rankings_combined |>
  select(gt, inc, gen_time, inc_period, disease, weight_prior, crps) |>
  pivot_wider(names_from = weight_prior, values_from = crps, names_prefix = "wp_") |>
  mutate(diff = wp_TRUE - wp_FALSE)  # Positive = TRUE is worse

p_diff <- ggplot(rankings_wide, aes(x = gen_time, y = inc_period)) +
  geom_tile(aes(fill = diff)) +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0,
                       name = "CRPS difference\n(TRUE - FALSE)") +
  facet_wrap(~disease, ncol = 2) +
  xlab("Generation time misspecification") +
  ylab("Incubation period misspecification") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  ggtitle("weight_prior effect: Blue = TRUE better, Red = FALSE better")

ggsave(
  here("results", "fig_weightprior_diff.png"),
  p_diff,
  width = 10,
  height = 5,
  dpi = 150
)

# Summary stats
summary_stats <- rankings_combined |>
  group_by(disease, weight_prior) |>
  summarise(
    mean_crps = mean(crps, na.rm = TRUE),
    sd_crps = sd(crps, na.rm = TRUE),
    .groups = "drop"
  )

print("Summary by disease and weight_prior:")
print(summary_stats)

# Marginal effects
marginal_wp <- rankings_combined |>
  group_by(disease, weight_prior) |>
  summarise(mean_crps = mean(crps, na.rm = TRUE), .groups = "drop")

p_bar <- ggplot(marginal_wp, aes(x = weight_prior, y = mean_crps, fill = weight_prior)) +
  geom_col() +
  facet_wrap(~disease, scales = "free_y") +
  scale_fill_manual(values = c("TRUE" = "steelblue", "FALSE" = "coral")) +
  xlab("weight_prior setting") +
  ylab("Mean CRPS (log scale)") +
  theme_classic() +
  theme(legend.position = "none") +
  ggtitle("Overall effect of weight_prior")

ggsave(
  here("results", "fig_weightprior_bar.png"),
  p_bar,
  width = 8,
  height = 4,
  dpi = 150
)

message("\n=== Weight prior figure generation complete ===\n")
