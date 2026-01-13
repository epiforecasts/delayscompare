######################
#### Resim Figure ####
######################

# Generates figures for re-simulated disease-like data
# Uses simulated data with known GT/INC ground truth

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

# Disease display names
disease_names <- c(
  "ebola" = "Ebola-like",
  "covid" = "COVID-like",
  "cholera" = "Cholera-like"
)

# Store all disease rankings
all_disease_rankings <- list()

# Process each disease
for(disease in c("ebola", "covid", "cholera")) {

  message(paste("\n=== Processing", disease, "resim ===\n"))

  # Load processed resim results
  res_samples <- tryCatch(
    read_latest(here("results/sim"), paste0("res_", disease, "_resim_", rt_opts, "_all_samples")),
    error = function(e) { message(paste("  Missing samples for", disease)); NULL }
  )
  res_id <- tryCatch(
    read_latest(here("results/sim"), paste0("res_", disease, "_resim_", rt_opts, "_all_id")),
    error = function(e) { message(paste("  Missing id for", disease)); NULL }
  )

  if(is.null(res_samples) || is.null(res_id)) {
    message(paste("Skipping", disease, "- missing data"))
    next
  }

  # Load simulated data for true values
  sim_data <- tryCatch(
    read_latest(here("data"), paste0(disease, "_sim_data")),
    error = function(e) { message(paste("  Missing sim_data for", disease)); NULL }
  )

  if(is.null(sim_data)) {
    message(paste("Skipping", disease, "- no sim_data"))
    next
  }

  # Get true values (reported cases)
  true_cases <- sim_data |>
    filter(variable == "reported_cases") |>
    rename(true_value = value) |>
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
    message(paste("  No matching data for", disease))
    next
  }

  message(paste("  Found", nrow(res_samples), "sample rows"))
  message(paste("  GT×INC combinations:", length(unique(paste(res_samples$gt, res_samples$inc)))))

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

  # Add gen_time/inc_period labels
  scores_cases <- scores_cases |>
    mutate(
      gen_time = factor(level_labels[as.character(gt)],
                        levels = c("no delay", "very low", "low", "correct", "high", "very high")),
      inc_period = factor(level_labels[as.character(inc)],
                          levels = c("no delay", "very low", "low", "correct", "high", "very high"))
    )

  # Get rankings by GT and INC
  scores_summary <- scores_cases |>
    group_by(timepoint, gt, inc) |>
    filter(date == max(date)) |>
    ungroup()

  # Calculate mean CRPS by GT and INC
  rankings_cases <- scores_summary |>
    group_by(gt, inc, gen_time, inc_period) |>
    summarise(crps = mean(crps, na.rm = TRUE), .groups = "drop") |>
    mutate(
      rank = rank(crps),
      disease_label = disease_names[disease]
    )

  message(paste("  Done:", nrow(rankings_cases), "GT×INC combinations"))

  # Store for combined plot
  all_disease_rankings[[disease]] <- rankings_cases

  # Create individual disease heatmap
  p_heatmap <- ggplot(rankings_cases, aes(x = gen_time, y = inc_period)) +
    geom_tile(aes(fill = crps)) +
    scale_fill_viridis_c(option = "plasma", name = "CRPS\n(log scale)") +
    xlab("Generation time misspecification") +
    ylab("Incubation period misspecification") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    ggtitle(paste0(disease_names[disease], " (re-simulated, known GT/INC)"))

  ggsave(
    here("results", paste0("fig_resim_", disease, "_crps_heatmap.png")),
    p_heatmap,
    width = 8,
    height = 6,
    dpi = 150
  )

  message(paste("Saved figure for", disease))
}

# Create combined plot if we have results
if(length(all_disease_rankings) > 0) {

  rankings_combined <- bind_rows(all_disease_rankings)

  # Combined CRPS heatmap
  p_combined_crps <- ggplot(rankings_combined, aes(x = gen_time, y = inc_period)) +
    geom_tile(aes(fill = crps)) +
    scale_fill_viridis_c(option = "plasma", name = "CRPS\n(log scale)") +
    facet_wrap(~disease_label, ncol = 3, scales = "free") +
    xlab("Generation time misspecification") +
    ylab("Incubation period misspecification") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 10)
    ) +
    ggtitle("Resim: Forecast performance on re-simulated data (known GT/INC)")

  ggsave(
    here("results", "fig_resim_combined_crps.png"),
    p_combined_crps,
    width = 14,
    height = 5,
    dpi = 150
  )

  # Marginal effects
  marginal_gt <- rankings_combined |>
    group_by(gen_time, disease_label) |>
    summarise(mean_crps = mean(crps, na.rm = TRUE), .groups = "drop")

  p_gt <- ggplot(marginal_gt, aes(x = gen_time, y = mean_crps)) +
    geom_col(fill = "steelblue") +
    facet_wrap(~disease_label, scales = "free_y", ncol = 3) +
    xlab("Generation time misspecification") +
    ylab("Mean CRPS (log scale)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  marginal_inc <- rankings_combined |>
    group_by(inc_period, disease_label) |>
    summarise(mean_crps = mean(crps, na.rm = TRUE), .groups = "drop")

  p_inc <- ggplot(marginal_inc, aes(x = inc_period, y = mean_crps)) +
    geom_col(fill = "darkgreen") +
    facet_wrap(~disease_label, scales = "free_y", ncol = 3) +
    xlab("Incubation period misspecification") +
    ylab("Mean CRPS (log scale)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  p_marginal <- cowplot::plot_grid(p_gt, p_inc, ncol = 1, labels = c("A", "B"))

  ggsave(
    here("results", "fig_resim_marginal.png"),
    p_marginal,
    width = 12,
    height = 10,
    dpi = 150
  )

  message("\n=== Resim figure generation complete ===\n")
}
