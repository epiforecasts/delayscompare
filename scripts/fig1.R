##################
#### Figure 1 ####
##################

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "generate_scores_func.R"))
source(here("R", "lshtm_theme.R"))

rt_opts <- "latest"

# Scenario labels for plotting
scen_labels <- c(
  "const_low" = "Constant Rt (low)",
  "const_high" = "Constant Rt (high)",
  "inc" = "Increasing Rt",
  "dec" = "Decreasing Rt"
)

# GT/INC level labels
level_labels <- c("1" = "no delay", "2" = "very low", "3" = "low",
                  "4" = "correct", "5" = "high", "6" = "very high")

# Process each disease
for(disease in c("ebola", "covid", "cholera")) {

  message(paste("\n=== Processing", disease, "===\n"))

  # Load simulated data for true values
  # Try scenario-specific files first, then fall back to generic
  sim_data_const <- tryCatch(
    read_latest(here("data"), paste0(disease, "_sim_data_const")),
    error = function(e) NULL
  )
  sim_data_inc <- tryCatch(
    read_latest(here("data"), paste0(disease, "_sim_data_inc")),
    error = function(e) NULL
  )
  sim_data_dec <- tryCatch(
    read_latest(here("data"), paste0(disease, "_sim_data_dec")),
    error = function(e) NULL
  )

  # Fall back to generic sim data if scenario-specific not found
  sim_data_generic <- tryCatch(
    read_latest(here("data"), paste0(disease, "_sim_data")),
    error = function(e) NULL
  )
  if(is.null(sim_data_const)) sim_data_const <- sim_data_generic
  if(is.null(sim_data_inc)) sim_data_inc <- sim_data_generic
  if(is.null(sim_data_dec)) sim_data_dec <- sim_data_generic

  # Store rankings for all scenarios
  rankings_cases_all <- list()

  for(scen in c("const_low", "const_high", "inc", "dec")) {

    message(paste("Processing scenario:", scen))

    # Load processed results
    res_samples <- tryCatch(
      read_latest(here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_samples")),
      error = function(e) { message(paste("  Missing samples for", scen)); NULL }
    )
    res_id <- tryCatch(
      read_latest(here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_id")),
      error = function(e) { message(paste("  Missing id for", scen)); NULL }
    )

    if(is.null(res_samples) || is.null(res_id)) next

    # Select appropriate sim data for true values
    if(grepl("const", scen)) {
      sim_data <- sim_data_const
    } else if(scen == "inc") {
      sim_data <- sim_data_inc
    } else {
      sim_data <- sim_data_dec
    }

    if(is.null(sim_data)) {
      message(paste("  No sim data for", scen))
      next
    }

    # Filter to forecasts only
    res_samples <- res_samples |>
      filter(type == "forecast")

    # Join with id info
    res_samples <- res_samples |>
      left_join(res_id, by = c("result_list", "gt", "inc"))

    # Get true values (reported cases)
    true_cases <- sim_data |>
      filter(variable == "reported_cases") |>
      rename(true_value = value) |>
      select(date, true_value)

    # Join with true values
    res_samples <- res_samples |>
      left_join(true_cases, by = "date")

    # Remove rows without true values
    res_samples <- res_samples |>
      filter(!is.na(true_value))

    if(nrow(res_samples) == 0) {
      message(paste("  No matching data for", scen))
      next
    }

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
    # First, get the last forecast date per timepoint (the actual forecast target)
    scores_summary <- scores_cases |>
      left_join(res_id |> select(result_list, gt, inc, timepoint, gen_time, inc_period),
                by = c("result_list", "gt", "inc", "timepoint")) |>
      group_by(timepoint, gt, inc) |>
      filter(date == max(date)) |>
      ungroup()

    # Calculate mean CRPS by GT and INC
    rankings_cases <- scores_summary |>
      group_by(gt, inc, gen_time, inc_period) |>
      summarise(crps = mean(crps, na.rm = TRUE), .groups = "drop") |>
      mutate(rank = rank(crps))

    # Add scenario label
    rankings_cases$scenario <- scen_labels[scen]

    # Make factor levels for proper ordering
    rankings_cases <- rankings_cases |>
      mutate(
        inc_period = factor(inc_period, levels = c("no delay", "very low", "low", "correct", "high", "very high")),
        gen_time = factor(gen_time, levels = c("no delay", "very low", "low", "correct", "high", "very high"))
      )

    rankings_cases_all[[scen]] <- rankings_cases
    message(paste("  Done:", nrow(rankings_cases), "GT×INC combinations"))
  }

  if(length(rankings_cases_all) == 0) {
    message(paste("No results for", disease))
    next
  }

  # Combine all scenarios
  rankings_combined <- bind_rows(rankings_cases_all)

  # Create heatmap plot
  p_heatmap <- ggplot(rankings_combined, aes(x = gen_time, y = inc_period)) +
    geom_tile(aes(fill = crps)) +
    scale_fill_viridis_c(option = "plasma", name = "CRPS\n(log scale)") +
    facet_wrap(~scenario, ncol = 2) +
    xlab("Generation time misspecification") +
    ylab("Incubation period misspecification") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    ggtitle(paste0(toupper(disease), ": Forecast performance by delay misspecification"))

  # Save plot
  ggsave(
    here("results", paste0("fig1_", disease, "_crps_heatmap.png")),
    p_heatmap,
    width = 10,
    height = 8,
    dpi = 150
  )

  message(paste("Saved figure for", disease))

  # Also create a rank-based heatmap
  p_rank <- ggplot(rankings_combined, aes(x = gen_time, y = inc_period)) +
    geom_tile(aes(fill = rank)) +
    scale_fill_viridis_c(option = "viridis", name = "Rank\n(1 = best)", direction = -1) +
    facet_wrap(~scenario, ncol = 2) +
    xlab("Generation time misspecification") +
    ylab("Incubation period misspecification") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    ggtitle(paste0(toupper(disease), ": Forecast ranking by delay misspecification"))

  ggsave(
    here("results", paste0("fig1_", disease, "_rank_heatmap.png")),
    p_rank,
    width = 10,
    height = 8,
    dpi = 150
  )

  # Create bar charts showing marginal effects
  # By generation time
  marginal_gt <- rankings_combined |>
    group_by(gen_time, scenario) |>
    summarise(mean_crps = mean(crps, na.rm = TRUE), .groups = "drop")

  p_gt <- ggplot(marginal_gt, aes(x = gen_time, y = mean_crps)) +
    geom_col(fill = "steelblue") +
    facet_wrap(~scenario, scales = "free_y", ncol = 2) +
    xlab("Generation time misspecification") +
    ylab("Mean CRPS (log scale)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # By incubation period
  marginal_inc <- rankings_combined |>
    group_by(inc_period, scenario) |>
    summarise(mean_crps = mean(crps, na.rm = TRUE), .groups = "drop")

  p_inc <- ggplot(marginal_inc, aes(x = inc_period, y = mean_crps)) +
    geom_col(fill = "darkgreen") +
    facet_wrap(~scenario, scales = "free_y", ncol = 2) +
    xlab("Incubation period misspecification") +
    ylab("Mean CRPS (log scale)") +
    theme_classic() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))

  # Combine marginal plots
  p_marginal <- cowplot::plot_grid(p_gt, p_inc, ncol = 2, labels = c("A", "B"))

  ggsave(
    here("results", paste0("fig1_", disease, "_marginal.png")),
    p_marginal,
    width = 14,
    height = 8,
    dpi = 150
  )
}

message("\n=== Figure generation complete ===\n")
