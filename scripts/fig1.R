##################
#### Figure 1 ####
##################

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "funcs_scoring.R"))
source(here("R", "lshtm_theme.R"))

rt_opts <- "latest"

# Scenario labels for plotting
scen_labels <- c(
  "const_low" = "Constant Rt (low)",
  "const_high" = "Constant Rt (high)",
  "inc" = "Increasing Rt",
  "dec" = "Decreasing Rt"
)

# Sim data file suffix for each scenario
sim_data_suffix <- c(
  "const_low" = "const_low",
  "const_high" = "const_hi",
  "inc" = "inc",
  "dec" = "dec"
)

# Process each disease
for(disease in c("ebola", "covid", "cholera")) {

  message(paste("\n=== Processing", disease, "===\n"))

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

    # Load and apply diagnostics filter
    diagnostics <- tryCatch(
      read_latest(here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_diagnostics")),
      error = function(e) { message(paste("  No diagnostics for", scen)); NULL }
    )
    if(!is.null(diagnostics)) {
      res_samples <- filter_by_diagnostics(res_samples, diagnostics)
    }

    # Load appropriate sim data for true values
    sim_data <- tryCatch(
      read_latest(here("data"), paste0(disease, "_sim_data_", sim_data_suffix[scen])),
      error = function(e) {
        message(paste("  No sim data for", scen, "- trying generic"))
        tryCatch(
          read_latest(here("data"), paste0(disease, "_sim_data")),
          error = function(e2) NULL
        )
      }
    )

    if(is.null(sim_data)) {
      message(paste("  No sim data for", scen))
      next
    }

    # Get true values (reported cases)
    true_cases <- sim_data |>
      filter(variable == "reported_cases") |>
      rename(true_value = value) |>
      select(date, true_value)

    # Score forecasts
    scores_cases <- score_case_forecasts(res_samples, res_id, true_cases)
    if(is.null(scores_cases)) {
      message(paste("  No scores for", scen))
      next
    }

    # Summarise into rankings
    rankings_cases <- summarise_rankings(scores_cases)

    # Add scenario label and factor levels
    rankings_cases <- rankings_cases |>
      mutate(
        scenario = scen_labels[scen],
        inc_period = factor(inc_period, levels = level_order),
        gen_time = factor(gen_time, levels = level_order)
      )

    rankings_cases_all[[scen]] <- rankings_cases
    message(paste("  Done:", nrow(rankings_cases), "GT x INC combinations"))
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
