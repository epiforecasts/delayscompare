##########################
#### Casestudy Figure ####
##########################

# Generates figures for real data (casestudy) results
# Shows forecast performance by GT x INC misspecification on actual outbreak data

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "funcs_scoring.R"))
source(here("R", "lshtm_theme.R"))

rt_opts <- "latest"

# Load casestudy real data for true values
source(here("scripts", "datacollect_casestudy.R"))

# Disease display names
disease_names <- c(
  "ebola" = "Ebola (Guinea 2014)",
  "covid" = "COVID-19 (England 2021)",
  "cholera" = "Cholera (Yemen 2017-18)"
)

# Store all disease rankings for combined plot
all_disease_rankings <- list()

# Process each disease
for(disease in c("ebola", "covid", "cholera")) {

  message(paste("\n=== Processing", disease, "casestudy ===\n"))

  # Load processed casestudy results
  res_samples <- tryCatch(
    read_latest(here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_samples")),
    error = function(e) { message(paste("  Missing samples for", disease)); NULL }
  )
  res_id <- tryCatch(
    read_latest(here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_id")),
    error = function(e) { message(paste("  Missing id for", disease)); NULL }
  )

  if(is.null(res_samples) || is.null(res_id)) {
    message(paste("Skipping", disease, "- missing data"))
    next
  }

  # Load and apply diagnostics filter
  diagnostics <- tryCatch(
    read_latest(here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_diagnostics")),
    error = function(e) { message(paste("  No diagnostics for", disease)); NULL }
  )
  if(!is.null(diagnostics)) {
    res_samples <- filter_by_diagnostics(res_samples, diagnostics)
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

  # Score forecasts
  scores_cases <- score_case_forecasts(res_samples, res_id, true_cases)
  if(is.null(scores_cases)) {
    message(paste("  No scores for", disease))
    next
  }

  # Summarise into rankings
  rankings_cases <- summarise_rankings(scores_cases) |>
    mutate(
      inc_period = factor(inc_period, levels = level_order),
      gen_time = factor(gen_time, levels = level_order),
      disease_label = disease_names[disease]
    )

  message(paste("  Done:", nrow(rankings_cases), "GT x INC combinations"))

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
    ggtitle(disease_names[disease])

  ggsave(
    here("results", paste0("fig_casestudy_", disease, "_crps_heatmap.png")),
    p_heatmap,
    width = 8,
    height = 6,
    dpi = 150
  )

  # Rank heatmap
  p_rank <- ggplot(rankings_cases, aes(x = gen_time, y = inc_period)) +
    geom_tile(aes(fill = rank)) +
    scale_fill_viridis_c(option = "viridis", name = "Rank\n(1 = best)", direction = -1) +
    xlab("Generation time misspecification") +
    ylab("Incubation period misspecification") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold")
    ) +
    ggtitle(disease_names[disease])

  ggsave(
    here("results", paste0("fig_casestudy_", disease, "_rank_heatmap.png")),
    p_rank,
    width = 8,
    height = 6,
    dpi = 150
  )

  message(paste("Saved figures for", disease))
}

# Create combined plot if we have all diseases
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
    ggtitle("Casestudy: Forecast performance on real outbreak data")

  ggsave(
    here("results", "fig_casestudy_combined_crps.png"),
    p_combined_crps,
    width = 14,
    height = 5,
    dpi = 150
  )

  # Combined rank heatmap
  p_combined_rank <- ggplot(rankings_combined, aes(x = gen_time, y = inc_period)) +
    geom_tile(aes(fill = rank)) +
    scale_fill_viridis_c(option = "viridis", name = "Rank\n(1 = best)", direction = -1) +
    facet_wrap(~disease_label, ncol = 3, scales = "free") +
    xlab("Generation time misspecification") +
    ylab("Incubation period misspecification") +
    theme_classic() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      strip.background = element_blank(),
      strip.text = element_text(face = "bold", size = 10)
    ) +
    ggtitle("Casestudy: Forecast ranking on real outbreak data")

  ggsave(
    here("results", "fig_casestudy_combined_rank.png"),
    p_combined_rank,
    width = 14,
    height = 5,
    dpi = 150
  )

  # Marginal effects - by generation time
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

  # Marginal effects - by incubation period
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
    here("results", "fig_casestudy_marginal.png"),
    p_marginal,
    width = 12,
    height = 10,
    dpi = 150
  )

  message("\n=== Casestudy figure generation complete ===\n")
}
