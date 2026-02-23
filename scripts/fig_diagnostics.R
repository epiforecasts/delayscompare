################################
#### Diagnostics Figure     ####
################################

# Creates diagnostics overview figures for supplementary material:
# - % of fits converging by disease x scenario
# - Distribution of Rhat, divergent transitions, ESS

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "funcs_scoring.R"))
source(here("R", "lshtm_theme.R"))

rt_opts <- "latest"

# Scenario labels
scen_labels <- c(
  "const_low" = "Constant Rt (low)",
  "const_high" = "Constant Rt (high)",
  "inc" = "Increasing Rt",
  "dec" = "Decreasing Rt",
  "casestudy" = "Case study"
)

# Disease display names
disease_names <- c(
  "ebola" = "Ebola",
  "covid" = "COVID-19",
  "cholera" = "Cholera"
)

# Collect all diagnostics
all_diagnostics <- list()

for(disease in c("ebola", "covid", "cholera")) {

  # Simulated scenarios
  for(scen in c("const_low", "const_high", "inc", "dec")) {
    diag <- tryCatch(
      read_latest(here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_diagnostics")),
      error = function(e) NULL
    )
    if(!is.null(diag)) {
      diag$disease <- disease_names[disease]
      diag$scenario <- scen_labels[scen]
      diag$analysis <- "Simulation"
      all_diagnostics[[paste(disease, scen)]] <- diag
    }
  }

  # Case study
  diag <- tryCatch(
    read_latest(here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_diagnostics")),
    error = function(e) NULL
  )
  if(!is.null(diag)) {
    diag$disease <- disease_names[disease]
    diag$scenario <- "Case study"
    diag$analysis <- "Case study"
    all_diagnostics[[paste(disease, "casestudy")]] <- diag
  }

  # Weight prior
  for(vary in c("gt", "inc", "both")) {
    for(wp in c("TRUE", "FALSE")) {
      diag <- tryCatch(
        read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_", wp, "_", vary, "_", rt_opts, "_all_diagnostics")),
        error = function(e) NULL
      )
      if(!is.null(diag)) {
        diag$disease <- disease_names[disease]
        diag$scenario <- paste0("vary = ", vary, ", wp = ", wp)
        diag$analysis <- "Weight prior"
        all_diagnostics[[paste(disease, "wp", vary, wp)]] <- diag
      }
    }
  }
}

if(length(all_diagnostics) == 0) {
  stop("No diagnostics files found!")
}

diagnostics <- bind_rows(all_diagnostics)
if(all(c("gt", "inc") %in% names(diagnostics))) {
  diagnostics <- diagnostics |> add_delay_labels()
} else if(any(c("gt", "inc") %in% names(diagnostics))) {
  # Some rows have gt/inc (sim/casestudy), some don't (weightprior)
  diagnostics <- diagnostics |>
    mutate(
      gen_time = if("gt" %in% names(diagnostics))
        factor(level_labels[as.character(gt)], levels = level_order) else NA,
      inc_period = if("inc" %in% names(diagnostics))
        factor(level_labels[as.character(inc)], levels = level_order) else NA
    )
}

message(paste("Loaded", nrow(diagnostics), "diagnostic records"))

rhat_threshold <- 1.05

#### Panel A: Convergence rates by disease x scenario ####

convergence <- diagnostics |>
  filter(!is.na(max_rhat)) |>
  mutate(converged = max_rhat <= rhat_threshold) |>
  group_by(disease, scenario, analysis) |>
  summarise(
    n_total = n(),
    n_converged = sum(converged),
    pct_converged = 100 * mean(converged),
    .groups = "drop"
  )

print("Convergence summary:")
print(convergence, n = 50)

p_convergence <- ggplot(convergence, aes(x = scenario, y = pct_converged, fill = disease)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_text(aes(label = paste0(round(pct_converged), "%")),
            position = position_dodge(width = 0.8), vjust = -0.3, size = 2.5) +
  scale_fill_brewer(palette = "Set2", name = "Disease") +
  xlab("") +
  ylab(paste0("% converged (Rhat < ", rhat_threshold, ")")) +
  ylim(0, 105) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#### Panel B: Rhat distribution ####

p_rhat <- ggplot(diagnostics |> filter(!is.na(max_rhat)),
                 aes(x = disease, y = max_rhat, fill = disease)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
  geom_hline(yintercept = rhat_threshold, linetype = "dashed", colour = "red") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  facet_wrap(~scenario, ncol = 4) +
  xlab("") +
  ylab("Max Rhat") +
  coord_cartesian(ylim = c(0.99, min(max(diagnostics$max_rhat, na.rm = TRUE), 3))) +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

#### Panel C: Divergent transitions ####

p_divergent <- ggplot(diagnostics |> filter(!is.na(num_divergent)),
                      aes(x = disease, y = num_divergent + 1, fill = disease)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  scale_y_log10(name = "Divergent transitions + 1") +
  facet_wrap(~scenario, ncol = 4) +
  xlab("") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

#### Panel D: Minimum ESS ####

p_ess <- ggplot(diagnostics |> filter(!is.na(min_ess), is.finite(min_ess)),
                aes(x = disease, y = min_ess, fill = disease)) +
  geom_violin(draw_quantiles = c(0.25, 0.5, 0.75), scale = "width") +
  scale_fill_brewer(palette = "Set2", guide = "none") +
  facet_wrap(~scenario, ncol = 4) +
  xlab("") +
  ylab("Min effective sample size") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  )

#### Combine and save ####

p_combined <- cowplot::plot_grid(
  p_convergence, p_rhat, p_divergent, p_ess,
  ncol = 1,
  labels = c("A", "B", "C", "D"),
  rel_heights = c(1, 1.2, 1.2, 1.2)
)

ggsave(
  here("results", "fig_diagnostics_overview.png"),
  p_combined,
  width = 14,
  height = 18,
  dpi = 150
)

#### Convergence by GT x INC heatmap (simulation scenarios only) ####

convergence_gtinc <- diagnostics |>
  filter(analysis == "Simulation", !is.na(max_rhat)) |>
  mutate(converged = max_rhat <= rhat_threshold) |>
  group_by(disease, gen_time, inc_period) |>
  summarise(
    pct_converged = 100 * mean(converged),
    .groups = "drop"
  )

p_conv_heatmap <- ggplot(convergence_gtinc, aes(x = gen_time, y = inc_period)) +
  geom_tile(aes(fill = pct_converged)) +
  geom_text(aes(label = paste0(round(pct_converged), "%")), size = 2.5) +
  scale_fill_viridis_c(option = "mako", name = "% converged", limits = c(0, 100)) +
  facet_wrap(~disease, ncol = 3) +
  xlab("Generation time misspecification") +
  ylab("Incubation period misspecification") +
  theme_classic() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.background = element_blank(),
    strip.text = element_text(face = "bold")
  ) +
  ggtitle("Convergence rate by delay misspecification (simulations, all scenarios)")

ggsave(
  here("results", "fig_diagnostics_convergence_heatmap.png"),
  p_conv_heatmap,
  width = 14,
  height = 5,
  dpi = 150
)

message("\n=== Diagnostics figure generation complete ===\n")
