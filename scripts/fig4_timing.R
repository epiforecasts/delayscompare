####################################
#### Figure 4 - Runtime heatmaps ###
####################################

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "lshtm_theme.R"))

diseases <- c("covid", "ebola", "cholera")
disease_labels <- c(covid = "COVID-19", ebola = "Ebola", cholera = "Cholera")
rt_opts_list <- c("latest", "project")

gt_labels <- c("1" = "no delay", "2" = "very low", "3" = "low",
               "4" = "correct", "5" = "high", "6" = "very high")
inc_labels <- c("1" = "no delay", "2" = "very low", "3" = "low",
                "4" = "correct", "5" = "high", "6" = "very high")

all_timing <- list()

for (rt_opts in rt_opts_list) {
  for (disease in diseases) {
    for (gt in 1:6) {
      for (inc_val in 1:6) {
        tryCatch({
          timing <- read_latest(here("results"),
            paste0("res_", disease, "_resim_", rt_opts, "_timing", gt, inc_val))
          timing$gt <- gt
          timing$inc <- inc_val
          timing$disease <- disease
          timing$rt_opts <- rt_opts
          all_timing[[paste0(rt_opts, "_", disease, "_", gt, inc_val)]] <- timing
        }, error = function(e) {
          message(paste("Missing timing:", disease, rt_opts, "gt=", gt, "inc=", inc_val))
        })
      }
    }
  }
}

timing_df <- bind_rows(all_timing)

# Compute total elapsed time per gt/inc combination (summed across timepoints)
timing_summary <- timing_df |>
  group_by(disease, rt_opts, gt, inc) |>
  summarise(total_seconds = sum(elapsed_seconds), .groups = "drop") |>
  mutate(total_minutes = total_seconds / 60) |>
  group_by(disease, rt_opts) |>
  mutate(median_minutes = median(total_minutes, na.rm = TRUE),
         relative_time = total_minutes / median_minutes) |>
  ungroup() |>
  mutate(gt_label = factor(gt_labels[as.character(gt)],
                           levels = c("no delay", "very low", "low",
                                      "correct", "high", "very high")),
         inc_label = factor(inc_labels[as.character(inc)],
                            levels = c("no delay", "very low", "low",
                                       "correct", "high", "very high")),
         disease = factor(disease_labels[disease],
                          levels = c("COVID-19", "Ebola", "Cholera")),
         rt_opts = factor(rt_opts, levels = c("latest", "project"),
                          labels = c("Latest Rt estimate", "Projected Rt")))

# Heatmap
heatmap_plot <- ggplot(timing_summary,
                       aes(x = gt_label, y = inc_label, fill = relative_time)) +
  geom_tile(colour = "white", linewidth = 0.5) +
  geom_text(aes(label = round(relative_time, 1)), size = 3) +
  facet_grid(disease ~ rt_opts) +
  scale_fill_viridis_c(name = "Relative runtime\n(× median)", option = "plasma", trans = "log10") +
  xlab("Generation time") +
  ylab("Incubation period") +
  lshtm_theme() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(size = 12))

ggsave(here("figures", "fig4_timing_heatmap.png"),
       heatmap_plot, width = 12, height = 14)

message("\n=== Figure 4 timing heatmap complete ===\n")
