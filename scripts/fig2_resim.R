####################################
#### Figure 2 - Resimulated data ###
####################################

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "generate_scores_func.R"))
source(here("R", "lshtm_theme.R"))
source(here("R", "plots_baseline.R"))

diseases <- c("covid", "ebola", "cholera")
disease_labels <- c(covid = "COVID-19", ebola = "Ebola", cholera = "Cholera")
rt_opts_list <- c("latest", "project")

for (rt_opts in rt_opts_list) {
  for (disease in diseases) {
    message(paste("=== Processing", disease, rt_opts, "==="))

    ## Data
    resim_data <- read_latest(here("data"), paste0(disease, "_sim_data"))

    ## Rt truth
    rt_truth <- readRDS(here("data", paste0("rt_", disease, ".rds")))
    if ("median" %in% names(rt_truth)) {
      rt_truth <- rt_truth |> select(date, median) |> rename(R = median)
    } else {
      rt_truth <- rt_truth |> select(date, R)
    }
    rt_truth <- rt_truth |> mutate(scen = 1)

    ## Forecasts
    resim_samples <- read_latest(here("results/sim"),
      paste0("res_", disease, "_resim_", rt_opts, "_all_samples"))
    resim_id <- read_latest(here("results/sim"),
      paste0("res_", disease, "_resim_", rt_opts, "_all_id"))

    startdate <- startenddates[[disease]]$startdate
    resim_samples <- resim_samples |> filter(date <= as.Date(startdate) + 6*4*7)
    resim_data <- resim_data |> filter(date <= as.Date(startdate) + 6*4*7)

    ## Score
    scores_cases <- generate_scores_cases(resim_samples, resim_id, resim_data) |>
      mutate(scen = 1, rt_opts = rt_opts, rt_traj = "Cases", ur = "y")

    ## Case plots
    caseplot <- plot_baseline_cases(resim_samples, resim_id, resim_data,
                                     scores_cases, forecast_freq = 4)

    label <- disease_labels[disease]
    assign(paste0("casetimeseries_", rt_opts, "_", disease),
      caseplot$timeseries + ggtitle(label) + ylab("Reported cases"))

    rm(caseplot, resim_samples, resim_id, resim_data, rt_truth, scores_cases); gc()
  }
}

## Combined timeseries: latest block then project block

# Section labels
label_latest <- ggplot() + annotate("text", x = 0.5, y = 0.5,
  label = "Latest Rt estimate", size = 7, fontface = "bold") + theme_void()
label_project <- ggplot() + annotate("text", x = 0.5, y = 0.5,
  label = "Projected Rt", size = 7, fontface = "bold") + theme_void()

timeseries_latest <- cowplot::plot_grid(
  casetimeseries_latest_covid, casetimeseries_latest_ebola, casetimeseries_latest_cholera,
  ncol = 1
)

timeseries_project <- cowplot::plot_grid(
  casetimeseries_project_covid, casetimeseries_project_ebola, casetimeseries_project_cholera,
  ncol = 1
)

# Performance legend
perf_legend <- cowplot::get_legend(
  ggplot(data.frame(x = 1:2, y = 1:2,
                    Performance = factor(c("Best-performing", "Worst-performing"),
                                         levels = c("Best-performing", "Worst-performing"))),
         aes(x = x, y = y, colour = Performance, fill = Performance)) +
    geom_line() +
    geom_ribbon(aes(ymin = y - 0.5, ymax = y + 0.5), alpha = 0.5) +
    scale_colour_manual(values = c("Best-performing" = "darkblue",
                                    "Worst-performing" = "darkred")) +
    scale_fill_manual(values = c("Best-performing" = "darkblue",
                                  "Worst-performing" = "darkred")) +
    theme(legend.position = "right",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
)

main_plot <- cowplot::plot_grid(
  label_latest,
  timeseries_latest,
  label_project,
  timeseries_project,
  ncol = 1, rel_heights = c(0.03, 1, 0.03, 1)
)

timeseries <- cowplot::plot_grid(
  main_plot, perf_legend,
  ncol = 2, rel_widths = c(1, 0.15)
)

ggsave(here("figures", "fig2_resim_combined_timeseries.png"),
       timeseries, width = 25, height = 18)

message("\n=== Figure 2 resim generation complete ===\n")
