####################################
#### Figure 1 - Resimulated data ###
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

for (disease in diseases) {
  message(paste("=== Processing", disease, "==="))

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
    paste0("res_", disease, "_resim_latest_all_samples"))
  resim_id <- read_latest(here("results/sim"),
    paste0("res_", disease, "_resim_latest_all_id"))
  resim_R <- read_latest(here("results/sim"),
    paste0("res_", disease, "_resim_latest_all_R"))

  startdate <- startenddates[[disease]]$startdate
  resim_samples <- resim_samples |> filter(date <= as.Date(startdate) + 6*4*7)
  resim_R <- resim_R |> filter(date <= as.Date(startdate) + 6*4*7)

  ## Score
  scores_cases <- generate_scores_cases(resim_samples, resim_id, resim_data) |>
    mutate(scen = 1, rt_opts = "latest", rt_traj = disease, ur = "y")
  scores_rt <- generate_scores_rt(resim_R, resim_id, rt_truth) |>
    mutate(scen = 1, rt_opts = "latest", rt_traj = disease, ur = "y")

  ## Rt plots
  rtplot <- plot_baseline_rt(resim_R, resim_id, rt_truth,
                              scores_rt, forecast_freq = 4)

  ## Case plots
  caseplot <- plot_baseline_cases(resim_samples, resim_id, resim_data,
                                   scores_cases, forecast_freq = 4)

  ## Extract sub-plots for combined figure
  label <- disease_labels[disease]
  title_theme <- theme(plot.title = element_text(hjust = 0.5, size = 20))

  assign(paste0("rt_barchartgentime_", disease),
    rtplot$barchart_mean_gen_time + ggtitle(label) + title_theme)
  assign(paste0("rt_barchartincperiod_", disease),
    rtplot$barchart_mean_inc_period + ggtitle(label) + title_theme)
  assign(paste0("case_barchartgentime_", disease),
    caseplot$barchart_mean_gen_time + ggtitle(label) + title_theme)
  assign(paste0("case_barchartincperiod_", disease),
    caseplot$barchart_mean_inc_period + ggtitle(label) + title_theme)
  assign(paste0("casetimeseries_", disease),
    caseplot$timeseries + ggtitle(label))

  rm(rtplot, caseplot, resim_samples, resim_R, resim_id, resim_data,
     rt_truth, scores_cases, scores_rt); gc()
}

#########################################
#### Combined barchart (all diseases) ###
#########################################

# Remove individual titles from sub-plots (row labels added separately)
no_title <- theme(plot.title = element_blank())

for (disease in diseases) {
  for (prefix in c("rt_barchartgentime_", "case_barchartgentime_",
                    "rt_barchartincperiod_", "case_barchartincperiod_")) {
    obj <- get(paste0(prefix, disease)) + no_title
    assign(paste0(prefix, disease), obj)
  }
}

# Build the plot grid (3 rows x 4 cols)
plot_grid_body <- cowplot::plot_grid(
  rt_barchartgentime_covid, case_barchartgentime_covid,
  rt_barchartincperiod_covid, case_barchartincperiod_covid,
  rt_barchartgentime_ebola, case_barchartgentime_ebola,
  rt_barchartincperiod_ebola, case_barchartincperiod_ebola,
  rt_barchartgentime_cholera, case_barchartgentime_cholera,
  rt_barchartincperiod_cholera, case_barchartincperiod_cholera,
  ncol = 4
)

# Row labels (disease names) on the left
row_labels <- cowplot::plot_grid(
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "COVID-19",
                       size = 6, fontface = "bold") + theme_void(),
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Ebola",
                       size = 6, fontface = "bold") + theme_void(),
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Cholera",
                       size = 6, fontface = "bold") + theme_void(),
  ncol = 1
)

body_with_rows <- cowplot::plot_grid(
  row_labels, plot_grid_body,
  ncol = 2, rel_widths = c(0.08, 1)
)

# Sub-headers: Rt / Cases
sub_headers <- cowplot::plot_grid(
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Rt", size = 5) + theme_void(),
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Cases", size = 5) + theme_void(),
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Rt", size = 5) + theme_void(),
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Cases", size = 5) + theme_void(),
  ncol = 4
)

sub_headers_padded <- cowplot::plot_grid(
  ggplot() + theme_void(), sub_headers,
  ncol = 2, rel_widths = c(0.08, 1)
)

# Top-level headers: Generation time / Incubation period
top_headers <- cowplot::plot_grid(
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Generation time",
                       size = 6, fontface = "bold") + theme_void(),
  ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Incubation period",
                       size = 6, fontface = "bold") + theme_void(),
  ncol = 2
)

top_headers_padded <- cowplot::plot_grid(
  ggplot() + theme_void(), top_headers,
  ncol = 2, rel_widths = c(0.08, 1)
)

# Extract shared legend
legend_data <- data.frame(
  x = rep("a", 3), y = c(1, 1, 1),
  measure = factor(c("dispersion", "overprediction", "underprediction"),
                   levels = c("dispersion", "overprediction", "underprediction"))
)

legend <- cowplot::get_legend(
  ggplot(legend_data, aes(x = x, y = y, fill = measure)) +
    geom_bar(stat = "identity") +
    scale_fill_discrete() +
    theme(legend.position = "right",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
)

# Combine: headers + body + legend
main_plot <- cowplot::plot_grid(
  top_headers_padded,
  sub_headers_padded,
  body_with_rows,
  ncol = 1, rel_heights = c(0.04, 0.03, 1)
)

combined_barcharts <- cowplot::plot_grid(
  main_plot, legend,
  ncol = 2, rel_widths = c(1, 0.1)
)

ggsave(here("figures", "fig1_resim_combined_barcharts.png"),
       combined_barcharts, width = 16, height = 10)

## Combined timeseries
timeseries <- cowplot::plot_grid(
  casetimeseries_covid, casetimeseries_ebola, casetimeseries_cholera,
  ncol = 1
)

ggsave(here("figures", "fig1_resim_combined_timeseries.png"),
       timeseries, width = 13.5, height = 10)

message("\n=== Resim figure generation complete ===\n")
