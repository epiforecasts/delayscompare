####################################
#### Figure 1 - Resimulated data ###
####################################

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "generate_scores_func.R"))
source(here("R", "lshtm_theme.R"))
source(here("R", "plot_casestudy.R"))
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
    resim_R <- read_latest(here("results/sim"),
      paste0("res_", disease, "_resim_", rt_opts, "_all_R"))

    startdate <- startenddates[[disease]]$startdate
    resim_samples <- resim_samples |> filter(date <= as.Date(startdate) + 6*4*7)
    resim_R <- resim_R |> filter(date <= as.Date(startdate) + 6*4*7)

    ## Score
    scores_cases <- generate_scores_cases(resim_samples, resim_id, resim_data) |>
      mutate(scen = 1, rt_opts = rt_opts, rt_traj = "Cases", ur = "y")
    scores_rt <- generate_scores_rt(resim_R, resim_id, rt_truth) |>
      mutate(scen = 1, rt_opts = rt_opts, rt_traj = "Rt", ur = "y")

    ## Rt plots
    rtplot <- plot_baseline_rt(resim_R, resim_id, rt_truth,
                                scores_rt, forecast_freq = 4)

    ## Case plots
    caseplot <- plot_baseline_cases(resim_samples, resim_id, resim_data,
                                     scores_cases, forecast_freq = 4)

    ## Extract sub-plots
    label <- disease_labels[disease]
    title_theme <- theme(plot.title = element_text(hjust = 0.5, size = 20))
    measure_colours <- scale_fill_manual(values = lshtm_pal, name = "Components of CRPS")

    assign(paste0("rt_barchartgentime_", rt_opts, "_", disease),
      rtplot$barchart_mean_gen_time + ggtitle(label) + title_theme + measure_colours)
    assign(paste0("rt_barchartincperiod_", rt_opts, "_", disease),
      rtplot$barchart_mean_inc_period + ggtitle(label) + title_theme + measure_colours)
    assign(paste0("case_barchartgentime_", rt_opts, "_", disease),
      caseplot$barchart_mean_gen_time + ggtitle(label) + title_theme + measure_colours)
    assign(paste0("case_barchartincperiod_", rt_opts, "_", disease),
      caseplot$barchart_mean_inc_period + ggtitle(label) + title_theme + measure_colours)

    rm(rtplot, caseplot, resim_samples, resim_R, resim_id, resim_data,
       rt_truth, scores_cases, scores_rt); gc()
  }
}

#########################################
#### Combined barchart (all diseases) ###
#########################################

no_title <- theme(plot.title = element_blank())

# Build a grid for each rt_opts
build_barchart_grid <- function(rt_opts_val, panel_start = 1) {
  panel_labels <- LETTERS[panel_start:(panel_start + 11)]
  panel_idx <- 1

  plots <- list()
  for (disease in diseases) {
    for (prefix in c("rt_barchartgentime_", "case_barchartgentime_",
                      "rt_barchartincperiod_", "case_barchartincperiod_")) {
      name <- paste0(prefix, rt_opts_val, "_", disease)
      plots[[panel_idx]] <- get(name) + no_title + labs(tag = panel_labels[panel_idx])
      panel_idx <- panel_idx + 1
    }
  }

  cowplot::plot_grid(plotlist = plots, ncol = 4)
}

grid_latest <- build_barchart_grid("latest", panel_start = 1)
grid_project <- build_barchart_grid("project", panel_start = 13)

# Row labels (disease names) on the left — same for both grids
make_row_labels <- function() {
  cowplot::plot_grid(
    ggplot() + annotate("text", x = 0.5, y = 0.5, label = "COVID-19",
                         size = 6) + theme_void(),
    ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Ebola",
                         size = 6) + theme_void(),
    ggplot() + annotate("text", x = 0.5, y = 0.5, label = "Cholera",
                         size = 6) + theme_void(),
    ncol = 1
  )
}

body_latest <- cowplot::plot_grid(
  make_row_labels(), grid_latest,
  ncol = 2, rel_widths = c(0.12, 1)
)

body_project <- cowplot::plot_grid(
  make_row_labels(), grid_project,
  ncol = 2, rel_widths = c(0.12, 1)
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
  ncol = 2, rel_widths = c(0.12, 1)
)

# Section labels
label_latest <- ggplot() + annotate("text", x = 0.5, y = 0.5,
  label = "Latest Rt estimate", size = 7, fontface = "bold") + theme_void()
label_project <- ggplot() + annotate("text", x = 0.5, y = 0.5,
  label = "Projected Rt", size = 7, fontface = "bold") + theme_void()

# Extract shared legend
legend_data <- data.frame(
  x = rep("a", 3), y = c(1, 1, 1),
  measure = factor(c("dispersion", "overprediction", "underprediction"),
                   levels = c("dispersion", "overprediction", "underprediction"))
)

legend <- cowplot::get_legend(
  ggplot(legend_data, aes(x = x, y = y, fill = measure)) +
    geom_bar(stat = "identity") +
    scale_fill_manual(values = lshtm_pal, name = "Components of CRPS") +
    theme(legend.position = "right",
          legend.text = element_text(size = 12),
          legend.title = element_text(size = 14))
)

# Combine: headers + latest block + project block
main_plot <- cowplot::plot_grid(
  top_headers_padded,
  label_latest,
  body_latest,
  label_project,
  body_project,
  ncol = 1, rel_heights = c(0.03, 0.03, 1, 0.03, 1)
)

combined_barcharts <- cowplot::plot_grid(
  main_plot, legend,
  ncol = 2, rel_widths = c(1, 0.15)
)

ggsave(here("figures", "fig1_resim_combined_barcharts.png"),
       combined_barcharts, width = 14, height = 18)

message("\n=== Figure 1 resim generation complete ===\n")
