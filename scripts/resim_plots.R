library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "generate_scores_func.R"))
source(here("R", "plots_baseline.R"))
source(here("R", "lshtm_theme.R"))

###############################################################################################
###### Figure 1: CRPS across gen time and inc period assumptions - Rt and case forecasts ######
###############################################################################################

args <- commandArgs(trailingOnly = TRUE)
diseases <- if (length(args) > 0) args else c("covid", "ebola", "cholera")

for (disease in diseases) {
  message(paste("=== Processing", disease, "==="))

  ## Data
  resim_data <- read_latest(here("data"), paste0(disease, "_sim_data"))

  ## Forecasts
  resim_samples <- read_latest(here("results/sim"), paste0("res_", disease, "_resim_latest_all_samples"))
  resim_id <- read_latest(here("results/sim"), paste0("res_", disease, "_resim_latest_all_id"))
  resim_R <- read_latest(here("results/sim"), paste0("res_", disease, "_resim_latest_all_R"))

  ## Rt truth
  rt_truth <- readRDS(here("data", paste0("rt_", disease, ".rds")))
  if ("median" %in% names(rt_truth)) {
    rt_truth <- rt_truth |> select(date, median) |> rename(R = median)
  } else {
    rt_truth <- rt_truth |> select(date, R)
  }
  rt_truth <- rt_truth |> mutate(scen = 1)

  ## Score
  scores_cases <- generate_scores_cases(resim_samples, resim_id, resim_data) |>
    mutate(scen = 1, rt_opts = "latest", rt_traj = "resim", ur = "y")
  scores_rt <- generate_scores_rt(resim_R, resim_id, rt_truth) |>
    mutate(scen = 1, rt_opts = "latest", rt_traj = "resim", ur = "y")

  ## Case plots
  caseplots <- plot_baseline_cases(resim_samples, resim_id, resim_data,
                                   scores_cases, forecast_freq = 4)
  ggsave(here("results", paste0("fig_resim_", disease, "_cases.png")),
         caseplots$final_plot, width = 13.5, height = 8.5)

  ## Rt plots
  rtplots <- plot_baseline_rt(resim_R, resim_id, rt_truth,
                              scores_rt, forecast_freq = 4)
  ggsave(here("results", paste0("fig_resim_", disease, "_rt.png")),
         rtplots$final_plot, width = 13.5, height = 8.5)

  message(paste("Saved figures for", disease))
}
