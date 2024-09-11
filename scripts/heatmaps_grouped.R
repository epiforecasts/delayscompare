library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "generate_scores_func.R"))
source(here("R", "lshtm_theme.R"))

startdate <- as.Date("2017-04-23") # Same start date as data
enddate <- as.Date("2017-04-23") + 6*4*7 + 14  # Long enough time horizon to have six forecast timepoints with forecasts every 4 weeks

## Loading all data - collect Rt trajectories and simulated data in a list ##

rt_traj <- read_latest(here("data"), "rt_traj_list_cholera")
sim_data <- read_latest(here("data"), "sim_data_list_cholera")

# Scenario labels
scen_labs <- data.frame(scen=c(1:16),
                        rt_traj=c(rep("const_low", 4), rep("const_high", 4), rep("inc", 4), rep("dec", 4)),
                        rt_opts=rep(c("latest", "latest", "project", "project"), 4),
                        ur=rep(c("No under-reporting", "Under-reporting", "No under-reporting", "Under-reporting"), 4))

rt_traj_scen <- list()
sim_data_scen <- list()
scores_cases <- list()
scores_rt <- list()

for(i in 1:16){
  
  ## Loading data ##
  rt_traj_scen[[i]] <- rt_traj[[i]] |> 
    as.data.frame() |> 
    mutate(scen=i)
  
  sim_data_scen[[i]] <- sim_data[[i]] |> 
    as.data.frame()|> 
    mutate(scen=i)
  
  ## Loading results & generating scores as we go in order to save memory ##
  cholera_samples <- read_latest(here("results/cholera"), paste0("res_cholerascen", i, "_all_samples"))
  cholera_R <- read_latest(here("results/cholera"), paste0("res_cholerascen", i, "_all_R"))
  cholera_id <- read_latest(here("results/cholera"), paste0("res_cholerascen", i, "_all_id")) 
  
  scores_cases[[i]] <- generate_scores_cases(cholera_samples, cholera_id, sim_data_scen[[i]]) |> mutate(scen=i)
  scores_rt[[i]] <- generate_scores_rt(cholera_R, cholera_id, rt_traj_scen[[i]]) |> mutate(scen=i)
  
}

rt_traj_scen <- bind_rows(rt_traj_scen)
sim_data_scen <- bind_rows(sim_data_scen)
scores_cases <- bind_rows(scores_cases)
scores_rt <- bind_rows(scores_rt)

# Add scenario labels
rt_traj_scen <- rt_traj_scen |> 
  left_join(scen_labs, by="scen")

sim_data_scen <- sim_data_scen |>
  left_join(scen_labs, by="scen")

scores_cases <- scores_cases |>
  left_join(scen_labs, by="scen")

scores_rt <- scores_rt |>
  left_join(scen_labs, by="scen")

heatmaps_cases <- plot_heatmaps(scores_cases)
heatmaps_rt <- plot_heatmaps(scores_rt)

box_overpredict_cases <- plot_boxplots(scores_cases)
box_overpredict_rt <- plot_boxplots(scores_rt)

ggsave(paste0("results/plotcholera_cases_overprediction_boxplot.png"), box_overpredict_cases, width=33, height=15, units="cm")
ggsave(paste0("results/plotcholera_rt_overprediction_boxplot.png"), box_overpredict_rt, width=33, height=15, units="cm")

box_underpredict_cases <- plot_boxplots(scores_cases)
box_underpredict_rt <- plot_boxplots(scores_rt)

ggsave(paste0("results/plotcholera_cases_underprediction_boxplot.png"), box_underpredict_cases, width=33, height=15, units="cm")
ggsave(paste0("results/plotcholera_rt_underprediction_boxplot.png"), box_underpredict_rt, width=33, height=15, units="cm")

box_crps_cases <- plot_boxplots(scores_cases)
box_crps_rt <- plot_boxplots(scores_rt)

ggsave(paste0("results/plotcholera_cases_crps_boxplot.png"), box_crps_cases, width=33, height=15, units="cm")
ggsave(paste0("results/plotcholera_rt_crps_boxplot.png"), box_crps_rt, width=33, height=15, units="cm")

box_disp_cases <- plot_boxplots(scores_cases)
box_disp_rt <- plot_boxplots(scores_rt)

ggsave(paste0("results/plotcholera_cases_disp_boxplot.png"), box_disp_cases, width=33, height=15, units="cm")
ggsave(paste0("results/plotcholera_rt_disp_boxplot.png"), box_disp_rt, width=33, height=15, units="cm")
