library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "lshtm_theme.R"))

startdate <- as.Date("2017-04-23") # Same start date as data
enddate <- as.Date("2017-04-23") + 6*4*7 + 14  # Long enough time horizon to have six forecast timepoints with forecasts every 4 weeks

## Loading all data - collect Rt trajectories and simulated data in a list ##

rt_traj <- read_latest(here("data"), "rt_traj_list_cholera")
sim_data <- read_latest(here("data"), "sim_data_list_cholera")

for(i in 1:20){
  
  ## Loading data ##
  rt_traj_scen <- rt_traj[[i]]
  sim_data_scen <- sim_data[[i]]
  
  ## Loading results ##
  cholera_samples <- read_latest(here("results/cholera"), paste0("res_cholerascen", i, "_all_samples"))
  #cholera_R <- read_latest(here("results/cholera"), paste0("res_cholerascen", i, "_all_R"))
  cholera_id <- read_latest(here("results/cholera"), paste0("res_cholerascen", i, "_all_id"))
  cholera_warnings <- read_latest(here("results/cholera"), paste0("res_cholerascen", i, "_all_warnings"))
  
  ## Rt plot - CRPS by timepoint
 # cholera_rt_tp <- plotrankrt_tp(cholera_R,
#                    cholera_id,
#                    rt_traj_scen,
#                    4)
  
  ## Cases plot - CRPS by timepoint
  cholera_cases_tp <- plotrankcase_tp(cholera_samples,
                                  cholera_id,
                                  sim_data_scen,
                                  4)
  
 # ggsave(paste0("results/plotcholerascen", i, "_rt_tp.png"), cholera_rt_tp, width=32, height=17, units="cm")
  ggsave(paste0("results/plotcholerascen", i, "_cases_tp.png"), cholera_cases_tp, width=34, height=17, units="cm")
}




















