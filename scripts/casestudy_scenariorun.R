library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

## Load data ##

case_data <- casestudydata[[disease]]
d <- delays[[disease]]

############### SCENARIOS #################

## Run scenario 21 - rt_opts=latest, under-reporting=no ##

res_disease <- sim_scenarios(case_data=case_data,
                           gt,
                           gen_mean=d$gen[["mean"]],
                           gen_sd=d$gen[["sd"]],
                           gen_max = d$gen[["max"]],
                           inc_mean = d$inc[["mean"]],
                           inc_sd = d$inc[["sd"]],
                           inc_max = d$inc[["max"]],
                           rep_mean = d$rep[["mean"]],
                           rep_sd = d$rep[["sd"]],
                           rep_max = d$rep[["max"]],
                           freq_fc=freq_fc,
                           weeks_inc=weeks_inc,
                           rt_opts_choice="latest",
                           obs_scale=1)

save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen21_id", gt))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen21_warnings", gt))

## Saving samples only ##
save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen21_samples", gt))
save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen21_R", gt)) 

## Run scenario 22 - rt_opts=latest, under-reporting=yes ##

  res_disease <- sim_scenarios(case_data=cholera_yem_tot,
                           gt,
                           gen_mean=d$gen[["mean"]],
                           gen_sd=d$gen[["sd"]],
                           gen_max = d$gen[["max"]],
                           inc_mean = d$inc[["mean"]],
                           inc_sd = d$inc[["sd"]],
                           inc_max = d$inc[["max"]],
                           rep_mean = d$rep[["mean"]],
                           rep_sd = d$rep[["sd"]],
                           rep_max = d$rep[["max"]],
                           freq_fc=freq_fc,
                           weeks_inc=weeks_inc,
                           rt_opts_choice="latest",
                           obs_scale=d$underreport)

save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen22_id", gt))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen22_warnings", gt))

## Saving samples only ##
save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen22_samples", gt))
save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen22_R", gt)) 

## Run scenario 23 - rt_opts=project, under-reporting=no ##

  res_disease <- sim_scenarios(case_data=cholera_yem_tot,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen[["max"]],
                             inc_mean = d$inc[["mean"]],
                             inc_sd = d$inc[["sd"]],
                             inc_max = d$inc[["max"]],
                             rep_mean = d$rep[["mean"]],
                             rep_sd = d$rep[["sd"]],
                             rep_max = d$rep[["max"]],
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen23_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen23_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen23_samples", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen23_R", gt)) 

## Run scenario 24 - rt_opts=project, under-reporting=yes ##

  res_disease <- sim_scenarios(case_data=cholera_yem_tot,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen[["max"]],
                             inc_mean = d$inc[["mean"]],
                             inc_sd = d$inc[["sd"]],
                             inc_max = d$inc[["max"]],
                             rep_mean = d$rep[["mean"]],
                             rep_sd = d$rep[["sd"]],
                             rep_max = d$rep[["max"]],
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="project",
                             obs_scale=d$underreport)
  
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen24_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen24_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen24_samples", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen24_R", gt)) 
  