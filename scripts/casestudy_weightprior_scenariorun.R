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

disease <- "covid"

case_data <- casestudydata[[disease]]
d <- delays[[disease]]

############### SCENARIOS #################

## Run scenario 25 - rt_opts=latest, under-reporting=no, weight_prior=TRUE ##

res_disease <- sim_weightprior(case_data=covid_eng,
                           gt,
                           gen_mean_mean=d$gen[["mean"]],
                           gen_mean_sd=d$gen[["mean_sd"]],
                           gen_sd_mean=d$gen[["sd"]], 
                           gen_sd_sd=d$gen[["sd_sd"]],
                           gen_max=d$gen[["max"]],
                           inc_mean_mean=d$inc[["mean"]],
                           inc_mean_sd=d$inc[["mean_sd"]],
                           inc_sd_mean=d$inc[["sd"]], 
                           inc_sd_sd=d$inc[["sd_sd"]],
                           inc_max=d$inc[["max"]],
                           rep_mean_mean=d$rep[["mean"]],
                           rep_mean_sd=d$rep[["mean_sd"]],
                           rep_sd_mean=d$rep[["sd"]],
                           rep_sd_sd=d$rep[["sd_sd"]],
                           rep_max=d$rep[["max"]],
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           weight_prior=TRUE,
                           obs_scale=1)

save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen25_samples", gt))
save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen25_id", gt))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen25_R", gt))
save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen25_summary", gt))
save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "scen25_warnings", gt))

## Run scenario 26 - rt_opts=latest, under-reporting=yes, weight_prior=TRUE ##

  res_disease <- sim_weightprior(case_data=covid_eng,
                           gt,
                           gen_mean_mean=d$gen[["mean"]],
                           gen_mean_sd=d$gen[["mean_sd"]],
                           gen_sd_mean=d$gen[["sd"]], 
                           gen_sd_sd=d$gen[["sd_sd"]],
                           gen_max=d$gen[["max"]],
                           inc_mean_mean=d$inc[["mean"]],
                           inc_mean_sd=d$inc[["mean_sd"]],
                           inc_sd_mean=d$inc[["sd"]], 
                           inc_sd_sd=d$inc[["sd_sd"]],
                           inc_max=d$inc[["max"]],
                           rep_mean_mean=d$rep[["mean"]],
                           rep_mean_sd=d$rep[["mean_sd"]],
                           rep_sd_mean=d$rep[["sd"]],
                           rep_sd_sd=d$rep[["sd_sd"]],
                           rep_max=d$rep[["max"]],
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           weight_prior=TRUE,
                           obs_scale=d$underreport)

  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen26_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen26_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen26_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen26_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "scen26_warnings", gt))
  
## Run scenario 27 - rt_opts=project, under-reporting=no, weight_prior=TRUE ##

  res_disease <- sim_weightprior(case_data=covid_eng,
                             gt,
                             gen_mean_mean=d$gen[["mean"]],
                             gen_mean_sd=d$gen[["mean_sd"]],
                             gen_sd_mean=d$gen[["sd"]], 
                             gen_sd_sd=d$gen[["sd_sd"]],
                             gen_max=d$gen[["max"]],
                             inc_mean_mean=d$inc[["mean"]],
                             inc_mean_sd=d$inc[["mean_sd"]],
                             inc_sd_mean=d$inc[["sd"]], 
                             inc_sd_sd=d$inc[["sd_sd"]],
                             inc_max=d$inc[["max"]],
                             rep_mean_mean=d$rep[["mean"]],
                             rep_mean_sd=d$rep[["mean_sd"]],
                             rep_sd_mean=d$rep[["sd"]],
                             rep_sd_sd=d$rep[["sd_sd"]],
                             rep_max=d$rep[["max"]],
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             weight_prior=TRUE,
                             obs_scale=1)
  
  
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen27_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen27_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen27_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen27_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "scen27_warnings", gt))
  
## Run scenario 28 - rt_opts=project, under-reporting=yes, weight_prior=TRUE ##

  res_disease <- sim_weightprior(case_data=covid_eng,
                             gt,
                             gen_mean_mean=d$gen[["mean"]],
                             gen_mean_sd=d$gen[["mean_sd"]],
                             gen_sd_mean=d$gen[["sd"]], 
                             gen_sd_sd=d$gen[["sd_sd"]],
                             gen_max=d$gen[["max"]],
                             inc_mean_mean=d$inc[["mean"]],
                             inc_mean_sd=d$inc[["mean_sd"]],
                             inc_sd_mean=d$inc[["sd"]], 
                             inc_sd_sd=d$inc[["sd_sd"]],
                             inc_max=d$inc[["max"]],
                             rep_mean_mean=d$rep[["mean"]],
                             rep_mean_sd=d$rep[["mean_sd"]],
                             rep_sd_mean=d$rep[["sd"]],
                             rep_sd_sd=d$rep[["sd_sd"]],
                             rep_max=d$rep[["max"]],
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             weight_prior=TRUE,
                             obs_scale=d$underreport)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen28_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen28_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen28_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen28_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "scen28_warnings", gt))
  
  ## Run scenario 29 - rt_opts=latest, under-reporting=no, weight_prior=FALSE ##
  
  res_disease <- sim_weightprior(case_data=covid_eng,
                               gt,
                               gen_mean_mean=d$gen[["mean"]],
                               gen_mean_sd=d$gen[["mean_sd"]],
                               gen_sd_mean=d$gen[["sd"]], 
                               gen_sd_sd=d$gen[["sd_sd"]],
                               gen_max=d$gen[["max"]],
                               inc_mean_mean=d$inc[["mean"]],
                               inc_mean_sd=d$inc[["mean_sd"]],
                               inc_sd_mean=d$inc[["sd"]], 
                               inc_sd_sd=d$inc[["sd_sd"]],
                               inc_max=d$inc[["max"]],
                               rep_mean_mean=d$rep[["mean"]],
                               rep_mean_sd=d$rep[["mean_sd"]],
                               rep_sd_mean=d$rep[["sd"]],
                               rep_sd_sd=d$rep[["sd_sd"]],
                               rep_max=d$rep[["max"]],
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="latest",
                               weight_prior=FALSE,
                               obs_scale=1)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen29_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen29_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen29_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen29_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "scen29_warnings", gt))
  
  ## Run scenario 30 - rt_opts=latest, under-reporting=yes, weight_prior=FALSE ##
  
  res_disease <- sim_weightprior(case_data=covid_eng,
                               gt,
                               gen_mean_mean=d$gen[["mean"]],
                               gen_mean_sd=d$gen[["mean_sd"]],
                               gen_sd_mean=d$gen[["sd"]], 
                               gen_sd_sd=d$gen[["sd_sd"]],
                               gen_max=d$gen[["max"]],
                               inc_mean_mean=d$inc[["mean"]],
                               inc_mean_sd=d$inc[["mean_sd"]],
                               inc_sd_mean=d$inc[["sd"]], 
                               inc_sd_sd=d$inc[["sd_sd"]],
                               inc_max=d$inc[["max"]],
                               rep_mean_mean=d$rep[["mean"]],
                               rep_mean_sd=d$rep[["mean_sd"]],
                               rep_sd_mean=d$rep[["sd"]],
                               rep_sd_sd=d$rep[["sd_sd"]],
                               rep_max=d$rep[["max"]],
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="latest",
                               weight_prior=FALSE,
                               obs_scale=d$underreport)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen30_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen30_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen30_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen30_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "scen30_warnings", gt))
  
  ## Run scenario 31 - rt_opts=project, under-reporting=no, weight_prior=FALSE ##
  
  res_disease <- sim_weightprior(case_data=covid_eng,
                               gt,
                               gen_mean_mean=d$gen[["mean"]],
                               gen_mean_sd=d$gen[["mean_sd"]],
                               gen_sd_mean=d$gen[["sd"]], 
                               gen_sd_sd=d$gen[["sd_sd"]],
                               gen_max=d$gen[["max"]],
                               inc_mean_mean=d$inc[["mean"]],
                               inc_mean_sd=d$inc[["mean_sd"]],
                               inc_sd_mean=d$inc[["sd"]], 
                               inc_sd_sd=d$inc[["sd_sd"]],
                               inc_max=d$inc[["max"]],
                               rep_mean_mean=d$rep[["mean"]],
                               rep_mean_sd=d$rep[["mean_sd"]],
                               rep_sd_mean=d$rep[["sd"]],
                               rep_sd_sd=d$rep[["sd_sd"]],
                               rep_max=d$rep[["max"]],
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="project",
                               weight_prior=FALSE,
                               obs_scale=1)
  
  
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen31_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen31_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen31_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen31_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "scen31_warnings", gt))
  
  ## Run scenario 32 - rt_opts=project, under-reporting=yes, weight_prior=FALSE ##
  
  res_disease <- sim_weightprior(case_data=covid_eng,
                               gt,
                               gen_mean_mean=d$gen[["mean"]],
                               gen_mean_sd=d$gen[["mean_sd"]],
                               gen_sd_mean=d$gen[["sd"]], 
                               gen_sd_sd=d$gen[["sd_sd"]],
                               gen_max=d$gen[["max"]],
                               inc_mean_mean=d$inc[["mean"]],
                               inc_mean_sd=d$inc[["mean_sd"]],
                               inc_sd_mean=d$inc[["sd"]], 
                               inc_sd_sd=d$inc[["sd_sd"]],
                               inc_max=d$inc[["max"]],
                               rep_mean_mean=d$rep[["mean"]],
                               rep_mean_sd=d$rep[["mean_sd"]],
                               rep_sd_mean=d$rep[["sd"]],
                               rep_sd_sd=d$rep[["sd_sd"]],
                               rep_max=d$rep[["max"]],
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="project",
                               weight_prior=FALSE,
                               obs_scale=d$underreport)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "scen32_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen32_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen32_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "scen32_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "scen32_warnings", gt))
  
  