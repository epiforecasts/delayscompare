library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

rt_opts <- var[2]

## Load data ##

disease <- "covid"

case_data <- casestudydata[[disease]]
d <- delays[[disease]]

############### SCENARIOS #################

## under-reporting=yes, weight_prior=TRUE ##

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

save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "_weightprior_TRUE", rt_opts, "_samples", gt))
save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "_weightprior_TRUE", rt_opts, "_id", gt))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "_weightprior_TRUE", rt_opts, "_R", gt))
save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "_weightprior_TRUE", rt_opts, "_summary", gt))
save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "_weightprior_TRUE", rt_opts, "_warnings", gt))

## under-reporting=yes, weight_prior=TRUE ##

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

save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "_weightprior_FALSE", rt_opts, "_samples", gt))
save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "_weightprior_FALSE", rt_opts, "_id", gt))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "_weightprior_FALSE", rt_opts, "_R", gt))
save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "_weightprior_FALSE", rt_opts, "_summary", gt))
save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "_weightprior_FALSE", rt_opts, "_warnings", gt))