library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

rt_opts <- var[2]
print(rt_opts)
disease <- var[3]
print(disease)

if (!disease %in% names(delays)) {
 stop("Invalid disease. Must be one of: ", paste(names(delays), collapse=", "))
}
 
d <- delays[[disease]]

############### SCENARIOS #################

## Loading data ##

sim_data_const_low <- read_latest(here("data"), paste0(disease, "_sim_data_const_low"))

# In required format for EpiNow2

sim_data_low_cases <- sim_data_const_low |> filter(variable=="reported_cases")
sim_data_low_cases <- sim_data_low_cases |>
  select(date, value) |>
  rename(confirm=value)

## Universal parameter values ##
freq_fc=4
weeks_inc=12

## Rt const low - under-reporting=no ##

res_disease <- sim_scenarios(case_data=sim_data_low_cases,
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
                           rt_opts_choice=rt_opts,
                           obs_scale=1)

save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "_const_low_", rt_opts, "_id", gt))
save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "_const_low_", rt_opts, "_warnings", gt))
save_latest(res_disease[[6]], here("results"), paste0("res_", disease, "_const_low_", rt_opts, "_timing", gt))

## Saving samples only ##
save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "_const_low_", rt_opts, "_samples", gt))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "_const_low_", rt_opts, "_R", gt))

  ## Loading data ##

sim_data_const_hi <- read_latest(here("data"), paste0(disease, "_sim_data_const_hi"))

# In required format for EpiNow2

sim_data_hi_cases <- sim_data_const_hi |> filter(variable=="reported_cases")
sim_data_hi_cases <- sim_data_hi_cases |>
  select(date, value) |>
  rename(confirm=value)

## Rt const low - under-reporting=no ##

  res_disease <- sim_scenarios(case_data=sim_data_hi_cases,
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
                             rt_opts_choice=rt_opts,
                             obs_scale=1)
  
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "_const_high_", rt_opts, "_id", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "_const_high_", rt_opts, "_warnings", gt))
  save_latest(res_disease[[6]], here("results"), paste0("res_", disease, "_const_high_", rt_opts, "_timing", gt))
  
  ## Saving samples only ##
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "_const_high_", rt_opts, "_samples", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "_const_high_", rt_opts, "_R", gt))
  