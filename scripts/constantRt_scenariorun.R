library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

disease <- "cholera"
d <- delays[[disease]]

############### SCENARIOS #################

## Loading data ##

sim_data_const_low <- read_latest(here("data"), paste0(disease, "_sim_data_const_low"))
sim_data_const_low_ur <- read_latest(here("data"), paste0(disease, "_sim_data_const_low_ur"))

# In required format for EpiNow2

sim_data_low_cases <- sim_data_const_low |> filter(variable=="reported_cases")
sim_data_low_cases <- sim_data_low_cases |>
  select(date, value) |>
  rename(confirm=value)

sim_data_low_cases_ur <- sim_data_const_low_ur |> filter(variable=="reported_cases")
sim_data_low_cases_ur <- sim_data_low_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

## Universal parameter values ##
freq_fc=4
weeks_inc=12

## Run scenario 1 - rt_opts=latest, under-reporting=no ##

res_disease <- sim_scenarios(case_data=sim_data_low_cases,
                           gt,
                           gen_mean=d$gen[["mean"]],
                           gen_sd=d$gen[["sd"]],
                           gen_max = d$gen["max"],
                           inc_mean = d$inc["mean"],
                           inc_sd = d$inc["sd"],
                           inc_max = d$inc["max"],
                           rep_mean = d$rep["mean"],
                           rep_sd = d$rep["sd"],
                           rep_max = d$rep["max"],
                           freq_fc=freq_fc,
                           weeks_inc=weeks_inc,
                           rt_opts_choice="latest",
                           obs_scale=1)

save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "scen1_id", gt))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "scen1_warnings", gt))

## Saving samples only ##
save_latest(res_disease[[1]], here("results"), paste0("res_", disease,"scen1_samples", gt))
save_latest(res_disease[[4]], here("results"), paste0("res_", disease,"scen1_R", gt)) 

## Run scenario 2 - rt_opts=latest, under-reporting=yes ##

  res_disease <- sim_scenarios(case_data=sim_data_low_cases_ur,
                           gt,
                           gen_mean=d$gen[["mean"]],
                           gen_sd=d$gen[["sd"]],
                           gen_max = d$gen["max"],
                           inc_mean = d$inc["mean"],
                           inc_sd = d$inc["sd"],
                           inc_max = d$inc["max"],
                           rep_mean = d$rep["mean"],
                           rep_sd = d$rep["sd"],
                           rep_max = d$rep["max"],
                           freq_fc=freq_fc,
                           weeks_inc=weeks_inc,
                           rt_opts_choice="latest",
                           obs_scale=0.28)

save_latest(res_disease[[2]], here("results"), paste0("res_", disease,"scen2_id", gt))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease,"scen2_warnings", gt))

## Saving samples only ##
save_latest(res_disease[[1]], here("results"), paste0("res_", disease,"scen2_samples", gt))
save_latest(res_disease[[4]], here("results"), paste0("res_", disease,"scen2_R", gt)) 

## Run scenario 3 - rt_opts=project, under-reporting=no ##

  res_disease <- sim_scenarios(case_data=sim_data_low_cases,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease,"scen3_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease,"scen3_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease,"scen3_samples", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease,"scen3_R", gt)) 

## Run scenario 4 - rt_opts=project, under-reporting=yes ##

  res_disease <- sim_scenarios(case_data=sim_data_low_cases_ur,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="project",
                             obs_scale=0.28)
  
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease,"scen4_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease,"scen4_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease,"scen4_samples", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease,"scen4_R", gt)) 
  
  ## Loading data ##

sim_data_const_hi <- read_latest(here("data"), paste0(disease, "_sim_data_const_hi"))
sim_data_const_hi_ur <- read_latest(here("data"), paste0(disease, "_sim_data_const_hi_ur"))
  
# In required format for EpiNow2

sim_data_hi_cases <- sim_data_const_hi |> filter(variable=="reported_cases")
sim_data_hi_cases <- sim_data_hi_cases |>
  select(date, value) |>
  rename(confirm=value)

sim_data_hi_cases_ur <- sim_data_const_hi_ur |> filter(variable=="reported_cases")
sim_data_hi_cases_ur <- sim_data_hi_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

## Run scenario 5 - rt_opts=latest, under-reporting=no ##

  res_disease <- sim_scenarios(case_data=sim_data_hi_cases,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="latest",
                             obs_scale=1)
  
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease,"scen5_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease,"scen5_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease,"scen5_samples", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease,"scen5_R", gt)) 

## Run scenario 6 - rt_opts=latest, under-reporting=yes ##

  res_disease <- sim_scenarios(case_data=sim_data_hi_cases_ur,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="latest",
                             obs_scale=0.28)
  
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease,"scen6_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease,"scen6_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease,"scen6_samples", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease,"scen6_R", gt)) 

## Run scenario 7 - rt_opts=project, under-reporting=no ##

  res_disease <- sim_scenarios(case_data=sim_data_hi_cases,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease,"scen7_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease,"scen7_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease,"scen7_samples", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease,"scen7_R", gt)) 

## Run scenario 8 - rt_opts=project, under-reporting=yes ##

  res_disease <- sim_scenarios(case_data=sim_data_hi_cases_ur,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=freq_fc,
                             weeks_inc=weeks_inc,
                             rt_opts_choice="project",
                             obs_scale=0.28)
  
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease,"scen8_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease,"scen8_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease,"scen8_samples", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease,"scen8_R", gt)) 

