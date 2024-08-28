library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

############### SCENARIOS #################

## Loading data ##

ebola_sim_data_sine <- read_latest(here("data"), "ebola_sim_data_sine")
ebola_sim_data_sine_ur <- read_latest(here("data"), "ebola_sim_data_sine_ur")

# In required format for EpiNow2

ebola_sim_data_sine_cases <- ebola_sim_data_sine |> filter(variable=="reported_cases")
ebola_sim_data_sine_cases <- ebola_sim_data_sine_cases |>
  select(date, value) |>
  rename(confirm=value)

ebola_sim_data_sine_cases_ur <- ebola_sim_data_sine_ur |> filter(variable=="reported_cases")
ebola_sim_data_sine_cases_ur <- ebola_sim_data_sine_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

ebola_rep_params <- get_parameters(fix_dist(ebola_reporting_delay))

## Run scenario 17 - rt_opts=latest, under-reporting=no ##

res_ebola <- sim_scenarios(case_data=ebola_sim_data_sine_cases,
                           gt,
                           gen_mean=16.2,
                           gen_sd=9.40, # from Park et al. 2019
                           gen_max=50,
                           inc_mean=11.4,
                           inc_sd=8.1, # from Aylward et al. 2014 
                           inc_max=60,
                           rep_meanlog=ebola_rep_params$meanlog,
                           rep_sdlog=ebola_rep_params$sdlog,
                           rep_max=50,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=1)

save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen17_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen17_warnings", gt))

## Saving samples only ##
save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen17_samples", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen17_R", gt)) 

## Run scenario 18 - rt_opts=latest, under-reporting=yes ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_sine_cases_ur,
                           gt,
                           gen_mean=16.2,
                           gen_sd=9.40, # from Park et al. 2019
                           gen_max=50,
                           inc_mean=11.4,
                           inc_sd=8.1, # from Aylward et al. 2014 
                           inc_max=60,
                           rep_meanlog=ebola_rep_params$meanlog,
                           rep_sdlog=ebola_rep_params$sdlog,
                           rep_max=50,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=0.83)

save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen18_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen18_warnings", gt))

## Saving samples only ##
save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen18_samples", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen18_R", gt)) 

## Run scenario 19 - rt_opts=project, under-reporting=no ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_sine_cases,
                             gt,
                             gen_mean=16.2,
                             gen_sd=9.40, # from Park et al. 2019
                             gen_max=50,
                             inc_mean=11.4,
                             inc_sd=8.1, # from Aylward et al. 2014 
                             inc_max=60,
                             rep_meanlog=ebola_rep_params$meanlog,
                             rep_sdlog=ebola_rep_params$sdlog,
                             rep_max=50,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen19_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen19_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen19_samples", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen19_R", gt)) 

## Run scenario 20 - rt_opts=project, under-reporting=yes ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_sine_cases_ur,
                             gt,
                             gen_mean=16.2,
                             gen_sd=9.40, # from Park et al. 2019
                             gen_max=50,
                             inc_mean=11.4,
                             inc_sd=8.1, # from Aylward et al. 2014 
                             inc_max=60,
                             rep_meanlog=ebola_rep_params$meanlog,
                             rep_sdlog=ebola_rep_params$sdlog,
                             rep_max=50,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=0.83)
  
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen20_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen20_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen20_samples", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen20_R", gt)) 
  
