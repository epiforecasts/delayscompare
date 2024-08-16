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

ebola_sim_data_const_low <- read_latest(here("data"), "ebola_sim_data_const_low")
ebola_sim_data_const_low_ur <- read_latest(here("data"), "ebola_sim_data_const_low_ur")

# In required format for EpiNow2

ebola_sim_data_cases <- ebola_sim_data_const_low |> filter(variable=="reported_cases")
ebola_sim_data_cases <- ebola_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

ebola_sim_data_cases_ur <- ebola_sim_data_const_low_ur |> filter(variable=="reported_cases")
ebola_sim_data_cases_ur <- ebola_sim_data_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

ebola_rep_params <- get_parameters(fix_dist(ebola_reporting_delay))

## Run scenario 1 - rt_opts=latest, under-reporting=no ##

res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases,
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

save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen1_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen1_warnings", gt))

## Saving samples only ##
save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen1_samples", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen1_R", gt)) 

## Run scenario 2 - rt_opts=latest, under-reporting=yes ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases_ur,
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

save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen2_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen2_warnings", gt))

## Saving samples only ##
save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen2_samples", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen2_R", gt)) 

## Run scenario 3 - rt_opts=project, under-reporting=no ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases,
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
  
  
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen3_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen3_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen3_samples", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen3_R", gt)) 

## Run scenario 4 - rt_opts=project, under-reporting=yes ##

  
  res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases_ur,
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
  
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen4_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen4_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen4_samples", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen4_R", gt)) 
  
  ## Loading data ##

  ebola_sim_data_const_hi <- read_latest(here("data"), "ebola_sim_data_const_hi")
  ebola_sim_data_const_hi_ur <- read_latest(here("data"), "ebola_sim_data_const_hi_ur")
  
# In required format for EpiNow2

ebola_sim_data_cases <- ebola_sim_data_const_hi |> filter(variable=="reported_cases")
ebola_sim_data_cases <- ebola_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

ebola_sim_data_cases_ur <- ebola_sim_data_const_hi_ur |> filter(variable=="reported_cases")
ebola_sim_data_cases_ur <- ebola_sim_data_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

ebola_rep_params <- get_parameters(fix_dist(ebola_reporting_delay))

## Run scenario 5 - rt_opts=latest, under-reporting=no ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases,
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
  
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen5_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen5_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen5_samples", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen5_R", gt)) 

## Run scenario 6 - rt_opts=latest, under-reporting=yes ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases_ur,
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
  
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen6_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen6_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen6_samples", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen6_R", gt)) 

## Run scenario 7 - rt_opts=project, under-reporting=no ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases,
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
  
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen7_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen7_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen7_samples", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen7_R", gt)) 

## Run scenario 8 - rt_opts=project, under-reporting=yes ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases_ur,
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
  
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen8_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen8_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen8_samples", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen8_R", gt)) 
}
