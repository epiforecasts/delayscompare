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

#### Increasing Rt ####

## Loading data ##

ebola_sim_data_inc <- read_latest(here("data"), "ebola_sim_data_inc")
ebola_sim_data_inc_ur <- read_latest(here("data"), "ebola_sim_data_inc_ur")

# In required format for EpiNow2

ebola_sim_data_inc_cases <- ebola_sim_data_inc |> filter(variable=="reported_cases")
ebola_sim_data_inc_cases <- ebola_sim_data_inc_cases |>
  select(date, value) |>
  rename(confirm=value)

ebola_sim_data_inc_cases_ur <- ebola_sim_data_inc_ur |> filter(variable=="reported_cases")
ebola_sim_data_inc_cases_ur <- ebola_sim_data_inc_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

# ebola_rep_params <- get_parameters(fix_dist(ebola_reporting_delay))

## Run scenario 9 - rt_opts=latest, under-reporting=no ##
# 
 res_ebola <- sim_scenarios(case_data=ebola_sim_data_inc_cases,
                            gt,
                            gen_mean=16.2,
                            gen_sd=9.40, # from Park et al. 2019
                            gen_max=50,
                            inc_mean=11.4,
                            inc_sd=8.1, # from Aylward et al. 2014 
                            inc_max=60,
                            rep_mean=1,
                            rep_sd=1,
                            rep_max=0,
                            freq_fc=4,
                            weeks_inc=12,
                            rt_opts_choice="latest",
                            obs_scale=1)
 
save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen9_samples", gt))
save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen9_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen9_R", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen9_summary", gt))
save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen9_warnings", gt))

## Run scenario 10 - rt_opts=latest, under-reporting=yes ##

   res_ebola <- sim_scenarios(case_data=ebola_sim_data_inc_cases_ur,
                            gt,
                            gen_mean=16.2,
                            gen_sd=9.40, # from Park et al. 2019
                            gen_max=50,
                            inc_mean=11.4,
                            inc_sd=8.1, # from Aylward et al. 2014 
                            inc_max=60,
                            rep_mean=1,
                            rep_sd=1,
                            rep_max=0,
                            freq_fc=4,
                            weeks_inc=12,
                            rt_opts_choice="latest",
                            obs_scale=0.83)
 
save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen10_samples", gt))
save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen10_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen10_R", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen10_summary", gt))
save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen10_warnings", gt))

## Run scenario 11 - rt_opts=project, under-reporting=no ##

 res_ebola <- sim_scenarios(case_data=ebola_sim_data_inc_cases,
                            gt,
                            gen_mean=16.2,
                            gen_sd=9.40, # from Park et al. 2019
                            gen_max=50,
                            inc_mean=11.4,
                            inc_sd=8.1, # from Aylward et al. 2014 
                            inc_max=60,
                            rep_mean=1,
                            rep_sd=1,
                            rep_max=0,
                            freq_fc=4,
                            weeks_inc=12,
                            rt_opts_choice="project",
                            obs_scale=1)

save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen11_samples", gt))
save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen11_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen11_R", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen11_summary", gt))
save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen11_warnings", gt))

## Run scenario 12 - rt_opts=project, under-reporting=yes ##

res_ebola <- sim_scenarios(case_data=ebola_sim_data_inc_cases_ur,
                            gt,
                            gen_mean=16.2,
                            gen_sd=9.40, # from Park et al. 2019
                            gen_max=50,
                            inc_mean=11.4,
                            inc_sd=8.1, # from Aylward et al. 2014 
                            inc_max=60,
                            rep_mean=1,
                            rep_sd=1,
                            rep_max=0,
                            freq_fc=4,
                            weeks_inc=12,
                            rt_opts_choice="project",
                            obs_scale=0.83)
 
save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen12_samples", gt))
save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen12_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen12_R", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen12_summary", gt))
save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen12_warnings", gt))

  #### Decreasing Rt ####
  
  ## Loading data ##

  ebola_sim_data_dec <- read_latest(here("data"), "ebola_sim_data_dec")
  ebola_sim_data_dec_ur <- read_latest(here("data"), "ebola_sim_data_dec_ur")
  
# In required format for EpiNow2

 ebola_sim_data_dec_cases <- ebola_sim_data_dec |> filter(variable=="reported_cases")
 ebola_sim_data_dec_cases <- ebola_sim_data_dec_cases |>
   select(date, value) |>
   rename(confirm=value)
 
 ebola_sim_data_dec_cases_ur <- ebola_sim_data_dec_ur |> filter(variable=="reported_cases")
 ebola_sim_data_dec_cases_ur <- ebola_sim_data_dec_cases_ur |>
   select(date, value) |>
   rename(confirm=value)
 
## Run scenario 13 - rt_opts=latest, under-reporting=no ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_dec_cases,
                             gt,
                             gen_mean=16.2,
                             gen_sd=9.40, # from Park et al. 2019
                             gen_max=50,
                             inc_mean=11.4,
                             inc_sd=8.1, # from Aylward et al. 2014 
                             inc_max=60,
                             rep_mean=1,
                             rep_sd=1,
                             rep_max=0,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="latest",
                             obs_scale=1)

save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen13_samples", gt))
save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen13_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen13_R", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen13_summary", gt))
save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen13_warnings", gt))

## Run scenario 14 - rt_opts=latest, under-reporting=yes ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_dec_cases_ur,
                             gt,
                             gen_mean=16.2,
                             gen_sd=9.40, # from Park et al. 2019
                             gen_max=50,
                             inc_mean=11.4,
                             inc_sd=8.1, # from Aylward et al. 2014 
                             inc_max=60,
                             rep_mean=1,
                             rep_sd=1,
                             rep_max=0,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="latest",
                             obs_scale=0.83)

  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen14_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen14_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen14_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen14_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen14_warnings", gt))

## Run scenario 15 - rt_opts=project, under-reporting=no ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_dec_cases,
                             gt,
                             gen_mean=16.2,
                             gen_sd=9.40, # from Park et al. 2019
                             gen_max=50,
                             inc_mean=11.4,
                             inc_sd=8.1, # from Aylward et al. 2014 
                             inc_max=60,
                             rep_mean=1,
                             rep_sd=1,
                             rep_max=0,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen15_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen15_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen15_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen15_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen15_warnings", gt))

## Run scenario 16 - rt_opts=project, under-reporting=yes ##

  res_ebola <- sim_scenarios(case_data=ebola_sim_data_cases_ur,
                             gt,
                             gen_mean=16.2,
                             gen_sd=9.40, # from Park et al. 2019
                             gen_max=50,
                             inc_mean=11.4,
                             inc_sd=8.1, # from Aylward et al. 2014 
                             inc_max=60,
                             rep_mean=1,
                             rep_sd=1,
                             rep_max=0,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=0.83)
  
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen16_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen16_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen16_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen16_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen16_warnings", gt))
