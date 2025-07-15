
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

ebola_confirmed_linelist <- read_xlsx(here("data", "ebola_linelist.xlsx"), "lab-confirmed database")

# Formating for EpiNow2

ebola_confirmed <- incidence(ebola_confirmed_linelist,
                             date_index="Date of sample tested",
                             interval="day")

ebola_confirmed <- ebola_confirmed |>
  select(date_index, count) |>
  rename(date=date_index, confirm=count)

# Assuming no data reported on missing days
extra_dates <- data.frame(date=seq(ebola_confirmed$date[1], ebola_confirmed$date[nrow(ebola_confirmed)], by="day"))
ebola_confirmed <- right_join(ebola_confirmed, extra_dates, by="date")

ebola_confirmed$confirm[is.na(ebola_confirmed$confirm)] <- 0

# Make sure days are in order
ebola_confirmed <- ebola_confirmed |>
  arrange(date)

############### SCENARIOS #################

## Run scenario 21 - rt_opts=latest, under-reporting=no ##

res_ebola <- sim_scenarios(case_data=ebola_confirmed,
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

save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen21_samples", gt))
save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen21_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen21_R", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen21_summary", gt))
save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen21_warnings", gt))

## Run scenario 22 - rt_opts=latest, under-reporting=yes ##

  res_ebola <- sim_scenarios(case_data=ebola_confirmed,
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

  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen22_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen22_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen22_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen22_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen22_warnings", gt))

## Run scenario 23 - rt_opts=project, under-reporting=no ##
  res_ebola <- sim_scenarios(case_data=ebola_confirmed,
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
  
  
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen23_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen23_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen23_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen23_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen23_warnings", gt))

## Run scenario 24 - rt_opts=project, under-reporting=yes ##
  res_ebola <- sim_scenarios(case_data=ebola_confirmed,
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
  
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen24_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen24_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen24_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen24_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen24_warnings", gt))
  
