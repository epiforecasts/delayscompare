library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

startdate <- as.Date("2014-06-01")
enddate <- as.Date("2014-10-01")

#####################
#### Constant Rt ####
#####################

##### Simulate data ######

rt_constant <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                          R=2)

ebola_sim_data <- simulate_infections(
  R=rt_constant,
  initial_infections=5,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(fix_dist(combined_delay_ebola)),
  obs=obs_opts(family="poisson", scale=1)
)

save_latest(ebola_sim_data, here("data"), "ebola_sim_data_const")

## In required format for EpiNow2 ##

ebola_sim_data_cases <- ebola_sim_data |> filter(variable=="reported_cases")
ebola_sim_data_cases <- ebola_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

for(gt in c(1:6)){

## Run scenarios ##
ebola_rep_params <- get_parameters(fix_dist(ebola_reporting_delay))
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
                           freq_fc=8,
                           weeks_inc=12,
                           obs_scale=1)

save_latest(res_ebola[[2]], here("results"), paste0("res_const_ebola_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_const_ebola_warnings", gt))

## Saving samples only ##
save_latest(res_ebola[[1]], here("results"), paste0("res_const_ebola_samples", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_const_ebola_R", gt)) }

#######################
#### Increasing Rt ####
#######################

##### Simulate data ######

rt_inc <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                     R=seq(from=0.5, to=2.5, length.out=difftime(enddate, startdate-1)))

ebola_sim_data <- simulate_infections(
  R=rt_constant,
  initial_infections=5,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(fix_dist(combined_delay_ebola)),
  obs=obs_opts(family="poisson", scale=1)
)

save_latest(ebola_sim_data, here("data"), "ebola_sim_data_inc")

## In required format for EpiNow2 ##

ebola_sim_data_cases <- ebola_sim_data |> filter(variable=="reported_cases")
ebola_sim_data_cases <- ebola_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

for(gt in c(1:6)){
  
  ## Run scenarios ##
  ebola_rep_params <- get_parameters(fix_dist(ebola_reporting_delay))
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
                             freq_fc=8,
                             weeks_inc=12,
                             obs_scale=1)
  
  save_latest(res_ebola[[2]], here("results"), paste0("res_inc_ebola_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_inc_ebola_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_ebola[[1]], here("results"), paste0("res_inc_ebola_samples", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_inc_ebola_R", gt)) }

#######################
#### Decreasing Rt ####
#######################

rt_dec <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                     R=rev(seq(from=0.5, to=2.5, length.out=difftime(enddate, startdate-1))))

ebola_sim_data <- simulate_infections(
  R=rt_constant,
  initial_infections=5,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(fix_dist(combined_delay_ebola)),
  obs=obs_opts(family="poisson", scale=1)
)

save_latest(ebola_sim_data, here("data"), "ebola_sim_data_dec")

## In required format for EpiNow2 ##

ebola_sim_data_cases <- ebola_sim_data |> filter(variable=="reported_cases")
ebola_sim_data_cases <- ebola_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

for(gt in c(1:6)){
  
  ## Run scenarios ##
  ebola_rep_params <- get_parameters(fix_dist(ebola_reporting_delay))
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
                             freq_fc=8,
                             weeks_inc=12,
                             obs_scale=1)
  
  save_latest(res_ebola[[2]], here("results"), paste0("res_dec_ebola_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_dec_ebola_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_ebola[[1]], here("results"), paste0("res_dec_ebola_samples", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_dec_ebola_R", gt)) }



save_latest(ebola_sim_data, here("data"), "ebola_sim_data_const")

