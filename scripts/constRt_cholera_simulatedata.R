library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

#####################
#### Constant Rt ####
#####################

startdate <- as.Date("2017-04-23") # Same start date as data
enddate <- as.Date("2017-04-23") + 6*4*7 + 14  # Long enough time horizon to have six forecast timepoints with forecasts every 4 weeks

##### Simulate data - low Rt ######

rt_const_low <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                           R=0.8)

cholera_sim_data_const_low <- simulate_infections(
  R=rt_const_low,
  initial_infections=75,
  generation_time=gt_opts(cholera_gen_time),
  delays=delay_opts(cholera_combined_delay),
  obs=obs_opts(family="poisson", scale=Fixed(1))
)

cholera_sim_data_const_low_ur <- simulate_infections(
  R=rt_const_low,
  initial_infections=75,
  generation_time=gt_opts(cholera_gen_time),
  delays=delay_opts(cholera_combined_delay),
  obs=obs_opts(family="poisson", scale=Fixed(0.28))
)

save_latest(cholera_sim_data_const_low, here("data"), "cholera_sim_data_const_low")
save_latest(cholera_sim_data_const_low_ur, here("data"), "cholera_sim_data_const_low_ur")

##### Simulate data - high Rt ######

rt_const_hi <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                          R=1.2)

cholera_sim_data_const_hi <- simulate_infections(
  R=rt_const_hi,
  initial_infections=75,
  generation_time=gt_opts(cholera_gen_time),
  delays=delay_opts(cholera_combined_delay),
  obs=obs_opts(family="poisson", scale=Fixed(1))
)

cholera_sim_data_const_hi_ur <- simulate_infections(
  R=rt_const_hi,
  initial_infections=75,
  generation_time=gt_opts(cholera_gen_time),
  delays=delay_opts(cholera_combined_delay),
  obs=obs_opts(family="poisson", scale=Fixed(0.28))
)

save_latest(cholera_sim_data_const_hi, here("data"), "cholera_sim_data_const_hi")
save_latest(cholera_sim_data_const_hi_ur, here("data"), "cholera_sim_data_const_hi_ur")

