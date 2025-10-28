library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

disease <- "ebola"
d <- delays[[disease]]

#####################
#### Constant Rt ####
#####################

startdate <- startenddates[[disease]][["startdate"]]
enddate <- startenddates[[disease]][["enddate"]]

gen_time <- get(paste0(disease, "_gen_time"))
inc_period <- get(paste0(disease, "_inc_period"))

if(exists(paste0(disease, "_reporting_delay"))){ # To cope with lack of Ebola reporting delay
  rep_delay <- get(paste0(disease, "_reporting_delay"))
  combined_delay <- inc_period + rep_delay
} else {combined_delay <- inc_period}

##### Simulate data - low Rt ######

rt_const_low <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                           R=0.8)

sim_data_const_low <- simulate_infections(
  R=rt_const_low,
  initial_infections=d$init_infs,
  generation_time=generation_time_opts(gen_time),
  delays=delay_opts(combined_delay),
  obs=obs_opts(family="poisson", scale=Fixed(1))
)

sim_data_const_low_ur <- simulate_infections(
  R=rt_const_low,
  initial_infections=d$init_infs,
  generation_time=generation_time_opts(gen_time),
  delays=delay_opts(combined_delay),
  obs=obs_opts(family="poisson", scale=Fixed(d$underreport))
)

save_latest(sim_data_const_low, here("data"), paste0(disease, "_sim_data_const_low"))
save_latest(sim_data_const_low_ur, here("data"), paste0(diseaes,"_sim_data_const_low_ur"))

##### Simulate data - high Rt ######

rt_const_hi <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                          R=1.2)

sim_data_const_hi <- simulate_infections(
  R=rt_const_hi,
  initial_infections=d$init_infs,
  generation_time=generation_time_opts(gen_time),
  delays=delay_opts(combined_delay),
  obs=obs_opts(family="poisson", scale=Fixed(1))
)

sim_data_const_hi_ur <- simulate_infections(
  R=rt_const_hi,
  initial_infections=d$init_infs,
  generation_time=generation_time_opts(gen_time),
  delays=delay_opts(combined_delay),
  obs=obs_opts(family="poisson", scale=Fixed(d$underreport))
)

save_latest(sim_data_const_hi, here("data"), paste0(disease,"sim_data_const_hi"))
save_latest(sim_data_const_hi_ur, here("data"), paste0(disease,"_sim_data_const_hi_ur"))
