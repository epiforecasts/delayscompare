library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

#######################
#### Sinusoidal Rt ####
#######################

startdate <- as.Date("2014-05-23") # Same start date as data
enddate <- as.Date("2014-05-23") + 6*4*7 + 14  # Long enough time horizon to have six forecast timepoints with forecasts every 4 weeks

##### Simulate data ######

total_days <- length(seq.Date(from=startdate, to=enddate, by=1))
cycles <- 2

T <- total_days / cycles      
omega <- 2 * pi / T  

rt_sine <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                      R=0.2*sin(omega*seq(1:total_days))+1)

ebola_sim_data_sine <- simulate_infections(
  R=rt_sine,
  initial_infections=5,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(fix_dist(combined_delay_ebola)),
  obs=obs_opts(family="poisson", scale=1)
)

ebola_sim_data_sine_ur <- simulate_infections(
  R=rt_sine,
  initial_infections=5,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(fix_dist(combined_delay_ebola)),
  obs=obs_opts(family="poisson", scale=0.83)
)

save_latest(rt_sine, here('data'), "ebolaRt_sine")
save_latest(ebola_sim_data_sine, here("data"), "ebola_sim_data_sine")
save_latest(ebola_sim_data_sine_ur, here("data"), "ebola_sim_data_sine_ur")
