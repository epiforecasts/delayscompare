library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

#######################
#### Sinusoidal Rt ####
#######################

startdate <- as.Date("2020-06-19") # Same start date as data
enddate <- as.Date("2020-06-19") + 6*4*7 + 14 # Long enough time horizon to have six forecast time points with forecasts every 4 weeks, + two weeks data at the end for forecast

##### Simulate data - increasing ######

total_days <- length(seq.Date(from=startdate, to=enddate, by=1))
cycles <- 2

T <- total_days / cycles      
omega <- 2 * pi / T  

rt_sine <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                      R=0.2*sin(omega*seq(1:total_days))+1)

covid_sim_data_sine <- simulate_infections(
  R=rt_sine,
  initial_infections=2500,
  generation_time=generation_time_opts(covid_gen_time),
  delays=delay_opts(fix_dist(combined_delay_covid)),
  obs=obs_opts(family="poisson", scale=1)
)

covid_sim_data_sine_ur <- simulate_infections(
  R=rt_sine,
  initial_infections=2500,
  generation_time=generation_time_opts(covid_gen_time),
  delays=delay_opts(fix_dist(combined_delay_covid)),
  obs=obs_opts(family="poisson", scale=0.3)
)

save_latest(rt_sine, here('data'), "covidRt_sine")
save_latest(covid_sim_data_sine, here("data"), "covid_sim_data_sine")
save_latest(covid_sim_data_sine_ur, here("data"), "covid_sim_data_sine_ur")
