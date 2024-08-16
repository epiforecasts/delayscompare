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

##### Simulate data - increasing ######

rt_inc <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                     R=seq(from=0.8, to=1.2, length.out=enddate-(startdate-1)))

cholera_sim_data_inc <- simulate_infections(
  R=rt_inc,
  initial_infections=75,
  generation_time=generation_time_opts(cholera_gen_time),
  delays=delay_opts(fix_dist(combined_delay_cholera)),
  obs=obs_opts(family="poisson", scale=1)
)

cholera_sim_data_inc_ur <- simulate_infections(
  R=rt_inc,
  initial_infections=75,
  generation_time=generation_time_opts(cholera_gen_time),
  delays=delay_opts(fix_dist(combined_delay_cholera)),
  obs=obs_opts(family="poisson", scale=0.28)
)

save_latest(cholera_sim_data_inc, here("data"), "cholera_sim_data_inc")
save_latest(cholera_sim_data_inc_ur, here("data"), "cholera_sim_data_inc_ur")

##### Simulate data - decreasing Rt ######

rt_dec <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                     R=rev(seq(from=0.8, to=1.2, length.out=enddate-(startdate-1))))

cholera_sim_data_dec <- simulate_infections(
  R=rt_dec,
  initial_infections=75,
  generation_time=generation_time_opts(cholera_gen_time),
  delays=delay_opts(fix_dist(combined_delay_cholera)),
  obs=obs_opts(family="poisson", scale=1)
)

cholera_sim_data_dec_ur <- simulate_infections(
  R=rt_dec,
  initial_infections=75,
  generation_time=generation_time_opts(cholera_gen_time),
  delays=delay_opts(fix_dist(combined_delay_cholera)),
  obs=obs_opts(family="poisson", scale=0.28)
)

save_latest(cholera_sim_data_dec, here("data"), "cholera_sim_data_dec")
save_latest(cholera_sim_data_dec_ur, here("data"), "cholera_sim_data_dec_ur")

