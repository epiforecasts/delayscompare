library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

#####################
#### Constant Rt ####
#####################

startdate <- as.Date("2014-05-18") # Same start date as data
enddate <- as.Date("2014-05-18") + 6*4*7 + 14  # Long enough time horizon to have six forecast timepoints with forecasts every 4 weeks

##### Simulate data - increasing Rt ######

rt_inc <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                           R=seq(from=0.8, to=1.2, length.out=enddate-(startdate-1)))

ebola_sim_data_inc <- simulate_infections(
  R=rt_inc,
  initial_infections=50,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(fix_dist(ebola_inc_period)),
  obs=obs_opts(family="poisson", scale=1)
)

ebola_sim_data_inc_ur <- simulate_infections(
  R=rt_inc,
  initial_infections=50,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(fix_dist(ebola_inc_period)),
  obs=obs_opts(family="poisson", scale=0.83)
)

save_latest(ebola_sim_data_inc, here("data"), "ebola_sim_data_inc")
save_latest(ebola_sim_data_inc_ur, here("data"), "ebola_sim_data_inc_ur")

##### Simulate data - high Rt ######

rt_dec <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                     R=rev(seq(from=0.8, to=1.2, length.out=enddate-(startdate-1))))

ebola_sim_data_dec <- simulate_infections(
  R=rt_dec,
  initial_infections=50,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(fix_dist(ebola_inc_period)),
  obs=obs_opts(family="poisson", scale=1)
)

ebola_sim_data_dec_ur <- simulate_infections(
  R=rt_dec,
  initial_infections=50,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(fix_dist(ebola_inc_period)),
  obs=obs_opts(family="poisson", scale=0.83)
)

save_latest(ebola_sim_data_dec, here("data"), "ebola_sim_data_dec")
save_latest(ebola_sim_data_dec_ur, here("data"), "ebola_sim_data_dec_ur")
