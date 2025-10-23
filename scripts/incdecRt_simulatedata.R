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
 
if(exists(paste0(disease, "_rep_delay"))){ # To cope with lack of Ebola reporting delay
rep_delay <- get(paste0(disease, "_rep_delay"))
combined_delay <- inc_period + rep_delay
} else {combined_delay <- inc_period}

##### Simulate data - increasing Rt ######

rt_inc <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                           R=seq(from=0.8, to=1.2, length.out=enddate-(startdate-1)))

sim_data_inc <- simulate_infections(
  R=rt_inc,
  initial_infections=d$init_infs,
  generation_time=generation_time_opts(gen_time),
  delays=delay_opts(fix_dist(combined_delay)),
  obs=obs_opts(family="poisson", scale=1)
)

sim_data_inc_ur <- simulate_infections(
  R=rt_inc,
  initial_infections=d$init_infs,
  generation_time=generation_time_opts(gen_time),
  delays=delay_opts(fix_dist(combined_delay)),
  obs=obs_opts(family="poisson", scale=d$underreport)
)

save_latest(sim_data_inc, here("data"), paste0(disease, "_sim_data_inc"))
save_latest(sim_data_inc_ur, here("data"), paste0(disease, "_sim_data_inc_ur"))

##### Simulate data - high Rt ######

rt_dec <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                     R=rev(seq(from=0.8, to=1.2, length.out=enddate-(startdate-1))))

sim_data_dec <- simulate_infections(
  R=rt_dec,
  initial_infections=d$init_infs,
  generation_time=generation_time_opts(gen_time),
  delays=delay_opts(fix_dist(combined_delay)),
  obs=obs_opts(family="poisson", scale=1)
)

sim_data_dec_ur <- simulate_infections(
  R=rt_dec,
  initial_infections=d$init_infs,
  generation_time=generation_time_opts(gen_time),
  delays=delay_opts(fix_dist(combined_delay)),
  obs=obs_opts(family="poisson", scale=d$underreport)
)

save_latest(sim_data_dec, here("data"), paste0("_sim_data_dec"))
save_latest(sim_data_dec_ur, here("data"), paste0("_sim_data_dec_ur"))
