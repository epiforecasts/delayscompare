

## Collect Rt trajectories and simulated data in a list ##

## Get correct start and end date for disease

startdate <- as.Date("2020-06-19") # Same start date as data
enddate <- as.Date("2020-06-19") + 6*4*7 + 14 # Long enough time horizon to have six forecast time points with forecasts every 4 weeks, + two weeks data at the end for forecast

rt_traj <- list()

# Scenarios 1-4 - const Rt - low 
for(i in 1:4){
  rt_traj[[i]] <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                             R=0.8)}

# Scenarios 5-8 - const Rt - high
for(i in 5:8){
  rt_traj[[i]] <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                             R=1.2)}

# Scenarios 9-12 - increasing Rt 
for(i in 9:12){
  rt_traj[[i]] <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                             R=seq(from=0.8, to=1.2, length.out=enddate-(startdate-1)))}

# Scenarios 13-16 - decreasing Rt
for(i in 13:16){
  rt_traj[[i]] <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                             R=rev(seq(from=0.8, to=1.2, length.out=enddate-(startdate-1))))}

# Scenarios 17-21 - sine Rt
for(i in 17:20){
  rt_traj[[i]] <- read_latest("data", "covidRt_sine")}

sim_data <- list()

# Scenario 1 - Rt const low, rt_opts=latest, under-reporting=no
sim_data[[1]] <- read_latest(here("data"), "covid_sim_data_const_low")
# Scenario 2 - Rt const low, rt_opts=latest, under-reporting=yes
sim_data[[2]] <- read_latest(here("data"), "covid_sim_data_const_low_ur")
# Scenario 3 - Rt const low, rt_opts=project, ur=no
sim_data[[3]] <- read_latest(here("data"), "covid_sim_data_const_low")
# Scenario 4 - Rt const low, rt_opts=project, ur=yes
sim_data[[4]] <- read_latest(here("data"), "covid_sim_data_const_low_ur")

# Scenario 5 - Rt const high, rt_opts=latest, under-reporting=no
sim_data[[5]] <- read_latest(here("data"), "covid_sim_data_const_hi")
# Scenario 6 - Rt const high, rt_opts=latest, under-reporting=yes
sim_data[[6]] <- read_latest(here("data"), "covid_sim_data_const_hi_ur")
# Scenario 7 - Rt const high, rt_opts=project, under-reporting=no 
sim_data[[7]] <- read_latest(here("data"), "covid_sim_data_const_hi")
# Scenario 8 - Rt const high, rt_opts=project, under-reporting=yes
sim_data[[8]] <- read_latest(here("data"), "covid_sim_data_const_hi_ur")

# Scenario 9 - Rt inc, rt_opts=latest, under-reporting=no
sim_data[[9]] <- read_latest(here("data"), "covid_sim_data_inc")
# Scenario 10 - Rt inc, rt_opts=latest, under-reporting=yes
sim_data[[10]] <- read_latest(here("data"), "covid_sim_data_inc_ur")
# Scenario 11 - Rt inc, rt_opts=project, under-reporting=no
sim_data[[11]] <- read_latest(here("data"), "covid_sim_data_inc")
# Scenario 12 - Rt inc, rt_opts=project, under-reporting=yes
sim_data[[12]] <- read_latest(here("data"), "covid_sim_data_inc_ur")

# Scenario 13 - Rt dec, rt_opts=latest, under-reporting=no
sim_data[[13]] <- read_latest(here("data"), "covid_sim_data_dec")
# Scenario 14 - Rt dec, rt_opts=latest, under-reporting=yes
sim_data[[14]] <- read_latest(here("data"), "covid_sim_data_dec_ur")
# Scenario 15 - Rt dec, rt_opts=project, under-reporting=no 
sim_data[[15]] <- read_latest(here("data"), "covid_sim_data_dec")
# Scenario 16 - Rt dec, rt_opts=project, under-reporting=yes
sim_data[[16]] <- read_latest(here("data"), "covid_sim_data_dec_ur")

# Scenario 17 - Rt sine, rt_opts=latest, under-reporting=no
sim_data[[17]] <- read_latest(here("data"), "covid_sim_data_sine")
# Scenario 18 - Rt sine, rt_opts=latest, under-reporting=yes
sim_data[[18]] <- read_latest(here("data"), "covid_sim_data_sine_ur")
# Scenario 19 - Rt sine, rt_opts=project, under-reporting=no
sim_data[[19]] <- read_latest(here("data"), "covid_sim_data_sine")
# Scenario 20 - Rt sine, rt_opts=project, under-reporting=yes
sim_data[[20]] <- read_latest(here("data"), "covid_sim_data_sine_ur")

## Save ##

save_latest(rt_traj, here("data"), "rt_traj_list_covid")
save_latest(sim_data, here("data"), "sim_data_list_covid")

