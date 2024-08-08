

library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

#####################
#### Constant Rt ####
#####################

startdate <- as.Date("2014-06-01")
enddate <- as.Date("2014-10-01")

##### Simulate data ######

rt_constant <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                          R=2)

rt_inc <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                     R=seq(from=0.5, to=2.5, length.out=difftime(enddate, startdate-1)))

rt_dec <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                     R=rev(seq(from=0.5, to=2.5, length.out=difftime(enddate, startdate-1))))

covid_sim_data <- simulate_infections(
  R=rt_inc,
  initial_infections=5,
  generation_time=generation_time_opts(covid_gen_time),
  delays=delay_opts(fix_dist(covid_inc_period + covid_rep_delay)),
  obs=obs_opts(family="poisson", scale=1)
)

save_latest(covid_sim_data, here("data"), "covid_sim_data_inc")

covid_sim_data <- read_latest("data", "covid_sim_data_inc")

# In required format for EpiNow2

covid_sim_data_cases <- covid_sim_data |> filter(variable=="reported_cases")
covid_sim_data_cases <- covid_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

covid_rep_params <- get_parameters(fix_dist(covid_rep_delay))

for(gt in c(6)){

## Run scenarios ##
res_covid <- sim_scenarios(case_data=covid_sim_data_cases,
                           gt,
                           gen_mean=3.6,
                           gen_sd=3.1, # from Sherratt et al. 2021 - surveillance paper
                           gen_max=30,
                           inc_mean=5.2,
                           inc_sd=1.52, # from Sherratt et al. 2021 - surveillance paper
                           inc_max=30,
                           rep_meanlog=covid_rep_params$meanlog,
                           rep_sdlog=covid_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           obs_scale=1)

save_latest(res_covid[[2]], here("results"), paste0("res_inc_covid_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_inc_covid_warnings", gt))

## Saving samples only ##
save_latest(res_covid[[1]], here("results"), paste0("res_inc_covid_samples", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_inc_covid_R", gt)) }

