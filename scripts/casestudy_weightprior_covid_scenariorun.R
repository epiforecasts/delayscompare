library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

## Load data ##

covid_eng <- read.csv(here("data", "newCasesBySpecimenDate_nation_2021.csv")) |>
  filter(area_name=="England", metric=="newCasesBySpecimenDate")

## Formatting data for EpiNow2
covid_eng <- covid_eng |> 
  select(date, value) |>
  rename(confirm=value)

covid_eng$date <- as.Date(covid_eng$date, "%Y-%m-%d" )

## Delta wave only 
covid_eng <- covid_eng |> filter(date >= "2021-06-01" & date < "2021-12-01")

############### SCENARIOS #################

covid_rep_params <- get_parameters(fix_dist(covid_reporting_delay))

## Run scenario 25 - rt_opts=latest, under-reporting=no, weight_prior=FALSE ##

res_covid <- sim_weightprior(case_data=covid_eng,
                           gt,
                           gen_mean=3.6,
                           gen_sd=3.1, # from Park et al. 2019
                           gen_max=30,
                           inc_mean=5.2,
                           inc_sd=1.52, # from Aylward et al. 2014 
                           inc_max=30,
                           rep_meanlog=covid_rep_params$meanlog,
                           rep_sdlog=covid_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=1)

save_latest(res_covid[[2]], here("results"), paste0("res_covidscen25_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covidscen25_warnings", gt))

## Saving samples only ##
save_latest(res_covid[[1]], here("results"), paste0("res_covidscen25_samples", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covidscen25_R", gt)) 

## Run scenario 26 - rt_opts=latest, under-reporting=yes, weight_prior=FALSE ##

  res_covid <- sim_weightprior(case_data=covid_eng,
                           gt,
                           gen_mean=3.6,
                           gen_sd=3.1, 
                           gen_max=30,
                           inc_mean=5.2,
                           inc_sd=1.52, # from Aylward et al. 2014 
                           inc_max=30,
                           rep_meanlog=covid_rep_params$meanlog,
                           rep_sdlog=covid_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=0.3)

save_latest(res_covid[[2]], here("results"), paste0("res_covidscen26_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covidscen26_warnings", gt))

## Saving samples only ##
save_latest(res_covid[[1]], here("results"), paste0("res_covidscen26_samples", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covidscen26_R", gt)) 

## Run scenario 27 - rt_opts=project, under-reporting=no, weight_prior=FALSE ##

  res_covid <- sim_weightprior(case_data=covid_eng,
                             gt,
                             gen_mean=3.6,
                             gen_sd=3.1, 
                             gen_max=30,
                             inc_mean=5.2,
                             inc_sd=1.52, # from Aylward et al. 2014 
                             inc_max=30,
                             rep_meanlog=covid_rep_params$meanlog,
                             rep_sdlog=covid_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen27_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen27_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen27_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen27_R", gt)) 

## Run scenario 28 - rt_opts=project, under-reporting=yes, weight_prior=FALSE ##

  res_covid <- sim_weightprior(case_data=covid_eng,
                             gt,
                             gen_mean=3.6,
                             gen_sd=3.1, 
                             gen_max=30,
                             inc_mean=5.2,
                             inc_sd=1.52, # from Aylward et al. 2014 
                             inc_max=30,
                             rep_meanlog=covid_rep_params$meanlog,
                             rep_sdlog=covid_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=0.3)
  
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen28_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen28_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen28_samples", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen28_R", gt)) 
  
