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

## Run scenario 25 - rt_opts=latest, under-reporting=no, weight_prior=TRUE ##

res_covid <- sim_weightprior(case_data=covid_eng,
                           gt,
                           gen_mean_mean=3.6,
                           gen_mean_sd=0.7,
                           gen_sd_mean=3.1, 
                           gen_sd_sd=0.8,
                           gen_max=30,
                           inc_mean_mean=5.2,
                           inc_mean_sd=1.1,
                           inc_sd_mean=1.52, 
                           inc_sd_sd=1.1,
                           inc_max=30,
                           rep_mean_mean=4.4,
                           rep_mean_sd=0.04,
                           rep_sd_mean=5.6,
                           rep_sd_sd=0.03,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           weight_prior=TRUE,
                           obs_scale=1)

save_latest(res_covid[[1]], here("results"), paste0("res_covidscen25_samples", gt))
save_latest(res_covid[[2]], here("results"), paste0("res_covidscen25_id", gt))
save_latest(res_covid[[3]], here("results"), paste0("res_covidscen25_R", gt))
save_latest(res_covid[[4]], here("results"), paste0("res_covidscen25_summary", gt))
save_latest(res_covid[[5]], here("results"), paste0("res_covidscen25_warnings", gt))

## Run scenario 26 - rt_opts=latest, under-reporting=yes, weight_prior=TRUE ##

  res_covid <- sim_weightprior(case_data=covid_eng,
                           gt,
                           gen_mean_mean=3.6,
                           gen_mean_sd=0.7,
                           gen_sd_mean=3.1, 
                           gen_sd_sd=0.8,
                           gen_max=30,
                           inc_mean_mean=5.2,
                           inc_mean_sd=1.1,
                           inc_sd_mean=1.52, 
                           inc_sd_sd=1.1,
                           inc_max=30,
                           rep_mean_mean=4.4,
                           rep_mean_sd=0.04,
                           rep_sd_mean=5.6,
                           rep_sd_sd=0.03,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           weight_prior=TRUE,
                           obs_scale=0.3)

  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen26_samples", gt))
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen26_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen26_R", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen26_summary", gt))
  save_latest(res_covid[[5]], here("results"), paste0("res_covidscen26_warnings", gt))
  
## Run scenario 27 - rt_opts=project, under-reporting=no, weight_prior=TRUE ##

  res_covid <- sim_weightprior(case_data=covid_eng,
                             gt,
                             gen_mean_mean=3.6,
                             gen_mean_sd=0.7,
                             gen_sd_mean=3.1, 
                             gen_sd_sd=0.8,
                             gen_max=30,
                             inc_mean_mean=5.2,
                             inc_mean_sd=1.1,
                             inc_sd_mean=1.52, 
                             inc_sd_sd=1.1,
                             inc_max=30,
                             rep_mean_mean=4.4,
                             rep_mean_sd=0.04,
                             rep_sd_mean=5.6,
                             rep_sd_sd=0.03,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             weight_prior=TRUE,
                             obs_scale=1)
  
  
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen27_samples", gt))
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen27_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen27_R", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen27_summary", gt))
  save_latest(res_covid[[5]], here("results"), paste0("res_covidscen27_warnings", gt))
  
## Run scenario 28 - rt_opts=project, under-reporting=yes, weight_prior=TRUE ##

  res_covid <- sim_weightprior(case_data=covid_eng,
                             gt,
                             gen_mean_mean=3.6,
                             gen_mean_sd=0.7,
                             gen_sd_mean=3.1, 
                             gen_sd_sd=0.8,
                             gen_max=30,
                             inc_mean_mean=5.2,
                             inc_mean_sd=1.1,
                             inc_sd_mean=1.52, 
                             inc_sd_sd=1.1,
                             inc_max=30,
                             rep_mean_mean=4.4,
                             rep_mean_sd=0.04,
                             rep_sd_mean=5.6,
                             rep_sd_sd=0.03,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             weight_prior=TRUE,
                             obs_scale=0.3)
  
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen28_samples", gt))
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen28_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen28_R", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen28_summary", gt))
  save_latest(res_covid[[5]], here("results"), paste0("res_covidscen28_warnings", gt))
  
  ## Run scenario 29 - rt_opts=latest, under-reporting=no, weight_prior=FALSE ##
  
  res_covid <- sim_weightprior(case_data=covid_eng,
                               gt,
                               gen_mean_mean=3.6,
                               gen_mean_sd=0.7,
                               gen_sd_mean=3.1, 
                               gen_sd_sd=0.8,
                               gen_max=30,
                               inc_mean_mean=5.2,
                               inc_mean_sd=1.1,
                               inc_sd_mean=1.52, 
                               inc_sd_sd=1.1,
                               inc_max=30,
                               rep_mean_mean=4.4,
                               rep_mean_sd=0.04,
                               rep_sd_mean=5.6,
                               rep_sd_sd=0.03,
                               rep_max=30,
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="latest",
                               weight_prior=FALSE,
                               obs_scale=1)
  
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen29_samples", gt))
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen29_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen29_R", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen29_summary", gt))
  save_latest(res_covid[[5]], here("results"), paste0("res_covidscen29_warnings", gt))
  
  ## Run scenario 30 - rt_opts=latest, under-reporting=yes, weight_prior=FALSE ##
  
  res_covid <- sim_weightprior(case_data=covid_eng,
                               gt,
                               gen_mean_mean=3.6,
                               gen_mean_sd=0.7,
                               gen_sd_mean=3.1, 
                               gen_sd_sd=0.8,
                               gen_max=30,
                               inc_mean_mean=5.2,
                               inc_mean_sd=1.1,
                               inc_sd_mean=1.52, 
                               inc_sd_sd=1.1,
                               inc_max=30,
                               rep_mean_mean=4.4,
                               rep_mean_sd=0.04,
                               rep_sd_mean=5.6,
                               rep_sd_sd=0.03,
                               rep_max=30,
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="latest",
                               weight_prior=FALSE,
                               obs_scale=0.3)
  
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen30_samples", gt))
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen30_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen30_R", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen30_summary", gt))
  save_latest(res_covid[[5]], here("results"), paste0("res_covidscen30_warnings", gt))
  
  ## Run scenario 31 - rt_opts=project, under-reporting=no, weight_prior=FALSE ##
  
  res_covid <- sim_weightprior(case_data=covid_eng,
                               gt,
                               gen_mean_mean=3.6,
                               gen_mean_sd=0.7,
                               gen_sd_mean=3.1, 
                               gen_sd_sd=0.8,
                               gen_max=30,
                               inc_mean_mean=5.2,
                               inc_mean_sd=1.1,
                               inc_sd_mean=1.52, 
                               inc_sd_sd=1.1,
                               inc_max=30,
                               rep_mean_mean=4.4,
                               rep_mean_sd=0.04,
                               rep_sd_mean=5.6,
                               rep_sd_sd=0.03,
                               rep_max=30,
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="project",
                               weight_prior=FALSE,
                               obs_scale=1)
  
  
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen31_samples", gt))
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen31_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen31_R", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen31_summary", gt))
  save_latest(res_covid[[5]], here("results"), paste0("res_covidscen31_warnings", gt))
  
  ## Run scenario 32 - rt_opts=project, under-reporting=yes, weight_prior=FALSE ##
  
  res_covid <- sim_weightprior(case_data=covid_eng,
                               gt,
                               gen_mean_mean=3.6,
                               gen_mean_sd=0.7,
                               gen_sd_mean=3.1, 
                               gen_sd_sd=0.8,
                               gen_max=30,
                               inc_mean_mean=5.2,
                               inc_mean_sd=1.1,
                               inc_sd_mean=1.52, 
                               inc_sd_sd=1.1,
                               inc_max=30,
                               rep_mean_mean=4.4,
                               rep_mean_sd=0.04,
                               rep_sd_mean=5.6,
                               rep_sd_sd=0.03,
                               rep_max=30,
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="project",
                               weight_prior=FALSE,
                               obs_scale=0.3)
  
  save_latest(res_covid[[1]], here("results"), paste0("res_covidscen32_samples", gt))
  save_latest(res_covid[[2]], here("results"), paste0("res_covidscen32_id", gt))
  save_latest(res_covid[[3]], here("results"), paste0("res_covidscen32_R", gt))
  save_latest(res_covid[[4]], here("results"), paste0("res_covidscen32_summary", gt))
  save_latest(res_covid[[5]], here("results"), paste0("res_covidscen32_warnings", gt))
  
  