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

# Weekly cholera case counts by district
cholera_yem <- read.csv(here("data", "YEM-CHOLERA-EOC-DIS-WEEK-20170424-20200621.csv")) # https://figshare.com/articles/dataset/Weekly_cholera_cases_and_rainfall_by_governorate_and_district_in_Yemen_between_2016_and_2020/24231019?file=42635185

cholera_yem$date_monday <- as.Date(cholera_yem$date_monday)
cholera_yem$date_sunday <- as.Date(cholera_yem$date_sunday)

# National daily count
cholera_yem_tot <- cholera_yem |>
  group_by(date_monday, date_sunday) |>
  summarise(cases=sum(cases)) |>
  ungroup()

# Selecting period of outbreak included last weekly WHO sit rep (1 July 2018) 
cholera_yem_tot <- cholera_yem_tot |>
  filter(date_sunday <="2018-07-01")

# Formatting data for EpiNow2
cholera_yem_tot <- cholera_yem_tot |>
  select(date_sunday, cases) |>
  rename(date=date_sunday, 
         confirm=cases)

cholera_rep_params <- get_parameters(fix_dist(cholera_reporting_delay))

############### SCENARIOS #################

## Run scenario 25 - rt_opts=latest, under-reporting=no, weight_prior=FALSE ##

res_cholera <- sim_weightprior(case_data=cholera_yem_tot,
                           gt,
                           gen_mean=5,
                           gen_sd=8, 
                           gen_max=30,
                           inc_mean=1.4,
                           inc_sd=1.98, 
                           inc_max=30,
                           rep_meanlog=cholera_rep_params$meanlog,
                           rep_sdlog=cholera_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=1)

save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen25_id", gt))
save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen25_warnings", gt))

## Saving samples only ##
save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen25_samples", gt))
save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen25_R", gt)) 

## Run scenario 26 - rt_opts=latest, under-reporting=yes, weight_prior=FALSE ##

  res_cholera <- sim_weightprior(case_data=cholera_yem_tot,
                           gt,
                           gen_mean=5,
                           gen_sd=8, 
                           gen_max=30,
                           inc_mean=1.4,
                           inc_sd=1.98, 
                           inc_max=30,
                           rep_meanlog=cholera_rep_params$meanlog,
                           rep_sdlog=cholera_rep_params$sdlog,
                           rep_max=30,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           obs_scale=0.28)

save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen26_id", gt))
save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen26_warnings", gt))

## Saving samples only ##
save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen26_samples", gt))
save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen26_R", gt)) 

## Run scenario 27 - rt_opts=project, under-reporting=no, weight_prior=FALSE ##

  res_cholera <- sim_weightprior(case_data=cholera_yem_tot,
                             gt,
                             gen_mean=5,
                             gen_sd=8, 
                             gen_max=30,
                             inc_mean=1.4,
                             inc_sd=1.98, 
                             inc_max=30,
                             rep_meanlog=cholera_rep_params$meanlog,
                             rep_sdlog=cholera_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen27_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen27_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen27_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen27_R", gt)) 

## Run scenario 28 - rt_opts=project, under-reporting=yes, weight_prior=FALSE ##

  res_cholera <- sim_weightprior(case_data=cholera_yem_tot,
                             gt,
                             gen_mean=5,
                             gen_sd=8, 
                             gen_max=30,
                             inc_mean=1.4,
                             inc_sd=1.98, 
                             inc_max=30,
                             rep_meanlog=cholera_rep_params$meanlog,
                             rep_sdlog=cholera_rep_params$sdlog,
                             rep_max=30,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=0.28)
  
  save_latest(res_cholera[[2]], here("results"), paste0("res_cholerascen28_id", gt))
  save_latest(res_cholera[[3]], here("results"), paste0("res_cholerascen28_warnings", gt))
  
  ## Saving samples only ##
  save_latest(res_cholera[[1]], here("results"), paste0("res_cholerascen28_samples", gt))
  save_latest(res_cholera[[4]], here("results"), paste0("res_cholerascen28_R", gt)) 
  