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

ebola_confirmed_linelist <- read_xlsx(here("data", "ebola_linelist.xlsx"), "lab-confirmed database")

# Formating for EpiNow2

ebola_confirmed <- incidence(ebola_confirmed_linelist,
                             date_index="Date of sample tested",
                             interval="day")

ebola_confirmed <- ebola_confirmed |>
  select(date_index, count) |>
  rename(date=date_index, confirm=count)

# Assuming no data reported on missing days
extra_dates <- data.frame(date=seq(ebola_confirmed$date[1], ebola_confirmed$date[nrow(ebola_confirmed)], by="day"))
ebola_confirmed <- right_join(ebola_confirmed, extra_dates, by="date")

ebola_confirmed$confirm[is.na(ebola_confirmed$confirm)] <- 0

# Make sure days are in order
ebola_confirmed <- ebola_confirmed |>
  arrange(date)

## Param values

# Generation time
gen_mean_mean=16.2
gen_mean_sd=0.16
gen_sd_mean=9.40
gen_sd_sd=0.11
gen_max=50

# Incubation period
inc_mean_mean=11.4
inc_mean_sd=0.14
inc_sd_mean=8.1
inc_sd_sd=0.1
inc_max=60

# Reporting delay
rep_mean_mean=1
rep_mean_sd=0.1
rep_sd_mean=1
rep_sd_sd=0.1
rep_max=0 # Setting this to zero as no reporting delay in the data

############### SCENARIOS #################

## Run scenario 25 - rt_opts=latest, under-reporting=no, weightprior=TRUE ##

res_ebola <- sim_weightprior(case_data=ebola_confirmed,
                           gt,
                           gen_mean_mean=gen_mean_mean,
                           gen_mean_sd=gen_mean_sd,
                           gen_sd_mean=gen_sd_mean,
                           gen_sd_sd=gen_sd_sd,
                           gen_max=gen_max,
                           inc_mean_mean=inc_mean_mean,
                           inc_mean_sd=inc_mean_sd,
                           inc_sd_mean=inc_sd_mean,
                           inc_sd_sd=inc_sd_sd,
                           inc_max=inc_max,
                           rep_mean_mean=rep_mean_mean,
                           rep_mean_sd=rep_mean_sd,
                           rep_sd_mean=rep_sd_mean,
                           rep_sd_sd=rep_sd_sd,
                           rep_max=rep_max,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           weight_prior=TRUE,
                           obs_scale=1)

save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen25_samples", gt))
save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen25_id", gt))
save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen25_R", gt))
save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen25_summary", gt))
save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen25_warnings", gt))

## Run scenario 26 - rt_opts=latest, under-reporting=yes, weightprior=TRUE ##

  res_ebola <- sim_weightprior(case_data=ebola_confirmed,
                           gt,
                           gen_mean_mean=gen_mean_mean,
                           gen_mean_sd=gen_mean_sd,
                           gen_sd_mean=gen_sd_mean,
                           gen_sd_sd=gen_sd_sd,
                           gen_max=gen_max,
                           inc_mean_mean=inc_mean_mean,
                           inc_mean_sd=inc_mean_sd,
                           inc_sd_mean=inc_sd_mean,
                           inc_sd_sd=inc_sd_sd,
                           inc_max=inc_max,
                           rep_mean_mean=rep_mean_mean,
                           rep_mean_sd=rep_mean_sd,
                           rep_sd_mean=rep_sd_mean,
                           rep_sd_sd=rep_sd_sd,
                           rep_max=rep_max,
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice="latest",
                           weight_prior=TRUE,
                           obs_scale=0.83)

  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen26_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen26_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen26_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen26_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen26_warnings", gt))

## Run scenario 27 - rt_opts=project, under-reporting=no, weightprior=TRUE ##

  res_ebola <- sim_weightprior(case_data=ebola_confirmed,
                             gt,
                             gen_mean_mean=gen_mean_mean,
                             gen_mean_sd=gen_mean_sd,
                             gen_sd_mean=gen_sd_mean,
                             gen_sd_sd=gen_sd_sd,
                             gen_max=gen_max,
                             inc_mean_mean=inc_mean_mean,
                             inc_mean_sd=inc_mean_sd,
                             inc_sd_mean=inc_sd_mean,
                             inc_sd_sd=inc_sd_sd,
                             inc_max=inc_max,
                             rep_mean_mean=rep_mean_mean,
                             rep_mean_sd=rep_mean_sd,
                             rep_sd_mean=rep_sd_mean,
                             rep_sd_sd=rep_sd_sd,
                             rep_max=rep_max,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             weight_prior=TRUE,
                             obs_scale=1)

  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen27_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen27_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen27_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen27_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen27_warnings", gt))

## Run scenario 28 - rt_opts=project, under-reporting=yes, weightprior=TRUE ##

  res_ebola <- sim_weightprior(case_data=ebola_confirmed,
                             gt,
                             gen_mean_mean=gen_mean_mean,
                             gen_mean_sd=gen_mean_sd,
                             gen_sd_mean=gen_sd_mean,
                             gen_sd_sd=gen_sd_sd,
                             gen_max=gen_max,
                             inc_mean_mean=inc_mean_mean,
                             inc_mean_sd=inc_mean_sd,
                             inc_sd_mean=inc_sd_mean,
                             inc_sd_sd=inc_sd_sd,
                             inc_max=inc_max,
                             rep_mean_mean=rep_mean_mean,
                             rep_mean_sd=rep_mean_sd,
                             rep_sd_mean=rep_sd_mean,
                             rep_sd_sd=rep_sd_sd,
                             rep_max=rep_max,
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             weight_prior=TRUE,
                             obs_scale=0.83)

  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen28_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen28_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen28_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen28_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen28_warnings", gt))
  
  ## Run scenario 29 - rt_opts=latest, under-reporting=no, weightprior=FALSE ##
  
  res_ebola <- sim_weightprior(case_data=ebola_confirmed,
                               gt,
                               gen_mean_mean=gen_mean_mean,
                               gen_mean_sd=gen_mean_sd,
                               gen_sd_mean=gen_sd_mean,
                               gen_sd_sd=gen_sd_sd,
                               gen_max=gen_max,
                               inc_mean_mean=inc_mean_mean,
                               inc_mean_sd=inc_mean_sd,
                               inc_sd_mean=inc_sd_mean,
                               inc_sd_sd=inc_sd_sd,
                               inc_max=inc_max,
                               rep_mean_mean=rep_mean_mean,
                               rep_mean_sd=rep_mean_sd,
                               rep_sd_mean=rep_sd_mean,
                               rep_sd_sd=rep_sd_sd,
                               rep_max=rep_max,
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="latest",
                               weight_prior=FALSE,
                               obs_scale=1)
  
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen29_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen29_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen29_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen29_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen29_warnings", gt))
  
  ## Run scenario 30 - rt_opts=latest, under-reporting=yes, weightprior=FALSE ##
  
  res_ebola <- sim_weightprior(case_data=ebola_confirmed,
                               gt,
                               gen_mean_mean=gen_mean_mean,
                               gen_mean_sd=gen_mean_sd,
                               gen_sd_mean=gen_sd_mean,
                               gen_sd_sd=gen_sd_sd,
                               gen_max=gen_max,
                               inc_mean_mean=inc_mean_mean,
                               inc_mean_sd=inc_mean_sd,
                               inc_sd_mean=inc_sd_mean,
                               inc_sd_sd=inc_sd_sd,
                               inc_max=inc_max,
                               rep_mean_mean=rep_mean_mean,
                               rep_mean_sd=rep_mean_sd,
                               rep_sd_mean=rep_sd_mean,
                               rep_sd_sd=rep_sd_sd,
                               rep_max=rep_max,
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="latest",
                               weight_prior=FALSE,
                               obs_scale=0.83)
  
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen30_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen30_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen30_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen30_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen30_warnings", gt))
  
  ## Run scenario 31 - rt_opts=project, under-reporting=no, weightprior=FALSE ##
  
  res_ebola <- sim_weightprior(case_data=ebola_confirmed,
                               gt,
                               gen_mean_mean=gen_mean_mean,
                               gen_mean_sd=gen_mean_sd,
                               gen_sd_mean=gen_sd_mean,
                               gen_sd_sd=gen_sd_sd,
                               gen_max=gen_max,
                               inc_mean_mean=inc_mean_mean,
                               inc_mean_sd=inc_mean_sd,
                               inc_sd_mean=inc_sd_mean,
                               inc_sd_sd=inc_sd_sd,
                               inc_max=inc_max,
                               rep_mean_mean=rep_mean_mean,
                               rep_mean_sd=rep_mean_sd,
                               rep_sd_mean=rep_sd_mean,
                               rep_sd_sd=rep_sd_sd,
                               rep_max=rep_max,
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="project",
                               weight_prior=FALSE,
                               obs_scale=1)
  
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen31_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen31_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen31_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen31_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen31_warnings", gt))
  
  ## Run scenario 32 - rt_opts=project, under-reporting=yes, weightprior=FALSE ##
  
  res_ebola <- sim_weightprior(case_data=ebola_confirmed,
                               gt,
                               gen_mean_mean=gen_mean_mean,
                               gen_mean_sd=gen_mean_sd,
                               gen_sd_mean=gen_sd_mean,
                               gen_sd_sd=gen_sd_sd,
                               gen_max=gen_max,
                               inc_mean_mean=inc_mean_mean,
                               inc_mean_sd=inc_mean_sd,
                               inc_sd_mean=inc_sd_mean,
                               inc_sd_sd=inc_sd_sd,
                               inc_max=inc_max,
                               rep_mean_mean=rep_mean_mean,
                               rep_mean_sd=rep_mean_sd,
                               rep_sd_mean=rep_sd_mean,
                               rep_sd_sd=rep_sd_sd,
                               rep_max=rep_max,
                               freq_fc=4,
                               weeks_inc=12,
                               rt_opts_choice="project",
                               weight_prior=FALSE,
                               obs_scale=0.83)
  
  save_latest(res_ebola[[1]], here("results"), paste0("res_ebolascen32_samples", gt))
  save_latest(res_ebola[[2]], here("results"), paste0("res_ebolascen32_id", gt))
  save_latest(res_ebola[[3]], here("results"), paste0("res_ebolascen32_R", gt))
  save_latest(res_ebola[[4]], here("results"), paste0("res_ebolascen32_summary", gt))
  save_latest(res_ebola[[5]], here("results"), paste0("res_ebolascen32_warnings", gt))
  
  

