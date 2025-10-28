library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

disease <- "covid"
d <- delays[[disease]]

############### SCENARIOS #################

#### Increasing Rt ####

## Loading data ##

sim_data_inc <- read_latest(here("data"), paste0(disease, "_sim_data_inc"))
sim_data_inc_ur <- read_latest(here("data"), paste0(disease, "_sim_data_inc_ur"))

# In required format for EpiNow2

sim_data_inc_cases <- sim_data_inc |> filter(variable=="reported_cases")
sim_data_inc_cases <- sim_data_inc_cases |>
  select(date, value) |>
  rename(confirm=value)

sim_data_inc_cases_ur <- sim_data_inc_ur |> filter(variable=="reported_cases")
sim_data_inc_cases_ur <- sim_data_inc_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

## Run scenario 9 - rt_opts=latest, under-reporting=no ##
 
 res_covid <- sim_scenarios(case_data=sim_data_inc_cases,
                            gt,
                            gen_mean=d$gen[["mean"]],
                            gen_sd=d$gen[["sd"]],
                            gen_max = d$gen[["max"]],
                            inc_mean = d$inc[["mean"]],
                            inc_sd = d$inc[["sd"]],
                            inc_max = d$inc[["max"]],
                            rep_mean = d$rep[["mean"]],
                            rep_sd = d$rep[["sd"]],
                            rep_max = d$rep[["max"]],
                            freq_fc=4,
                            weeks_inc=12,
                            rt_opts_choice="latest",
                            obs_scale=1)
 
 save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"scen9_samples", gt))
 save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"scen9_id", gt))
 save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"scen9_R", gt))
 save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"scen9_summary", gt))
 save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"scen9_warnings", gt))
 
 ## Run scenario 10 - rt_opts=latest, under-reporting=yes ##
 
   res_covid <- sim_scenarios(case_data=sim_data_inc_cases_ur,
                            gt,
                            gen_mean=d$gen[["mean"]],
                            gen_sd=d$gen[["sd"]],
                            gen_max = d$gen["max"],
                            inc_mean = d$inc["mean"],
                            inc_sd = d$inc["sd"],
                            inc_max = d$inc["max"],
                            rep_mean = d$rep["mean"],
                            rep_sd = d$rep["sd"],
                            rep_max = d$rep["max"],
                            freq_fc=4,
                            weeks_inc=12,
                            rt_opts_choice="latest",
                            obs_scale=0.3)
 
   save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"scen10_samples", gt))
   save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"scen10_id", gt))
   save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"scen10_R", gt))
   save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"scen10_summary", gt))
   save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"scen10_warnings", gt))

## Run scenario 11 - rt_opts=project, under-reporting=no ##

  res_covid <- sim_scenarios(case_data=sim_data_inc_cases,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"scen11_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"scen11_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"scen11_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"scen11_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"scen11_warnings", gt))

## Run scenario 12 - rt_opts=project, under-reporting=yes ##

  res_covid <- sim_scenarios(case_data=sim_data_inc_cases_ur,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=0.3)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"scen12_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"scen12_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"scen12_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"scen12_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"scen12_warnings", gt)) 
  
  #### Decreasing Rt ####
  
  ## Loading data ##

  sim_data_dec <- read_latest(here("data"), paste0(disease, "_sim_data_dec"))
  sim_data_dec_ur <- read_latest(here("data"), paste0(disease, "_sim_data_dec_ur"))
  
# In required format for EpiNow2

sim_data_dec_cases <-  sim_data_dec |> filter(variable=="reported_cases")
sim_data_dec_cases <- sim_data_dec_cases |>
  select(date, value) |>
  rename(confirm=value)

sim_data_dec_cases_ur <- sim_data_dec_ur |> filter(variable=="reported_cases")
sim_data_dec_cases_ur <- sim_data_dec_cases_ur |>
  select(date, value) |>
  rename(confirm=value)

## Run scenario 13 - rt_opts=latest, under-reporting=no ##

  res_disease <- sim_scenarios(case_data=sim_data_dec_cases,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="latest",
                             obs_scale=1)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"scen13_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"scen13_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"scen13_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"scen13_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"scen13_warnings", gt))

## Run scenario 14 - rt_opts=latest, under-reporting=yes ##

  res_disease <- sim_scenarios(case_data=sim_data_dec_cases_ur,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="latest",
                             obs_scale=0.3)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"scen14_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"scen14_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"scen14_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"scen14_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"scen14_warnings", gt))

## Run scenario 15 - rt_opts=project, under-reporting=no ##

  res_disease <- sim_scenarios(case_data=sim_data_dec_cases,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=1)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"scen15_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"scen15_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"scen15_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"scen15_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"scen15_warnings", gt))

## Run scenario 16 - rt_opts=project, under-reporting=yes ##

  res_disease <- sim_scenarios(case_data=sim_data_dec_cases_ur,
                             gt,
                             gen_mean=d$gen[["mean"]],
                             gen_sd=d$gen[["sd"]],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc=4,
                             weeks_inc=12,
                             rt_opts_choice="project",
                             obs_scale=0.3)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"scen16_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"scen16_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"scen16_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"scen16_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"scen16_warnings", gt)) 
