library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

rt_opts <- var[2]
print(rt_opts)

disease <- var[3]
print(disease)

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
                            rt_opts_choice=rt_opts,
                            obs_scale=1)
 
 save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"inc", rt_opts, "_samples", gt))
 save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"inc", rt_opts, "_id", gt))
 save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"inc", rt_opts, "_R", gt))
 save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"inc", rt_opts, "_summary", gt))
 save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"inc", rt_opts, "_warnings", gt))
 save_latest(res_disease[[6]], here("results"), paste0("res_",disease,"inc", rt_opts, "_timing", gt))
 
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
                             rt_opts_choice=rt_opts,
                             obs_scale=1)
  
  save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"dec", rt_opts, "_samples", gt))
  save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"dec", rt_opts, "_id", gt))
  save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"dec", rt_opts, "_R", gt))
  save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"dec", rt_opts, "_summary", gt))
  save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"dec", rt_opts, "_warnings", gt))
  save_latest(res_disease[[6]], here("results"), paste0("res_",disease,"dec", rt_opts, "_timing", gt))