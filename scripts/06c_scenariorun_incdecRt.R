library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

inc <- as.numeric(var[2])
print(inc)

rt_opts <- var[3] # "latest" or "project"
print(rt_opts)

disease <- var[4] # "cholera", "covid" or "ebola"
print(disease)

d <- delays[[disease]]

############### SCENARIOS #################

#### Increasing Rt ####

## Loading data ##

sim_data_inc <- read_latest(here("data"), paste0(disease, "_sim_data_inc"))

# In required format for EpiNow2

sim_data_inc_cases <- sim_data_inc |> filter(variable=="reported_cases")
sim_data_inc_cases <- sim_data_inc_cases |>
  select(date, value) |>
  rename(confirm=value)

## Run scenario 9 - under-reporting=no ##
 
 res_disease <- sim_scenarios(case_data=sim_data_inc_cases,
                            gt,
                            inc,
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
 
 save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"_inc_", rt_opts, "_samples", gt, inc))
 save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"_inc_", rt_opts, "_id", gt, inc))
 save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"_inc_", rt_opts, "_R", gt, inc))
 save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"_inc_", rt_opts, "_summary", gt, inc))
 save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"_inc_", rt_opts, "_warnings", gt, inc))
 save_latest(res_disease[[6]], here("results"), paste0("res_",disease,"_inc_", rt_opts, "_timing", gt, inc))
 
#### Decreasing Rt ####
  
  ## Loading data ##

sim_data_dec <- read_latest(here("data"), paste0(disease, "_sim_data_dec"))

# In required format for EpiNow2

sim_data_dec_cases <-  sim_data_dec |> filter(variable=="reported_cases")
sim_data_dec_cases <- sim_data_dec_cases |>
  select(date, value) |>
  rename(confirm=value)

## Run scenario 13 - rt_opts=latest, under-reporting=no ##

  res_disease <- sim_scenarios(case_data=sim_data_dec_cases,
                             gt,
                             inc,
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
  
save_latest(res_disease[[1]], here("results"), paste0("res_",disease,"_dec_", rt_opts, "_samples", gt,inc))
save_latest(res_disease[[2]], here("results"), paste0("res_",disease,"_dec_", rt_opts, "_id", gt,inc))
save_latest(res_disease[[3]], here("results"), paste0("res_",disease,"_dec_", rt_opts, "_R", gt,inc))
save_latest(res_disease[[4]], here("results"), paste0("res_",disease,"_dec_", rt_opts, "_summary", gt,inc))
save_latest(res_disease[[5]], here("results"), paste0("res_",disease,"_dec_", rt_opts, "_warnings", gt,inc))
save_latest(res_disease[[6]], here("results"), paste0("res_",disease,"_dec_", rt_opts, "_timing", gt,inc))