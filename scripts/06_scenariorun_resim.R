library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("scripts", "05_simulate_data.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

disease <- "cholera"
rt_opts <- "latest"

## Loading data ##

sim_data <- read_latest(here("data"), paste0(disease, "_sim_data"))
d <- delays[[disease]]

# In required format for EpiNow2

sim_data_cases <- sim_data |> filter(variable=="reported_cases")
sim_data_cases <- sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

## Run scenario 1 - rt_opts=latest, under-reporting=yes ##

res_disease <- sim_scenarios(case_data=sim_data_cases,
                             gt,
                             gen_mean = d$gen["mean"],
                             gen_sd = d$gen["sd"],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc = 4,
                             weeks_inc = 12,
                             rt_opts_choice = rt_opts,
                             obs_scale = d$underreport)

save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "_sim_latest_samples", gt))
save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "_sim_latest_id", gt))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "_sim_latest_R", gt))
save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "_sim_latest_summary", gt))
save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "_sim_latest_warnings", gt))

## Run scenario 2 - rt_opts=project, under-reporting=yes ##

res_disease <- sim_scenarios(case_data=sim_data_cases,
                             gt,
                             gen_mean = d$gen["mean"],
                             gen_sd = d$gen["sd"],
                             gen_max = d$gen["max"],
                             inc_mean = d$inc["mean"],
                             inc_sd = d$inc["sd"],
                             inc_max = d$inc["max"],
                             rep_mean = d$rep["mean"],
                             rep_sd = d$rep["sd"],
                             rep_max = d$rep["max"],
                             freq_fc = 4,
                             weeks_inc = 12,
                             rt_opts_choice = rt_opts,
                             obs_scale = d$underreport)
save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "sim_project_samples", gt))
save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "sim_project_id", gt))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "sim_project_R", gt))
save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "sim_project_summary", gt))
save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "sim_project_warnings", gt))