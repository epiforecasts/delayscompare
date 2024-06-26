library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load data ##

cholera_sim_data <- read_latest(here("data"), "cholera_sim_data")

# In required format for EpiNow2

cholera_sim_data_cases <- cholera_sim_data |> filter(variable=="reported_cases")
cholera_sim_data_cases <- cholera_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

## Run scenarios ##

res_cholera <- sim_scenarios(case_data=cholera_sim_data_cases,
                           gen_mean=5,
                           gen_sd=8,
                           gen_max=30,# careful in case this changes https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(18)30230-4
                           inc_mean=1.4,
                           inc_sd=1.98,
                           inc_max=30,# from Azman et al. 2013
                           rep_meanlog=convert_to_logmean(4.4, 1.88),
                           rep_sdlog=convert_to_logsd(4.4, 1.88),
                           rep_max=30, # https://tbiomed.biomedcentral.com/articles/10.1186/s12976-017-0061-x 
                           freq_fc=2,
                           weeks_inc=12,
                           obs_scale=0.28) # https://tbiomed.biomedcentral.com/articles/10.1186/s12976-017-0061-x

#save_latest(res_cholera[[1]], here("results"), "res_cholera")
save_latest(res_cholera[[2]], here("results"), "res_cholera_id")
save_latest(res_cholera[[3]], here("results"), "res_cholera_warnings")

## Saving samples only ##
save_latest(res_cholera[[1]], here("results"), "res_cholera_samples")
save_latest(res_cholera[[4]], here("results"), "res_cholera_R")


