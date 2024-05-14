source("~/delayscompare/scripts/01_packages.R")
source(here("R", "scenario_loop.R"))

## Load data ##

covid_sim_data <- readRDS(here("data", paste0("covid_sim_data", "2024-04-11", ".rds")))

# In required format for EpiNow2

covid_sim_data_cases <- covid_sim_data |> filter(variable=="reported_cases")
covid_sim_data_cases <- covid_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

## Run scenarios ##

res_covid <- sim_scenarios(case_data=covid_sim_data_cases,
                           gen_mean=3.6,
                           gen_sd=3.1, # from Sherratt et al. 2021 - surveillance paper
                           gen_max=30,
                           inc_mean=5.2,
                           inc_sd=1.52, # from Sherratt et al. 2021 - surveillance paper
                           inc_max=30,
                           rep_meanlog=convert_to_logmean(4.4, 5.6),
                           rep_sdlog=convert_to_logsd(4.4, 5.6), # MAXIMUM IS JUST A PLACEHOLDER # from Sherratt et al. 2021 - surveillance paper
                           rep_max=30,
                           freq_fc=16,
                           weeks_inc=12,
                           obs_scale=0.4)

saveRDS(res_covid[[1]], here("results", paste0("res_covid", Sys.Date(), ".rds")))
saveRDS(res_covid[[2]], here("results", paste0("res_covid_id", Sys.Date(), ".rds")))
saveRDS(res_covid[[3]], here("results", paste0("res_covid_warnings", Sys.Date(), ".rds")))

## Saving samples only ##

covid_samples <- lapply(1:length(res_ebola[[1]]), function(i) {
  res_covid[[1]][[i]][$variable=="reported_cases"]
}) |>
  bind_rows(.id = "result_list") |>
  mutate(model = "EpiNow2", result_list = as.integer(result_list)) |>
  rename(prediction=value)

saveRDS(covid_samples, here("results", paste0("res_covid_samples", Sys.Date(), ".rds")))


