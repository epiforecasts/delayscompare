source("~/delayscompare/scripts/01_packages.R")
source(here("R", "scenario_loop.R"))

## Load data ##

cholera_sim_data <- readRDS(here("data", paste0("cholera_sim_data", "2024-05-07", ".rds")))

# In required format for EpiNow2

cholera_sim_data_cases <- cholera_sim_data |> filter(variable=="reported_cases")
cholera_sim_data_cases <- cholera_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

## Run scenarios ##

res_cholera <- sim_scenarios(case_data=cholera_sim_data_cases,
                           gen_mean=5,
                           gen_sd=8, # careful in case this changes https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(18)30230-4
                           inc_mean=1.4,
                           inc_sd=1.98, # from Azman et al. 2013
                           rep_meanlog=convert_to_logmean(4.4, 1.88),
                           rep_sdlog=convert_to_logsd(4.4, 1.88), # https://tbiomed.biomedcentral.com/articles/10.1186/s12976-017-0061-x 
                           freq_fc=8,
                           weeks_inc=12,
                           obs_scale=0.28) # https://tbiomed.biomedcentral.com/articles/10.1186/s12976-017-0061-x

saveRDS(res_cholera[[1]], here("results", paste0("res_cholera", Sys.Date(), ".rds")))
saveRDS(res_cholera[[2]], here("results", paste0("res_cholera_id", Sys.Date(), ".rds")))
saveRDS(res_cholera[[3]], here("results", paste0("res_cholera_warnings", Sys.Date(), ".rds")))

## Saving samples only ##

cholera_samples <- data.frame()
for(i in 1:length(res_cholera)){
  samples_scen <- res_cholera[[1]][[i]][res_cholera[[i]]$variable=="infections"] |>
    mutate(model="EpiNow2")

# Add ID
samples_scen$result_list <- i

# Bind to dataframe
cholera_samples <- rbind(cholera_samples, samples_scen)}

cholera_samples <- cholera_samples |>
  rename(prediction=value)

cholera_samples <- cholera_samples |>
  filter(type=="forecast")

saveRDS(cholera_samples, here("results", paste0("res_cholera_samples", Sys.Date(), ".rds")))

