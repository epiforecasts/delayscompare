source("~/delayscompare/scripts/packages.R")

source(here("R", "scenario_loop.R"))
source(here("scripts", "data_processing.R"))

## Example data ##

res_example <- sim_scenarios(case_data=example_confirmed,
                             gen_mean=3.6,
                             gen_sd=3.1,
                             inc_mean=1.6,
                             inc_sd=0.42,
                             reporting_delay=example_reporting_delay,
                             timesplit=10)

#saveRDS(res_example[[1]], here("results", paste0("res_example", Sys.Date(), ".RDS")))
#saveRDS(res_example[[2]], here("results", paste0("res_example_id", Sys.Date(), ".RDS")))

## Ebola ##

res_ebola <- sim_scenarios(case_data=ebola_confirmed,
                           gen_mean=16.2,
                           gen_sd=9.40, # from Park et al. 2019
                           inc_mean=11.4,
                           inc_sd=8.1, # from Aylward et al. 2014 
                           reporting_delay=Lognormal(mean=ebola_reporting_delay$mean_mean, sd=ebola_reporting_delay$sd_mean, max=48),
                           timesplit=2)

saveRDS(res_ebola[[1]], here("results", paste0("res_example", Sys.Date(), ".RDS")))
saveRDS(res_ebola[[2]], here("results", paste0("res_ebola_id", Sys.Date(), ".RDS")))



## COVID-19 ##

res_covid <- sim_scenarios(case_data=covid_eng,
                           gen_mean=3.6,
                           gen_sd=3.1, # from Sherratt et al. 2021 - surveillance paper
                           inc_mean=5.2,
                           inc_sd=1.52, # from Sherratt et al. 2021 - surveillance paper
                           reporting_delay=dist_spec(mean=4.4, sd=5.6, max=30, distribution="lognormal"), # MAXIMUM IS JUST A PLACEHOLDER # from Sherratt et al. 2021 - surveillance paper
                           timesplit=10)

#saveRDS(res_covid[[1]], here("results", paste0("res_covid", Sys.Date(), ".RDS")))
#saveRDS(res_covid[[2]], here("results", paste0("res_covid_id", Sys.Date(), ".RDS")))
