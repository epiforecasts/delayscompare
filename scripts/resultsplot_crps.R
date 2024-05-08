source("~/delayscompare/scripts/01_packages.R")
source(here("R", "scenario_loop.R"))

library(scoringutils)
library(viridis)
library(RColorBrewer)

####################
#### EBOLA-LIKE ####
####################

## Loading results ##

ebola_samples <- readRDS(here("results", paste0("res_ebola_samples", "2024-05-08", ".rds")))
res_ebola_id <- readRDS(here("results", paste0("res_ebola2_id", "2024-04-24", ".rds")))
ebola_sim_data <- readRDS(here("data", paste0("ebola_sim_data", "2024-04-23", ".rds")))

## CRPS plot ##

plot_crps_ebola <- plotcrps(ebola_samples, 
                            res_ebola_id, 
                            ebola_sim_data)

plot_forecasts_ebola <- plotforecasts(ebola_samples,
                                      res_ebola_id,
                                      ebola_sim_data)
