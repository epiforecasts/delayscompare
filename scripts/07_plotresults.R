source("~/delayscompare/scripts/01_packages.R")
source(here("R", "scenario_loop.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "lshtm_theme.R"))

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
rt_ebola <- readRDS(here("data", "rt_ebola.rds"))

## CRPS ##

plot_crps_ebola <- plotcrps(ebola_samples, 
                            res_ebola_id, 
                            ebola_sim_data)

## Forecasts by parameter ##

plot_forecasts_ebola <- plotforecasts(ebola_samples,
                                      res_ebola_id,
                                      ebola_sim_data)

## Forecasts by timepoint ##

plots_timepoint_ebola <- plotbytime(ebola_samples,
                                    res_ebola_id,
                                    ebola_sim_data,
                                    disease="ebola")

for(i in 1:max(res_ebola_id$timepoint)){
  ggsave(here("results", paste0("plots_timepoint_ebola", i, ".png")), plots_timepoint_ebola[[i]], width=12, height=7.65, units="in")
}

## "Correct" parameter forecasts across time horizon ##

plotcorrect(ebola_samples,
            res_ebola_id,
            ebola_sim_data,
            rt_ebola)

ggsave(here("results", paste0("plot_ebola_correct.png")), plot_correct, width=12, height=7.65, units="in")

######################
#### CHOLERA-LIKE ####
######################

## Loading results ##

cholera_samples <- readRDS(here("results", paste0("res_cholera_samples", "2024-05-08", ".rds")))
res_cholera_id <- readRDS(here("results", paste0("res_cholera_id", "2024-05-08", ".rds")))
cholera_sim_data <- readRDS(here("data", paste0("cholera_sim_data", "2024-05-07", ".rds")))
rt_cholera <- readRDS(here("data", "rt_cholera.rds"))

## CRPS ##

plot_crps_cholera <- plotcrps(cholera_samples, 
                            res_cholera_id, 
                            cholera_sim_data)

## Forecasts by parameter ##

plot_forecasts_cholera <- plotforecasts(cholera_samples,
                                      res_cholera_id,
                                      cholera_sim_data)

## Forecasts by timepoint ##

plots_timepoint_cholera <- plotbytime(cholera_samples,
                                    res_cholera_id,
                                    cholera_sim_data,
                                    disease="cholera")

for(i in 1:max(res_cholera_id$timepoint)){
  ggsave(here("results", paste0("plots_timepoint_cholera", i, ".png")), plots_timepoint_cholera[[i]], width=12, height=7.65, units="in")
}

# Focussing on forecasts
plots_timepoint_cholera2 <- plotbytime(cholera_samples,
                                      res_cholera_id,
                                      cholera_sim_data,
                                      disease="cholera",
                                      forecastonly=TRUE)

## "Correct" parameter forecasts across time horizon ##

plotcorrect(cholera_samples,
            res_cholera_id,
            cholera_sim_data,
            rt_cholera)