library(here)

source(here("scripts", "01_packages.R"))
#source(here("R", "scenario_loop.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "lshtm_theme.R"))

library(scoringutils)
library(viridis)
library(RColorBrewer)

####################
#### EBOLA-LIKE ####
####################

## Loading data ##

ebola_sim_data <- read_latest(here("data"), "ebola_sim_data")
rt_ebola <- readRDS(here("data", "rt_ebola.rds"))

## Loading results ##

ebola_samples <- list()
res_ebola_id <- list()
ebola_rt_samples <- list()

for(gt in c(1:6)){
  ebola_samples[[gt]] <- read_latest(here("results"), paste0("res_ebola_samples", gt))
  res_ebola_id[[gt]] <- read_latest(here("results"), paste0("res_ebola_id", gt))
  # ebola_rt_samples[[gt]] <- read_latest(here("results"), paste0("res_ebola_R", gt))
  # Need to add extra id for generation time #
  ebola_samples[[gt]]$gt <- gt
  res_ebola_id[[gt]]$gt <- gt
  #ebola_rt_samples[[gt]] <- gt
}

ebola_samples <- bind_rows(ebola_samples)
res_ebola_id <- bind_rows(res_ebola_id)
#ebola_rt_samples <- bind_rows(ebola_rt_samples)

## CRPS ##

plot_crps_ebola <- plotcrps(ebola_samples, 
                            res_ebola_id, 
                            ebola_sim_data,
                            forecast_freq=4)

ggsave(here("results", paste0("plot_crps_ebola.png")), plot_crps_ebola, width=40, height=15, units="cm")

## Rt estimates

plot_crps_rt_ebola <- plotcrps_rt(ebola_rt_samples,
                               res_ebola_id,
                               rt_ebola,
                               forecast_freq=4)

ggsave(here("results", paste0("plot_rt_ebola.png")), plot_crps_rt_ebola, width=40, height=15, units="cm")

## plotrankings 

plot_rankings_ebola <- plotrankings(ebola_samples,
                                    res_ebola_id,
                                    res_sim_data)

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

plot_correct <- plotcorrect(ebola_samples,
            res_ebola_id,
            ebola_sim_data,
            rt_ebola)

ggsave(here("results", paste0("plot_ebola_correct.png")), plot_correct, width=12, height=7.65, units="in")

plot_rankings <- plotrankings(ebola_samples,
                              res_ebola_id,
                              ebola_sim_data)

ggsave(here("results", paste0("plot_ebola_rankings.png")), plot_rankings)

######################
#### CHOLERA-LIKE ####
######################

## Loading results ##

cholera_samples <- read_latest(here("results"), "res_cholera_samples")
res_cholera_id <- read_latest(here("results"), "res_cholera_id")
cholera_sim_data <- read_latest(here("data"), "cholera_sim_data")
rt_cholera <- readRDS(here("data", "rt_cholera.rds"))
cholera_rt_samples <- read_latest(here("results"), "res_cholera_R")

## CRPS ##

plot_crps_cholera <- plotcrps(cholera_samples, 
                            res_cholera_id, 
                            cholera_sim_data,
                            forecast_freq=4)

## Rt estimates

plot_crps_rt_cholera <- plotcrps_rt(cholera_rt_samples,
                                  res_cholera_id,
                                  rt_cholera,
                                  forecast_freq=4)

## Forecasts by parameter ##

plot_forecasts_cholera <- plotforecasts(cholera_samples,
                                      res_cholera_id,
                                      cholera_sim_data)

## Forecasts by timepoint ##

cholera_samples <- cholera_samples |> 
  # add info
  left_join(res_cholera_id, by="result_list")

for(i in c(1:max(cholera_samples$timepoint))){

  cholera_samples_timepoint <- cholera_samples |> filter(timepoint==i)
  
  plots_timepoint_cholera <- plotbytime(cholera_samples_timepoint,
                                    res_cholera_id,
                                    cholera_sim_data,
                                    disease="cholera")
  ggsave(here("results", paste0("plots_timepoint_cholera", i, ".png")), plots_timepoint_cholera, width=12, height=7.65, units="in")
  rm(plots_timepoint_cholera)
}

# Focussing on forecasts
plots_timepoint_cholera2 <- plotbytime(cholera_samples,
                                      res_cholera_id,
                                      cholera_sim_data,
                                      disease="cholera",
                                      forecastonly=TRUE)

## "Correct" parameter forecasts across time horizon ##

plot_correct <- plotcorrect(cholera_samples,
                            res_cholera_id,
                            cholera_sim_data,
                            rt_cholera)

ggsave(here("results", paste0("plot_cholera_correct.png")), plot_correct, width=12, height=7.65, units="in")

plot_rankings <-plotrankings(cholera_samples,
                             res_cholera_id,
                             cholera_sim_data)

ggsave(here("results", paste0("plot_cholera_rankings.png")), plot_rankings)

####################
#### COVID-LIKE ####
####################

## Loading results ##

covid_samples <- read_latest(here("results"), "res_covid_samples")
res_covid_id <- read_latest(here("results"), "res_covid_id")
covid_sim_data <- read_latest(here("data"), "covid_sim_data")
rt_covid <- readRDS(here("data", "rt_covid.rds"))
covid_rt_samples <- read_latest(here("results"), "res_covid_R")

## CRPS ##

plot_crps_covid <- plotcrps(covid_samples, 
                            res_covid_id, 
                            covid_sim_data,
                            forecast_freq=4)

ggsave(here("results", paste0("plot_crps_covid.png")), plot_crps_covid, width=40, height=15, units="cm")
