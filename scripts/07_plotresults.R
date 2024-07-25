library(here)

source(here("scripts", "01_packages.R"))
#source(here("R", "scenario_loop.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "lshtm_theme.R"))

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
ebola_R_est <- list()

for(gt in c(1:6)){
  ebola_samples[[gt]] <- read_latest(here("results"), paste0("res_const_ebola_samples", gt)) |>
    filter(type=='forecast')
  res_ebola_id[[gt]] <- read_latest(here("results"), paste0("res_const_ebola_id", gt))
  ebola_rt_samples[[gt]] <- read_latest(here("results"), paste0("res_const_ebola_R", gt))
  #ebola_R_est[[gt]] <- read_latest(here("results"), paste0("res_const_ebola_R_est", gt))
  # Need to add extra id for generation time #
  ebola_samples[[gt]]$gt <- gt
  res_ebola_id[[gt]]$gt <- gt
  ebola_rt_samples[[gt]]$gt <- gt
  ebola_rt_samples[[gt]] <- ebola_rt_samples[[gt]] |> filter(type=='forecast')
  #ebola_R_est[[gt]]$gt <- gt
}

ebola_samples <- bind_rows(ebola_samples)
res_ebola_id <- bind_rows(res_ebola_id)
ebola_rt_samples <- bind_rows(ebola_rt_samples)
 ebola_R_est <- bind_rows(ebola_R_est)


 test <- plotrankrt(ebola_rt_samples,
                    res_ebola_id,
                    rt_constant,
                    8)
 
 plotrank_growth_ebola <- plotrankings_growth(ebola_samples,
                                 res_ebola_id,
                                 ebola_sim_data,
                                 rt_ebola,
                                 oneweek=FALSE)
 
 
 ggsave(here("results", paste0("plot_rank_growth_ebola.png")), plotrank_growth_ebola, width=15, height=10, units="cm")
 
 ggsave(here("results", paste0("plot_rank_change_ebola.png")), plotrank_change_ebola, width=15, height=10, units="cm")

#remove first timepoint from ebola_rt_samples because doesn't work
for(i in c(1:6)){
ebola_rt_samples[[i]] <- ebola_rt_samples[[i]] |> filter(! result_list %in% c(1, 18, 35, 52, 69, 86),
                                                         date <='2015-03-01')
}
 
 for(i in c(1:6)){
   ebola_rt_samples[[i]] <- ebola_rt_samples[[i]] |> filter(! result_list %in% c(1, 5, 9, 13, 17, 21))
 }

ggsave(here("results", paste0("plot_rt_rank_ebola.png")), plot_rtest_rank_ebola, width=40, height=15, units="cm")

ggsave(here("results", paste0("plot_rt_rank_growth_ebola.png")), plot_rt_rank_growth_ebola, width=15, height=20, units="cm")

ggsave(here("results", paste0("plot_rt_rank_change_ebola.png")), plot_rt_rank_change_ebola, width=15, height=20, units="cm")

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

plotrankrt_ebola <- plotrankrt(ebola_rt_samples,
                               res_ebola_id,
                               rt_constant,
                               forecast_freq=4)

ggsave(here("results", paste0("plot_rt_ebola.png")), plot_crps_rt_ebola, width=40, height=15, units="cm")

plotrankcase_ebola <- plotrankcase(ebola_samples, res_ebola_id, ebola_sim_data, 4)

ggsave(here("results", paste0("plot_rankcase_ebola.png")), plotrankcase_ebola, width=40, height=13, units="cm")

## plotrankings 

plot_rankings_ebola <- plotrankings(ebola_samples,
                                    res_ebola_id,
                                    ebola_sim_data,
                                    oneweek=FALSE)

ggsave(here("results", paste0("plot_rankings_ebola_oneweek.png")), plot_rankings_ebola, width=15, height=15, units="cm")


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

plot_rankings <- plotrankcase(ebola_samples,
                              res_ebola_id,
                              ebola_sim_data,
                              forecast_freq=4)

ggsave(here("results", paste0("plot_ebola_rankings.png")), plot_rankings)

######################
#### CHOLERA-LIKE ####
######################

cholera_samples <- list()
res_cholera_id <- list()
cholera_rt_samples <- list()
cholera_R_est <- list()

for(gt in c(1:6)){
  cholera_samples[[gt]] <- read_latest(here("results"), paste0("res_cholera_samples", gt)) |>
    filter(type=='forecast')
  res_cholera_id[[gt]] <- read_latest(here("results"), paste0("res_cholera_id", gt))
  #cholera_rt_samples[[gt]] <- read_latest(here("results"), paste0("res_cholera_R", gt))
  #cholera_R_est[[gt]] <- read_latest(here("results"), paste0("res_cholera_R_est", gt))
  # Need to add extra id for generation time #
  cholera_samples[[gt]]$gt <- gt
  res_cholera_id[[gt]]$gt <- gt
  #cholera_rt_samples[[gt]]$gt <- gt
  #cholera_rt_samples[[gt]] <- cholera_rt_samples[[gt]] |> filter(type=='estimate')
  # cholera_R_est[[gt]]$gt <- gt
}

cholera_samples <- bind_rows(cholera_samples)
res_cholera_id <- bind_rows(res_cholera_id)
cholera_rt_samples <- bind_rows(cholera_rt_samples)
cholera_R_est <- bind_rows(cholera_R_est)


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

## Loading data ##

covid_sim_data <- read_latest(here("data"), "covid_sim_data") |> filter(date < "2021-04-30")
rt_covid <- readRDS(here("data", "rt_covid.rds"))

## Loading results ##

covid_samples <- list()
res_covid_id <- list()
covid_rt_samples <- list()
covid_R_est <- list()


for(gt in c(1:6)){
  covid_samples[[gt]] <- read_latest(here("results"), paste0("res_covid_samples", gt)) |>
   filter(type=='forecast')
  res_covid_id[[gt]] <- read_latest(here("results"), paste0("res_covid_id", gt))
  #covid_rt_samples[[gt]] <- read_latest(here("results"), paste0("res_covid_R", gt))
  #covid_R_est[[gt]] <- read_latest(here("results"), paste0("res_covid_R_est", gt))
  # Need to add extra id for generation time #
  covid_samples[[gt]]$gt <- gt
  res_covid_id[[gt]]$gt <- gt
  #covid_rt_samples[[gt]]$gt <- gt
  #covid_rt_samples[[gt]] <- covid_rt_samples[[gt]] |> filter(type=='estimate')
  # covid_R_est[[gt]]$gt <- gt
}

covid_samples <- bind_rows(covid_samples)
res_covid_id <- bind_rows(res_covid_id)
covid_rt_samples <- bind_rows(covid_rt_samples)
covid_R_est <- bind_rows(covid_R_est)


plot_rankings_covid <- plotrankings(covid_samples,
                                    res_covid_id,
                                    covid_sim_data,
                                    oneweek=FALSE)


ggsave(here("results", paste0("plot_rank_growth_covid.png")), plotrank_change, width=15, height=10, units="cm")

ggsave(here("results", paste0("plot_rank_change_ebola.png")), plot_ranks, width=15, height=10, units="cm")


## CRPS ##

plot_crps_covid <- plotcrps(covid_samples, 
                            res_covid_id, 
                            covid_sim_data,
                            forecast_freq=4)

ggsave(here("results", paste0("plot_crps_covid.png")), plot_crps_covid, width=40, height=15, units="cm")

plotrankcase_covid <- plotrankcase(covid_samples,
                              res_covid_id,
                              covid_sim_data,
                              forecast_freq=4)

ggsave(here("results", paste0("plot_rankcase_covid.png")), plot_rankcase_covid, width=40, height=14, units="cm")

rt_covid <- rt_covid |> rename(R=mean)

# For memory reasons
rt_covid <- rt_covid |> filter(date<"2021-01-01")
covid_rt_samples <- covid_rt_samples |> filter(date<"2021-01-01")
# Need to get rid of first timepoint
covid_rt_samples <- covid_rt_samples |> filter(! result_list %in% c(1, 12, 23, 34, 45, 56))

plotrankrt_covid <- plotrankrt(covid_rt_samples,
                               res_covid_id,
                               rt_covid,
                               forecast_freq=4)

ggsave(here("results", paste0("plotrankrt_covid.png")), plotrankrt_covid, width=40, height=16, units="cm")

ggsave(here("results", paste0("plot_rt_rank_growth_covid.png")), plotrankrt_covid, width=15, height=15, units="cm")

ggsave(here("results", paste0("plot_rt_rank_change_covid.png")), plotrankrt_covid, width=15, height=20, units="cm")



ggsave(here("results", paste0("plot_rank_growth_covid.png")), plotrank_change, width=15, height=10, units="cm")

ggsave(here("results", paste0("plot_rank_change_covid.png")), plotrank_change, width=15, height=10, units="cm")

ggsave(here("results", paste0("plot_rt_rank_change_covid.png")), plotrankrt_covid, width=15, height=20, units="cm")

