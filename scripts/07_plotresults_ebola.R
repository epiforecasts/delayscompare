library(here)

source(here("scripts", "01_packages.R"))
#source(here("R", "scenario_loop.R"))
source(here("R", "funcs_plots.R"))
source(here("R", "funcs_data.R"))
source(here("R", "lshtm_theme.R"))

####################
#### EBOLA-LIKE ####
####################

## Loading data - constant Rt ##

ebola_sim_data <- read_latest(here("data"), "ebola_sim_data_const")

# Adding extra dates beyond enddate to catch all forecasted dates
rt_constant <- data.frame(date=seq.Date(from=startdate, to=enddate+14, by=1),
                          R=2)

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

## Rt plot

ebola_const_rt_tp <- plotrankrt_tp(ebola_rt_samples,
                    res_ebola_id,
                    rt_constant,
                    8)

ggsave(here("results", paste0("plotebola_const_rt_tp.png")), ebola_const_rt_tp, width=18, height=22, units="cm")
 
## Forecast plot

ebola_const_case_tp <- plotrankcase(ebola_samples,
             res_ebola_id,
             ebola_sim_data,
             8)

ggsave(here("results", paste0("plotebola_const_case_tp.png")), ebola_const_case_tp, width=18, height=22, units="cm")



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

######################################################

## Loading data - increasing Rt ##

ebola_sim_data <- read_latest(here("data"), "ebola_sim_data_inc")

# Adding extra dates beyond enddate to catch all forecasted dates
rt_inc <- data.frame(date=seq.Date(from=startdate, to=enddate+122, by=1),
                     R=seq(from=0.5, to=4.5, length.out=(difftime(enddate, startdate-1)*2)-1))

## Loading results ##
ebola_samples <- list()
res_ebola_id <- list()
ebola_rt_samples <- list()
ebola_R_est <- list()

for(gt in c(1:6)){
  ebola_samples[[gt]] <- read_latest(here("results"), paste0("res_inc_ebola_samples", gt)) |>
    filter(type=='forecast')
  res_ebola_id[[gt]] <- read_latest(here("results"), paste0("res_inc_ebola_id", gt))
  ebola_rt_samples[[gt]] <- read_latest(here("results"), paste0("res_inc_ebola_R", gt))
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

## Rt plot - increasing

ebola_inc_rt_tp <- plotrankrt_tp(ebola_rt_samples,
                                   res_ebola_id,
                                   rt_inc,
                                   8)

ggsave(here("results", paste0("plotebola_inc_rt_tp.png")), ebola_inc_rt_tp, width=18, height=22, units="cm")

## Forecast plot

ebola_inc_case_tp <- plotrankcase(ebola_samples,
                                    res_ebola_id,
                                    ebola_sim_data,
                                    8)

ggsave(here("results", paste0("plotebola_inc_case_tp.png")), ebola_inc_case_tp, width=18, height=22, units="cm")

##################################################################

## Loading data - decreasing Rt ##

ebola_sim_data <- read_latest(here("data"), "ebola_sim_data_dec")

# Adding extra dates beyond enddate to catch all forecasted dates
rt_dec <- data.frame(date=seq.Date(from=startdate, to=enddate+122, by=1),
                     R=rev(seq(from=0.5, to=4.5, length.out=(difftime(enddate, startdate-1)*2)-1)))

## Loading results ##
ebola_samples <- list()
res_ebola_id <- list()
ebola_rt_samples <- list()
ebola_R_est <- list()

for(gt in c(1:6)){
  ebola_samples[[gt]] <- read_latest(here("results"), paste0("res_dec_ebola_samples", gt)) |>
    filter(type=='forecast')
  res_ebola_id[[gt]] <- read_latest(here("results"), paste0("res_dec_ebola_id", gt))
  ebola_rt_samples[[gt]] <- read_latest(here("results"), paste0("res_dec_ebola_R", gt))
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

## Rt plot - decreasing

ebola_dec_rt_tp <- plotrankrt_tp(ebola_rt_samples,
                                 res_ebola_id,
                                 rt_dec,
                                 8)

ggsave(here("results", paste0("plotebola_dec_rt_tp.png")), ebola_dec_rt_tp, width=18, height=22, units="cm")

## Forecast plot

ebola_dec_case_tp <- plotrankcase(ebola_samples,
                                  res_ebola_id,
                                  ebola_sim_data,
                                  8)

ggsave(here("results", paste0("plotebola_dec_case_tp.png")), ebola_dec_case_tp, width=18, height=22, units="cm")

############## COVID-19 #############

covid_sim_data <- read_latest("data", "covid_sim_data_const")

## Loading results ##
covid_samples <- list()
res_covid_id <- list()
covid_rt_samples <- list()
covid_R_est <- list()

for(gt in c(1:6)){
  covid_samples[[gt]] <- read_latest(here("results"), paste0("res_const_covid_samples", gt)) |>
    filter(type=='forecast')
  res_covid_id[[gt]] <- read_latest(here("results"), paste0("res_const_covid_id", gt))
  covid_rt_samples[[gt]] <- read_latest(here("results"), paste0("res_const_covid_R", gt))
  #covid_R_est[[gt]] <- read_latest(here("results"), paste0("res_const_covid_R_est", gt))
  # Need to add extra id for generation time #
  covid_samples[[gt]]$gt <- gt
  res_covid_id[[gt]]$gt <- gt
  covid_rt_samples[[gt]]$gt <- gt
  covid_rt_samples[[gt]] <- covid_rt_samples[[gt]] |> filter(type=='forecast')
  #covid_R_est[[gt]]$gt <- gt
}

covid_samples <- bind_rows(covid_samples)
res_covid_id <- bind_rows(res_covid_id)
covid_rt_samples <- bind_rows(covid_rt_samples)
covid_R_est <- bind_rows(covid_R_est)

## Rt plot - constreasing

covid_const_rt_tp <- plotrankrt_tp(covid_rt_samples,
                                 res_covid_id,
                                 rt_constant,
                                 4)


ggsave(here("results", paste0("plotcovid_const_rt_tp.png")), covid_const_rt_tp, width=30, height=22, units="cm")

## Forecast plot

covid_const_case_tp <- plotrankcase(covid_samples,
                                  res_covid_id,
                                  covid_sim_data,
                                  4)

ggsave(here("results", paste0("plotcovid_const_case_tp.png")), covid_const_case_tp, width=30, height=22, units="cm")












