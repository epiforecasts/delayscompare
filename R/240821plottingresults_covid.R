
library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))
source(here("R", "lshtm_theme.R"))

library(scoringutils)

startdate <- as.Date("2020-06-19") # Same start date as data
enddate <- as.Date("2020-06-19") + 6*4*7 + 14 # Long enough time horizon to have six forecast time points with forecasts every 4 weeks, + two weeks data at the end for forecast

## Which scenario
i <- 4

## Load data ##

rt_const_low <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                           R=0.8)

rt_inc <- data.frame(date=seq.Date(from=startdate, to=enddate, by=1),
                     R=seq(from=0.8, to=1.2, length.out=enddate-(startdate-1)))

# Make sure this is correct for the scenario in question:
covid_sim_data_inc <- read_latest(here("data"), "covid_sim_data_inc")

## Load results ##

covid_id <- read_latest(here("results/ebola"), paste0("res_ebolascen", i, "_all_id"))
covid_R <- read_latest(here("results/ebola"), paste0("res_ebolascen", i, "_all_R"))
covid_samples <- read_latest(here("results/ebola"), paste0("res_ebolascen", i, "_all_samples"))

## Plot Rt rankings over time ##

test <- plotrankrt_tp(covid_R,
              covid_id,
              rt_inc,
              forecast_freq = 4)

## Plot case rankings over time ##

plotrankcase(covid_samples,
             covid_id,
             covid_sim_data_inc,
             forecast_freq=4)


