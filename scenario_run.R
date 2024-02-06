library(EpiNow2)
library(ggplot2)
library(tidyverse)
library(here)

options(mc.cores = 8)

## Example data ##

res_example <- simulate_scenarios(case_data=example_confirmed,
                                  baseline_gen_time=example_generation_time, # uncertain dist
                                  baseline_inc_period=example_incubation_period, # uncertain dist
                                  reporting_delay=example_reporting_delay, # fixed dist
                                  rt_prior=list(mean=2, sd=0.1),
                                  timesplit=10)

## Ebola ##

ebola_confirmed <- read_xlsx(here("data", "ebola_linelist.xlsx"), "lab-confirmed database")
ebola_suspected <- read_xlsx(here("data", "ebola_linelist.xlsx"), "suspected database")

# Need - baseline generation time, baseline incubation period, baseline reporting delay, rt prior

## SARS-CoV-2 ##
covid_ireland <- read.csv("https://raw.githubusercontent.com/european-modelling-hubs/covid19-forecast-hub-europe/main/data-truth/ECDC/final/covid-cases-deaths-final_2024-01-05.csv") |>
  filter(target_variable=="inc case",
         location=="IE")

# Formatting data for EpiNow2
covid_ireland <- covid_ireland |>
  select(date, value) |>
  rename(confirm=value) |>
  filter(confirm>0)

covid_ireland$date <- as.Date(covid_ireland$date, "%Y-%m-%d" )
covid_ireland$confirm <- as.numeric(covid_ireland$confirm)

# All from Sherratt et al. 2021 - surveillance paper
covid_gen_time <- dist_spec(mean=3.6, 
                            sd=3.1, 
                            mean_sd=0.7,
                            sd_sd=0.8,
                            max=30, 
                            distribution="gamma") # MAXIMUM IS JUST A PLACEHOLDER

covid_inc_period <- dist_spec(mean=5.2, 
                              sd=1.52, 
                              mean_sd=1.1,
                              sd_sd=1.1,
                              max=30, 
                              distribution="lognormal") # MAXIMUM IS JUST A PLACEHOLDER

covid_reporting_delay <- dist_spec(mean=4.4, sd=5.6, max=30, distribution="lognormal") # MAXIMUM IS JUST A PLACEHOLDER

res_covid <- simulate_scenarios(case_data=covid_ireland,
                                baseline_gen_time=covid_gen_time,
                                baseline_inc_period=covid_inc_period,
                                reporting_delay=covid_reporting_delay,
                                rt_prior=list(mean=2, sd=0.1))


