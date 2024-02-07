## Example data ##

res_example <- simulate_scenarios2(case_data=example_confirmed,
                                   gen_mean=3.6,
                                   gen_sd=3.1,
                                   inc_mean=1.6,
                                   inc_sd=0.42,
                                   reporting_delay=example_reporting_delay, # fixed dist
                                   rt_prior=list(mean=2, sd=0.1),
                                   timesplit=10)

## Ebola ##

ebola_confirmed <- read_xlsx(here("data", "ebola_linelist.xlsx"), "lab-confirmed database")
ebola_suspected <- read_xlsx(here("data", "ebola_linelist.xlsx"), "suspected database")

# Need - baseline generation time, baseline incubation period, baseline reporting delay, rt prior

res_ebola <- simulate_scenarios2(case_data=ebola_confirmed,
                                 gen_mean=16.2,
                                 gen_sd=9.40, # from Park et al. 2019
                                 inc_mean=11.4,
                                 inc_sd=8.1, # from Aylward et al. 2014 
                                 reporting_delay=dist_spec(mean=ebola_reporting_delay$mean_mean, sd=ebola_report_delay$sd_mean, distribution="lognormal"),
                                 timesplit=10)

## SARS-CoV-2 ##
covid_eng <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDate&format=csv")

# Formatting data for EpiNow2
covid_eng <- covid_eng |> 
  select(date, newCasesBySpecimenDate) |>
  rename(confirm=newCasesBySpecimenDate)

covid_eng$date <- as.Date(covid_eng$date, "%Y-%m-%d" )
covid_eng$confirm <- as.numeric(covid_eng$confirm)

# Testing on shorter timeseries
covid_eng <- covid_eng |>
  filter(date < "2020-12-31")

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

res_covid <- simulate_scenarios2(case_data=covid_eng,
                                 gen_mean=3.6,
                                 gen_sd=3.1,
                                 inc_mean=5.2,
                                 inc_sd=1.52,
                                 reporting_delay=covid_reporting_delay,
                                 rt_prior=list(mean=2, sd=0.1),
                                 timesplit=10)

####################

covid_ireland <- read.csv("https://raw.githubusercontent.com/european-modelling-hubs/covid19-forecast-hub-europe/main/data-truth/ECDC/final/covid-cases-deaths-final_2024-01-05.csv") |>
  filter(target_variable=="inc case",
         location=="IE")

covid_ireland <- covid_ireland |>
  select(date, value) |>
  rename(confirm=value) |>
  filter(confirm>0)

covid_ireland$confirm <- ifelse(is.na(covid_ireland$confirm), 0, covid_ireland$confirm)

covid_ireland$date <- as.Date(covid_ireland$date, "%Y-%m-%d" )
covid_ireland$confirm <- as.numeric(covid_ireland$confirm)

