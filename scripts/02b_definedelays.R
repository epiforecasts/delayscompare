#######################
### Defining delays ###
#######################

# Truncation rule: when literature does not provide a maximum value,
# max is set to round(mean + 5*sd). Where a literature source provides
# a max value directly, that is used instead (noted inline).

#### Ebola-like ####

## Generation time

library(here)

ebola_gen_time <- Gamma(mean=10.9,
                        sd=8,
                        max=50) # generation time from Lau et al. 2017 (PNAS); SD assumed

## Incubation period

ebola_inc_period <- Gamma(mean=8.5,
                          sd=5.5,
                          max=36) # pooled incubation period from Nash et al. 2024 review; SD assumed

## Reporting delay

 #ebola_reporting_delay <- readRDS(here("data", "ebolareportingdelay.RDS")) # estimated from Fang et al. 2016 linelist - no longer need, as using date of symptom onset.

# combined_delay_ebola <- ebola_inc_period + ebola_reporting_delay

#### SARS-CoV-2-like ####

## Generation time

covid_gen_time <- Gamma(mean=3.6,
                        sd=3.1,
                        max=15) # from Sherratt et al. 2021 - surveillance paper

## Incubation period

covid_inc_period <- LogNormal(mean=5.2,
                              sd=1.52,
                              max=21) # from Sherratt et al. 2021 - surveillance paper

## Reporting delay

covid_reporting_delay <- LogNormal(mean=4.4,
                             sd=5.6,
                             max=18) # from Sherratt et al. 2021 - surveillance paper

combined_delay_covid <- covid_inc_period + covid_reporting_delay

#### cholera-like ####

## Generation time
## No study has estimated the cholera generation interval directly. Azman et al.
## (2016, Emerg Infect Dis, doi:10.3201/eid2206.151592) assumed Gamma(shape=0.5,
## rate=0.1) (mean 5, SD ~7.1), grounded in household-contact data from Weil et
## al. (2009). We use it as the imposed value, treating it as an assumption
## rather than an estimate.
cholera_gen_time <- Gamma(shape=0.5,
                          rate=0.1,
                          max=35) # Azman et al. 2016; ultimately Weil et al. 2009

## Incubation period 

cholera_inc_period <- LogNormal(mean=1.77,
                                sd=1.36, max=8) # Azman et al. 2013

## Reporting delay

cholera_reporting_delay <- Gamma(shape=1, rate=1/4.4, max=22) # Exponential with mean 4.4 from Nishiura et al. 2017 https://pmc.ncbi.nlm.nih.gov/articles/PMC5527441/


combined_delay_cholera <- cholera_inc_period + cholera_reporting_delay

## Saving delays for sim_scenario input 
delays <- list(
  ebola = list(
    gen = c(mean = 10.9, mean_sd = 1.09, sd = 8, sd_sd = 0.8, max = 50),  # Lau et al. 2017
    inc = c(mean = 8.5, mean_sd = 0.85, sd = 5.5, sd_sd = 0.55, max = 36),  # Nash et al. 2024 (pooled)
    rep = c(mean = 0, sd = 0, max = 0),
    underreport = 0.83,
    init_infs=50
  ),
  covid = list(
    gen = c(mean = 3.6, mean_sd = 0.7, sd = 3.1, sd_sd = 0.8, max = 15),
    inc = c(mean = 5.2, mean_sd = 1.1, sd = 1.52, sd_sd = 1.1, max = 21),
    rep = c(mean = 4.4, mean_sd = 0.44, sd = 5.6, sd_sd = 0.56, max = 18),
    underreport = 0.4,
    init_infs=2500
  ),
  cholera = list(
    gen = c(mean = 5, mean_sd = 0.5, sd = 7.07, sd_sd = 0.71, max = 35),  # Azman 2016: Gamma(shape=0.5, rate=0.1)
    inc = c(mean = 1.77, mean_sd = 0.18, sd = 1.36, sd_sd = 0.14, max = 8),
    rep = c(mean = 4.4, mean_sd = 0.66, sd = 4.4, sd_sd = 0.66, max = 22),
    underreport = 0.28,
    init_infs=75
  ))

## Wide-prior variant for the regime-2 case-study comparison ("learn vs impose").
## Prior SDs on mean and SD set to 10% relative; max truncations set to the
## 99.95% quantile of the literature distribution (rounded up). Means and SDs at
## the distribution centre are unchanged from the literature `delays` list above.
delays_widepriors <- list(
  ebola = list(
    gen = c(mean = 10.9, mean_sd = 1.09, sd = 8,    sd_sd = 0.8,  max = 50),  # Lau et al. 2017
    inc = c(mean = 8.5,  mean_sd = 0.85, sd = 5.5,  sd_sd = 0.55, max = 36),  # Nash et al. 2024
    rep = c(mean = 0,    mean_sd = 0,    sd = 0,    sd_sd = 0,    max = 0),
    underreport = 0.83,
    init_infs = 50
  ),
  covid = list(
    gen = c(mean = 3.6,  mean_sd = 0.36, sd = 3.1,  sd_sd = 0.31,  max = 25),
    inc = c(mean = 5.2,  mean_sd = 0.52, sd = 1.52, sd_sd = 0.152, max = 15),
    rep = c(mean = 4.4,  mean_sd = 0.44, sd = 5.6,  sd_sd = 0.56,  max = 70),
    underreport = 0.4,
    init_infs = 2500
  ),
  cholera = list(
    gen = c(mean = 5,    mean_sd = 0.5,   sd = 7.07, sd_sd = 0.71,  max = 35),  # Azman 2016: Gamma(shape=0.5, rate=0.1)
    inc = c(mean = 1.77, mean_sd = 0.177, sd = 1.36, sd_sd = 0.136, max = 15),
    rep = c(mean = 4.4,  mean_sd = 0.44,  sd = 4.4,  sd_sd = 0.44,  max = 35),
    underreport = 0.28,
    init_infs = 75
  )
)

## Saving dates for datacollect
startenddates <- list(
  ebola = list(
    startdate = as.Date("2014-05-18"), # Same start date as data
    enddate = as.Date("2014-05-18") + 6*4*7 + 14 # Long enough time horizon to have six forecast timepoints with forecasts every 4 weeks
  ),
  covid = list(
  startdate  = as.Date("2021-01-01"), # Same start date as data
  enddate = as.Date("2021-01-01") + 6*4*7 + 14 # Long enough time horizon to have six forecast time points with forecasts every 4 weeks, + two weeks data at the end for forecast
  ),
  cholera = list(
    startdate = as.Date("2017-04-23"), # Same start date as data
    enddate = as.Date("2017-04-23") + 6*4*7 + 14 # Long enough time horizon to have six forecast timepoints with forecasts every 4 weeks
  )
)

