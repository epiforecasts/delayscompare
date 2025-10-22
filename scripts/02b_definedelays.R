#######################
### Defining delays ###
#######################

#### Ebola-like ####

## Generation time

library(here)

ebola_gen_time <- Gamma(mean=16.2,
                        sd=9.4,
                        max=45) # from Park et al. 2019

## Incubation period

ebola_inc_period <- LogNormal(mean=11.4,
                              sd=8.1,
                              max=45) # from Aylward et al. 2014 

## Reporting delay

 #ebola_reporting_delay <- readRDS(here("data", "ebolareportingdelay.RDS")) # estimated from Fang et al. 2016 linelist - no longer need, as using date of symptom onset.

# combined_delay_ebola <- ebola_inc_period + ebola_reporting_delay

#### SARS-CoV-2-like ####

## Generation time

covid_gen_time <- Gamma(mean=3.6, 
                        sd=3.1,
                        max=30) # from Sherratt et al. 2021 - surveillance paper

## Incubation period

covid_inc_period <- LogNormal(mean=5.2,
                              sd=1.52,
                              max=30) # from Sherratt et al. 2021 - surveillance paper

## Reporting delay

covid_reporting_delay <- LogNormal(mean=4.4,
                             sd=5.6,
                             max=30) # MAXIMUM IS JUST A PLACEHOLDER # from Sherratt et al. 2021 - surveillance paper

combined_delay_covid <- covid_inc_period + covid_reporting_delay

#### cholera-like ####

## Generation time

cholera_gen_time <- Gamma(mean=8.51,
                          sd=0.54,
                          max=50) # max is placeholder # https://pubmed.ncbi.nlm.nih.gov/21752809/

# cholera_gen_time <- Gamma(mean=5,
#                           sd=8,
#                          max=30) # https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(18)30230-4

## Incubation period 

cholera_inc_period <- LogNormal(mean=1.77,
                                sd=1.08, max=30) # max is a placeholder # Azman et al. 2013

## Reporting delay

cholera_reporting_delay <- LogNormal(mean=4.4,
                               sd=0.67, max=30) # max is placeholder # https://tbiomed.biomedcentral.com/articles/10.1186/s12976-017-0061-x 


combined_delay_cholera <- cholera_inc_period + cholera_reporting_delay

## Saving delays for sim_scenario input 
delays <- list(
  ebola = list(
    gen = c(mean = 16.2, sd = 9.4, max = 45),
    inc = c(mean = 11.4, sd = 8.1, max = 45),
    rep = c(mean = 0, sd = 0, max = 0),
    underreport = 0.83
  ),
  covid = list(
    gen = c(mean = 3.6, sd = 3.1, max = 30),
    inc = c(mean = 5.2, sd = 1.52, max = 30),
    rep = c(mean = 4.4, sd = 5.6, max = 30),
    underreport = 0.4
  ),
  cholera = list(
    gen = c(mean = 8.51, sd = 0.54, max = 50),
    inc = c(mean = 1.77, sd = 1.08, max = 30),
    rep = c(mean = 4.4, sd = 0.67, max = 30),
    underreport = 0.28
  ))
