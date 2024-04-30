#######################
### Defining delays ###
#######################

#### Ebola-like ####

## Generation time

ebola_gen_time <- Gamma(mean=16.2,
                        sd=9.4,
                        max=45) # from Park et al. 2019

## Incubation period

ebola_inc_period <- LogNormal(meanlog=convert_to_logmean(11.4, 8.1),
                              sdlog=convert_to_logsd(11.4, 8.1),
                              max=45) # from Aylward et al. 2014 

## Reporting delay

ebola_reporting_delay <- readRDS(here("data", "ebolareportingdelay.RDS")) # estimated from Fang et al. 2016 linelist

ebola_rep_delay <- LogNormal(meanlog=ebola_reporting_delay$mean_mean,
                             sdlog=ebola_reporting_delay$sd_mean,
                             max=48)

combined_delay_ebola <- ebola_inc_period + ebola_rep_delay

#### SARS-CoV-2-like ####

## Generation time

covid_gen_time <- Gamma(mean=3.6, 
                        sd=3.1,
                        max=30) # from Sherratt et al. 2021 - surveillance paper

## Incubation period

covid_inc_period <- LogNormal(meanlog=convert_to_logmean(5.2, 1.52),
                              sdlog=convert_to_logsd(5.2, 1.52),
                              max=30) # from Sherratt et al. 2021 - surveillance paper

## Reporting delay

covid_rep_delay <- LogNormal(meanlog=convert_to_logmean(4.4, 5.6),
                             sdlog=convert_to_logsd(4.4, 5.6),
                             max=30) # MAXIMUM IS JUST A PLACEHOLDER # from Sherratt et al. 2021 - surveillance paper

#### cholera-like ####

## Generation time

#cholera_gen_time <- Gamma(mean=8.51,
#                          sd=0.54,
#                          max=30) # max is placeholder # https://pubmed.ncbi.nlm.nih.gov/21752809/

cholera_gen_time <- Gamma(mean=5,
                          sd=8,
                          max=30) # https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(18)30230-4

## Incubation period 

cholera_inc_period <- LogNormal(meanlog=convert_to_logmean(1.4,1.98),
                                sdlog=convert_to_logsd(1.4, 1.98),
                                max=30) # max is a placeholder # Azman et al. 2013

## Reporting delay

cholera_rep_delay <- LogNormal(meanlog=convert_to_logmean(4.4, 1.88),
                               sdlog=convert_to_logsd(4.4, 1.88),
                               max=30) # max is placeholder # https://tbiomed.biomedcentral.com/articles/10.1186/s12976-017-0061-x 

combined_delay_cholera <- cholera_inc_period + cholera_rep_delay
