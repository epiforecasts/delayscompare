library(here)

source(here("scripts", "02_dataprocessing.R"))

## Estimate Ebola reporting delay ##

ebola_delay <- ebola_confirmed_linelist |>
  dplyr::select("Date of symptom onset", "Date of sample tested") |>
  rename(date_onset="Date of symptom onset", report_date="Date of sample tested") # from Fang et al. 2016

ebola_delay$delay <- difftime(ebola_delay$report_date, ebola_delay$date_onset, units="days")

# Finding the maximum
max_ebola_delay <- max(ebola_delay$delay) # 48 days

ggplot(ebola_delay, aes(x=delay)) +
  geom_histogram(binwidth = 1, col = "#d3d3d36e") +
  theme_bw()

ebola_reporting_delay <- bootstrapped_dist_fit(
  ebola_delay$delay,
  dist="lognormal",
  bootstraps=100,
  bootstrap_samples=1000,
  max_value=max_ebola_delay)

test <- data.frame(test=rlnorm(500,
  meanlog=mean(ebola_reporting_delay[[1]]$parameters$meanlog),
  sdlog=mean(ebola_reporting_delay[[1]]$parameters$sdlog)
))

ggplot(test, aes(x=test)) +
  geom_histogram(binwidth = 1, col = "#d3d3d36e") +
  theme_bw()

saveRDS(ebola_reporting_delay, file=here("data", "ebolareportingdelay.RDS"))
