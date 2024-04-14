source(here("scripts", "02_dataprocessing.R"))

## Estimate Ebola reporting delay ##

ebola_delay <- ebola_confirmed_linelist |>
  dplyr::select("Date of symptom onset", "Date of sample tested") |>
  rename(date_onset="Date of symptom onset", report_date="Date of sample tested")

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

## WHY DOESN'T THIS WORK NOW? ##
test <- data.frame(test=rlnorm(500, meanlog=ebola_reporting_delay$mean_mean, sdlog=ebola_reporting_delay$sd_mean))

ggplot(test, aes(x=test)) +
  geom_histogram(binwidth = 1, col = "#d3d3d36e") +
  theme_bw()

saveRDS(ebola_reporting_delay, file=here("data", "ebolareportingdelay.RDS"))

## Estimate cholera reporting delay ##

cholera_confirmed <- read.csv(here("data", "ZambiaCholera.csv"))

# Separate out total reports
cholera_confirmed_tot <- cholera_confirmed |>
  filter(Location=="AFR::ZMB" | Location=="AFR::ZMB::Lusaka::Lusaka::(Chawama|Chelston|Chilenje|Chipata|Kanyama|Matero)")

cholera_confirmed <- cholera_confirmed |>
  filter(TL==TR)

# Given all other variables are "NA" once get down to daily case counts, can only look at suspected cases.

cholera_confirmed <- cholera_confirmed |>
  group_by(TL) |>
  summarise(suspect=sum(sCh))

# Under-reporting
