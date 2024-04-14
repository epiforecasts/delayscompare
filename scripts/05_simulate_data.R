####################
#### Ebola-like ####
####################

## Generation time

ebola_gen_time <- Gamma(mean=16.2,
                        sd=9.4,
                        max=45)

## Incubation period

ebola_inc_period <- LogNormal(meanlog=convert_to_logmean(11.4, 8.1),
                              sdlog=convert_to_logsd(11.4, 8.1),
                              max=45)

## Reporting delay

ebola_reporting_delay <- readRDS(here("data", "ebolareportingdelay.RDS"))

ebola_rep_delay <- LogNormal(meanlog=ebola_reporting_delay$mean_mean,
                             sdlog=ebola_reporting_delay$sd_mean,
                             max=48)

combined_delay_ebola <- ebola_inc_period + ebola_rep_delay

## Loading and visualising Rt trajectory

rt_ebola <- readRDS(here("data", "rt_ebola.rds"))

ggplot(rt_ebola) + 
  geom_line(aes(x=date, y=R), colour="firebrick4", linewidth=1.2) +
  geom_ribbon(aes(x=date, ymin=lower_50, ymax=upper_50), alpha=0.5, fill="firebrick4") +
  geom_ribbon(aes(x=date, ymin=lower_90, ymax=upper_90), alpha=0.3, fill="firebrick4") +
  xlab("Date") +
  ylab("Rt") +
  theme_classic() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", limits = c(min(rt_ebola$date), max(rt_ebola$date)), expand=c(0,0))

# Reformatting rt_ebola for EpiNow2
rt_ebola_epinow <- rt_ebola |>
  select(date, R)

#### Simulate Ebola data ####

ebola_sim_data <- simulate_infections(
  R=rt_ebola_epinow,
  initial_infections=5,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(combined_delay_ebola),
  obs=obs_opts(family="poisson", scale=0.83)
)

saveRDS(ebola_sim_data, file=here("data", paste0("ebola_sim_data", Sys.Date(), ".rds")))

ggplot(ebola_sim_data) + geom_line(aes(x=date, y=value)) + facet_wrap(~variable) + theme_classic()

#########################
#### SARS-CoV-2-like ####
#########################

## Generation time

covid_gen_time <- Gamma(mean=3.6, 
                        sd=3.1,
                        max=30)

## Incubation period

covid_inc_period <- LogNormal(meanlog=convert_to_logmean(5.2, 1.52),
                              sdlog=convert_to_logsd(5.2, 1.52),
                              max=30)

## Reporting delay

covid_rep_delay <- LogNormal(meanlog=convert_to_logmean(4.4, 5.6),
                             sdlog=convert_to_logsd(4.4, 5.6),
                             max=30)

# Loading and visualising Rt trajectory

rt_covid <- readRDS(here("data", "rt_covid.rds"))

ggplot(rt_covid) + 
  geom_line(aes(x=date, y=median), color="darkblue", linewidth=1.2) +
  geom_ribbon(aes(x=date, ymin=lower_50, ymax=upper_50), alpha=0.5, fill="darkblue") +
  geom_ribbon(aes(x=date, ymin=lower_90, ymax=upper_90), alpha=0.3, fill="darkblue") +
  xlab("Date") +
  ylab("Rt") +
  theme_classic() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", limits = c(min(rt_covid$date), max(rt_covid$date)), expand=c(0,0))

# Reformatting rt_covid for EpiNow2
rt_covid_epinow <- rt_covid |> 
  select(date, median) |>
  rename(R=median)

covid_sim_data <- simulate_infections(
  R=rt_covid_epinow,
  initial_infections=2500,
  generation_time=generation_time_opts(covid_gen_time),
  delays=delay_opts(covid_inc_period + covid_rep_delay),
  obs=obs_opts("poisson", scale=0.4)
)

ggplot(covid_sim_data) + geom_line(aes(x=date, y=value)) + facet_wrap(~variable)

saveRDS(covid_sim_data, file=here("data", paste0("covid_sim_data", Sys.Date(), ".rds")))


