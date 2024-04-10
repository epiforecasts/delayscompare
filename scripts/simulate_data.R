
Ex <- 11.4
Vx <- 8.1

## SARS-CoV-2-like 

covid_gen_time <- Gamma(mean=3.6, 
                        sd=3.1,
                        max=30)

covid_inc_period <- LogNormal(meanlog=convert_to_logmean(5.2, 1.52),
                              sdlog=convert_to_logsd(5.2, 1.52),
                              max=30)

covid_rep_delay <- LogNormal(meanlog=convert_to_logmean(4.4, 5.6),
                             sdlog=convert_to_logsd(4.4, 5.6),
                             max=30)

covid_sim_data <- simulate_infections(
  R=rt_covid_epinow,
  initial_infections=10000,
  generation_time=generation_time_opts(covid_gen_time),
  delays=delay_opts(covid_inc_period + covid_rep_delay),
  obs=obs_opts("poisson", scale=0.4)
)

ggplot(covid_sim_data) + geom_line(aes(x=date, y=value)) + facet_wrap(~variable)

saveRDS(covid_sim_data, file=here("data", paste0("covid_sim_data", Sys.Date(), ".rds")))

# In required format for EpiNow2
covid_sim_data_cases <- covid_sim_data |> filter(variable=="reported_cases")
covid_sim_data_cases <- covid_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)

ggplot(covid_sim_data_cases) + geom_line(aes(x=date, y=confirm))

## Ebola-like

ebola_gen_time <- Gamma(mean=16.2,
                        sd=9.4,
                        max=45)

ebola_inc_period <- LogNormal(meanlog=convert_to_logmean(11.4, 8.1),
                              sdlog=convert_to_logsd(11.4, 8.1),
                              max=45)

ebola_rep_delay <- LogNormal(meanlog=ebola_reporting_delay$mean_mean,
                             sdlog=ebola_reporting_delay$sd_mean,
                             max=48)

combined_delay_ebola <- ebola_inc_period + ebola_rep_delay

# Simulate Ebola data

ebola_sim_data <- simulate_infections(
  R=rt_ebola_epinow,
  initial_infections=1,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(combined_delay_ebola),
  obs=obs_opts(family="poisson", scale=0.83)
)

saveRDS(ebola_sim_data, file=here("data", paste0("ebola_sim_data", Sys.Date(), ".rds")))

ggplot(ebola_sim_data) + geom_line(aes(x=date, y=value)) + facet_wrap(~variable) + theme_classic()

# In required format for EpiNow2
ebola_sim_data_cases <- ebola_sim_data |> filter(variable=="reported_cases")
ebola_sim_data_cases <- ebola_sim_data_cases |>
  select(date, value) |>
  rename(confirm=value)


