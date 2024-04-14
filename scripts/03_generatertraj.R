
source(here("R", "funcs_rtraj.R"))

## Ebola Rt trajectory

samples_semi_mechanistic <- readRDS(here("data", "samples_semi_mechanistic.rds" ))

rt_ebola <- samples_semi_mechanistic %>%
  gather(variable, value, R0, cases) %>%
  mutate(variable=factor(variable)) 

rt_ebola <- rt_ebola |> filter(variable=="R0",
                              stochasticity=="stochastic",
                              start_n_week_before=="1",
                              weeks_averaged=="1",
                              transmission_rate=="varying",
                              last_obs==max(last_obs))

rt_ebola <- rt_ebola |> 
  group_by(date) |>
  summarise(R=median(value),
            lower_50=quantile(value, 0.25),
            upper_50=quantile(value, 0.75),
            lower_90=quantile(value, 0.1),
            upper_90=quantile(value, 0.9))
            
saveRDS(rt_ebola, here("data", "rt_ebola.rds"))

#### SARS-CoV-2 ####

## SARS-CoV-2 Rt trajectory

rt_covid <- get_covid19_nowcasts()

# Keep UK and up to end of 2021 only
rt_covid <- rt_covid |>
  filter(country=="United Kingdom") |>
  filter(date<="2021-12-31")

saveRDS(rt_covid, here("data", "rt_covid.rds"))