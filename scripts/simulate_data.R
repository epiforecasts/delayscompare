
## SARS-CoV-2-like 

covid_gen_time <- dist_spec(mean=3.6, 
                            sd=3.1, 
                            max=30, 
                            distribution="gamma")
covid_inc_period <- dist_spec(mean=5.2,
                              sd=1.52,
                              max=30,
                              distribution="lognormal")
covid_rep_delay <- dist_spec(mean=4.4, 
                             sd=5.6, 
                             max=30, 
                             distribution="lognormal")

covid_sim_data <- simulate_infections(
  R=rt_step_covid,
  initial_infections=10,
  generation_time=generation_time_opts(covid_gen_time),
  delays=delay_opts(covid_inc_period + covid_rep_delay),
  obs=obs_opts("poisson", scale=0.3)
)

ggplot(covid_sim_data) + geom_line(aes(x=date, y=value)) + facet_wrap(~variable)

## Ebola-like

ebola_gen_time <- dist_spec(mean=16.2, 
                            sd=9.4, 
                            max=45, 
                            distribution="gamma")

ebola_inc_period <- dist_spec(mean=11.4,
                              sd=8.1,
                              max=30,
                              distribution="lognormal")

ebola_rep_delay <- dist_spec(mean=ebola_reporting_delay$mean_mean, 
                             sd=ebola_reporting_delay$sd_mean, 
                             max=48,
                             distribution="lognormal")

ebola_sim_data <- simulate_infections(
  R=####,
  initial_infections=10,
  generation_time=generation_time_opts(ebola_gen_time),
  delays=delay_opts(ebola_inc_period + ebola_rep_delay),
  obs=obs_opts("poisson")
)



