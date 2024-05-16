library(here)

source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))

####################
#### Ebola-like ####
####################

# Loading and visualising Rt trajectory

rt_ebola <- readRDS(here("data", "rt_ebola.rds"))

ggplot(rt_ebola) + 
  geom_line(aes(x=date, y=R), colour="firebrick4", size=1.2) +
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

save_latest(ebola_sim_data, here("data"), "ebola_sim_data")

#########################
#### SARS-CoV-2-like ####
#########################

# Loading and visualising Rt trajectory

rt_covid <- readRDS(here("data", "rt_covid.rds"))

ggplot(rt_covid) + 
  geom_line(aes(x=date, y=median), color="darkblue", size=1.2) +
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

#### Simulate SARS-CoV-2 data ####

covid_sim_data <- simulate_infections(
  R=rt_covid_epinow,
  initial_infections=2500,
  generation_time=generation_time_opts(covid_gen_time),
  delays=delay_opts(covid_inc_period + covid_rep_delay),
  obs=obs_opts("poisson", scale=0.4)
)

ggplot(covid_sim_data) + geom_line(aes(x=date, y=value)) + facet_wrap(~variable)

save_latest(covid_sim_data, here("data"), "covid_sim_data")

######################
#### Cholera-like ####
######################

# Loading and visualising Rt trajectory

rt_cholera <- readRDS(here("data", "rt_cholera.rds"))

ggplot(rt_cholera) + 
  geom_line(aes(x=date, y=R), colour="#C4961A", size=1.2) +
  geom_ribbon(aes(x=date, ymin=lower_50, ymax=upper_50), alpha=0.5, fill="#FFDB6D") +
  geom_ribbon(aes(x=date, ymin=lower_90, ymax=upper_90), alpha=0.3, fill="#FFDB6D") +
  xlab("Date") +
  ylab("Rt") +
  theme_classic() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", limits = c(min(rt_cholera$date), max(rt_cholera$date)), expand=c(0,0))

# Reformatting rt_cholera for EpiNow2
rt_cholera_epinow <- rt_cholera |>
  select(date, R)

#### Simulate cholera data ####

cholera_sim_data <- simulate_infections(
  R=rt_cholera_epinow,
  initial_infections=5,
  generation_time=generation_time_opts(cholera_gen_time),
  delays=delay_opts(combined_delay_cholera),
  obs=obs_opts(family="poisson", scale=0.28)
)

save_latest(cholera_sim_data, here("data"), "cholera_sim_data")

