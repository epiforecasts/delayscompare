
#### Ebola ####

ebola_confirmed_linelist <- read_xlsx(here("data", "ebola_linelist.xlsx"), "lab-confirmed database")
ebola_suspected_linelist <- read_xlsx(here("data", "ebola_linelist.xlsx"), "suspected database")

# Formating for EpiNow2

ebola_confirmed <- incidence(ebola_confirmed_linelist,
                             date_index="Date of sample tested",
                             interval="day")

ebola_confirmed <- ebola_confirmed |>
  select(date_index, count) |>
  rename(date=date_index, confirm=count)

# Assuming no data reported on missing days
extra_dates <- data.frame(date=seq(ebola_confirmed$date[1], ebola_confirmed$date[nrow(ebola_confirmed)], by="day"))
ebola_confirmed <- right_join(ebola_confirmed, extra_dates, by="date")

ebola_confirmed$confirm[is.na(ebola_confirmed$confirm)] <- 0

ebola_reporting_delay <- readRDS(here("data", "ebolareportingdelay.rds"))

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
            

ggplot(rt_ebola) + 
  geom_line(aes(x=date, y=R), colour="firebrick4", linewidth=1.2) +
  geom_ribbon(aes(x=date, ymin=lower_50, ymax=upper_50), alpha=0.5, fill="firebrick4") +
  geom_ribbon(aes(x=date, ymin=lower_90, ymax=upper_90), alpha=0.3, fill="firebrick4") +
  xlab("Date") +
  ylab("Rt") +
  theme_classic() +
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y", limits = c(min(rt_ebola$date), max(rt_ebola$date)), expand=c(0,0))

rt_ebola_epinow <- rt_ebola |>
  select(date, R)
                                               
#### SARS-CoV-2 ####

covid_eng <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDate&format=csv")

# Formatting data for EpiNow2
covid_eng <- covid_eng |> 
  select(date, newCasesBySpecimenDate) |>
  rename(confirm=newCasesBySpecimenDate)

covid_eng$date <- as.Date(covid_eng$date, "%Y-%m-%d" )
covid_eng$confirm <- as.numeric(covid_eng$confirm)

# Testing on shorter timeseries
covid_eng <- covid_eng |>
  filter(date < "2020-12-31")

rt_covid <- read.csv(here("data", "rt.csv")) |>
  filter(region=="United Kingdom",
         type=="estimate") |>
  select(date, median) |>
  rename(R=median)

rt_covid$date <- as.Date(rt_covid$date, "%Y-%m-%d")

## SARS-CoV-2 Rt trajectory

rt_covid <- get_covid19_nowcasts()

# Keep UK and up to end of 2021 only
rt_covid <- rt_covid |>
  filter(country=="United Kingdom") |>
  filter(date<="2021-12-31")

#saveRDS(rt_covid, here("data", "rt_covid.rds"))

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
  
