###############
#### Ebola ####
###############

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

###################
#### SARS-CoV2 ####
###################

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