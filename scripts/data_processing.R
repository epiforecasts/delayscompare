
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

ebola_reporting_delay <- readRDS(here("data", "ebolareportingdelay.RDS"))

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

