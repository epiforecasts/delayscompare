#################################
### Preparing case study data ###
#################################
 
library(here)
source(here("scripts", "01_packages.R"))

### EBOLA ###

ebola_confirmed_linelist <- read_xlsx(here("data", "ebola_linelist.xlsx"), "lab-confirmed database")

# Formating for EpiNow2

ebola_confirmed <- ebola_confirmed_linelist |>
  count(`Date of sample tested`, name = "confirm") |>
  rename(date = `Date of sample tested`) |>
  mutate(date = as.Date(date))

# Assuming no data reported on missing days
extra_dates <- data.frame(date=seq(ebola_confirmed$date[1], ebola_confirmed$date[nrow(ebola_confirmed)], by="day"))
ebola_confirmed <- right_join(ebola_confirmed, extra_dates, by="date")

ebola_confirmed$confirm[is.na(ebola_confirmed$confirm)] <- 0

# Make sure days are in order
ebola_confirmed <- ebola_confirmed |>
  arrange(date)

### COVID ###

covid_eng <- read.csv(here("data", "covid_england.csv"))

## Formatting data for EpiNow2
covid_eng$date <- as.Date(covid_eng$date, "%Y-%m-%d" )

## Delta wave only
covid_eng <- covid_eng |> filter(date >= "2021-06-01" & date < "2021-12-01")

### CHOLERA ###

# Weekly cholera case counts by district
cholera_yem <- read.csv(here("data", "YEM-CHOLERA-EOC-DIS-WEEK-20170424-20200621.csv")) # https://figshare.com/articles/dataset/Weekly_cholera_cases_and_rainfall_by_governorate_and_district_in_Yemen_between_2016_and_2020/24231019?file=42635185

cholera_yem$date_monday <- as.Date(cholera_yem$date_monday)
cholera_yem$date_sunday <- as.Date(cholera_yem$date_sunday)

# National daily count
cholera_yem_tot <- cholera_yem |>
  group_by(date_monday, date_sunday) |>
  summarise(cases=sum(cases)) |>
  ungroup()

# Selecting period of outbreak included last weekly WHO sit rep (1 July 2018) 
cholera_yem_tot <- cholera_yem_tot |>
  filter(date_sunday <="2018-07-01")

# Formatting data for EpiNow2
cholera_yem_tot <- cholera_yem_tot |>
  select(date_sunday, cases) |>
  rename(date=date_sunday, 
         confirm=cases)

casestudydata <- list(
  ebola = ebola_confirmed,
  covid = covid_eng,
  cholera = cholera_yem_tot
)