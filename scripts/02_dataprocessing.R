###############
#### Ebola ####
###############

library(here)

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

###################
#### SARS-CoV2 ####
###################

# covid_eng <- read.csv("https://api.coronavirus.data.gov.uk/v2/data?areaType=nation&areaCode=E92000001&metric=newCasesBySpecimenDate&format=csv") # THIS DOESN'T WORK ANYMORE - NEED TO FIX
#

#
#covid_eng$date <- as.Date(covid_eng$date, "%Y-%m-%d" )
#covid_eng$confirm <- as.numeric(covid_eng$confirm)
#
## Testing on shorter timeseries
#covid_eng <- covid_eng |>
#  filter(date < "2020-12-31")
#
#rt_covid <- read.csv(here("data", "rt.csv")) |>
#  filter(region=="United Kingdom",
#         type=="estimate") |>
#  select(date, median) |>
#  rename(R=median)
#
#rt_covid$date <- as.Date(rt_covid$date, "%Y-%m-%d")

#################
#### CHOLERA ####
#################

# Weekly cholera case counts by district
cholera_yem <- read.csv(here("data", "YEM-CHOLERA-EOC-DIS-WEEK-20170424-20200621.csv")) # https://figshare.com/articles/dataset/Weekly_cholera_cases_and_rainfall_by_governorate_and_district_in_Yemen_between_2016_and_2020/24231019?file=42635185

cholera_yem$date_monday <- as.Date(cholera_yem$date_monday)
cholera_yem$date_sunday <- as.Date(cholera_yem$date_sunday)

# National daily count
cholera_yem_tot <- cholera_yem |>
  group_by(date_monday, date_sunday) |>
  summarise(cases=sum(cases)) |>
  ungroup()

plot(x=cholera_yem_tot$date_sunday, y=cholera_yem_tot$cases)

# Selecting period of outbreak included last weekly WHO sit rep (1 July 2018) 
cholera_yem_tot <- cholera_yem_tot |>
  filter(date_sunday <="2018-07-01")

# Formatting data for EpiNow2
cholera_yem_tot <- cholera_yem_tot |>
  select(date_sunday, cases) |>
  rename(date=date_sunday, 
         confirm=cases)
