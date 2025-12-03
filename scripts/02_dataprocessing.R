###############
#### Ebola ####
###############

library(here)

ebola_confirmed_linelist <- read_xlsx(here("data", "ebola_linelist.xlsx"), "lab-confirmed database")
ebola_suspected_linelist <- read_xlsx(here("data", "ebola_linelist.xlsx"), "suspected database")

# Formating for EpiNow2

ebola_confirmed <- ebola_confirmed_linelist |>
  count(`Date of symptom onset`, name = "confirm") |>
  rename(date = `Date of symptom onset`)

# Assuming no data reported on missing days
extra_dates <- data.frame(date=seq(ebola_confirmed$date[1], ebola_confirmed$date[nrow(ebola_confirmed)], by="day"))
ebola_confirmed <- right_join(ebola_confirmed, extra_dates, by="date")

ebola_confirmed$confirm[is.na(ebola_confirmed$confirm)] <- 0

###################
#### SARS-CoV2 ####
###################

covid_eng_file <- here("data", "covid_england.csv")

if (file.exists(covid_eng_file)) {
  message("Reading COVID-19 data from cache...")
  covid_eng <- read_csv(covid_eng_file)
} else {
  message("Fetching COVID-19 data from UKHSA API...")
  if (!requireNamespace("ukhsadatR", quietly = TRUE)) {
    stop("Package 'ukhsadatR' needed to fetch COVID data. Install with: install.packages('ukhsadatR')")
  }
  tryCatch({
    covid_eng <- ukhsadatR::get_data(
      theme = "infectious_disease",
      sub_theme = "respiratory",
      topic = "COVID-19",
      geography_type = "Nation",
      geography = "England",
      metric = "COVID-19_cases_casesByDay"
    )

    # Validate expected columns exist
    if (!all(c("date", "metric_value") %in% names(covid_eng))) {
      stop("Fetched data missing expected columns")
    }

    covid_eng <- covid_eng |>
      select(date, confirm = metric_value)

    # Ensure data directory exists
    if (!dir.exists(here("data"))) {
      dir.create(here("data"), recursive = TRUE)
    }

    write_csv(covid_eng, covid_eng_file)
    message("Data cached successfully")
  }, error = function(e) {
    stop("Failed to fetch COVID-19 data: ", e$message)
  })
}

covid_eng$date <- as.Date(covid_eng$date, "%Y-%m-%d" )
covid_eng$confirm <- as.numeric(covid_eng$confirm)

rt_covid <- read_csv(here("data", "rt.csv")) |>
 filter(region=="United Kingdom",
        type=="estimate") |>
 select(date, median) |>
 rename(R=median)

rt_covid$date <- as.Date(rt_covid$date, "%Y-%m-%d")

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
