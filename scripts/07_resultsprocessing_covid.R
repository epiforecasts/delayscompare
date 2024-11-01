library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

####################
#### COVID-LIKE ####
####################

for(i in 1:16){

covid_id <- list()
covid_R <- list()
covid_samples <- list()
covid_warnings <- list()

for(gt in c(1:6)){
  covid_id[[gt]] <- read_latest(here("results"), paste0("res_covidscen", i, "_id", gt))
  covid_R[[gt]] <- read_latest(here("results"), paste0("res_covidscen", i, "_R", gt))
  covid_samples[[gt]] <- read_latest(here("results"), paste0("res_covidscen", i, "_samples", gt))
  covid_warnings[[gt]] <- read_latest(here("results"), paste0("res_covidscen", i, "_warnings", gt))
  # Add gt column
  covid_id[[gt]]$gt <- gt
  covid_R[[gt]]$gt <- gt
  covid_samples[[gt]]$gt <- gt
  covid_warnings[[gt]]$gt <- gt
}

covid_id <- bind_rows(covid_id)
covid_R <- bind_rows(covid_R)
covid_samples <- bind_rows(covid_samples)

save_latest(covid_id, here("results/covid"), paste0("res_covidscen", i, "_all_id"))
save_latest(covid_R, here("results/covid"), paste0("res_covidscen", i, "_all_R"))
save_latest(covid_samples, here("results/covid"), paste0("res_covidscen", i, "_all_samples"))
save_latest(covid_warnings, here("results/covid"), paste0("res_covidscen", i, "_all_warnings"))

}








