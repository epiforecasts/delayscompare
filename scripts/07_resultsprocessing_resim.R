library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

####################
#### COVID-LIKE ####
#####################

#covid_id <- list()
#covid_R <- list()
#covid_samples <- list()
#covid_warnings <- list()
# covid_summaries <- list()

#for(gt in c(1:6)){
#  covid_id[[gt]] <- read_latest(here("results"), paste0("res_covid_sim_latest_id", gt))
#  covid_R[[gt]] <- read_latest(here("results"), paste0("res_covid_sim_latest_R", gt))
#  covid_samples[[gt]] <- read_latest(here("results"), paste0("res_covid_sim_latest_samples", gt))
#  covid_warnings[[gt]] <- read_latest(here("results"), paste0("res_covid_sim_latest_warnings", gt))
#  covid_summaries[[gt]] <- read_latest(here("results"), paste0("res_covid_sim_latest_summary", gt))
#  # Add gt column
#  covid_id[[gt]]$gt <- gt
#  covid_R[[gt]]$gt <- gt
#  covid_samples[[gt]]$gt <- gt
#  covid_warnings[[gt]]$gt <- gt
#  covid_summaries[[gt]]$gt <- gt
#}

#covid_id <- bind_rows(covid_id)
#covid_R <- bind_rows(covid_R)
#covid_samples <- bind_rows(covid_samples)
## covid_summaries <- bind_rows(covid_summaries)

#save_latest(covid_id, here("results/resim_covid"), paste0("resim_covid_latest_all_id"))
#save_latest(covid_R, here("results/resim_covid"), paste0("resim_covid_latest_all_R"))
#save_latest(covid_samples, here("results/resim_covid"), paste0("resim_covid_latest_all_samples"))
## save_latest(covid_warnings, here("results/covidcase"), paste0("res_covidscen", i, "_all_warnings"))
#save_latest(covid_summaries, here("results/resim_covid"), paste0("resim_covid_latest_all_summary"))

###################
#### EBOLA-LIKE ####
####################

ebola_id <- list()
ebola_R <- list()
ebola_samples <- list()
ebola_warnings <- list()
ebola_summaries <- list()

for(gt in c(1:6)){
  ebola_id[[gt]] <- read_latest(here("results"), paste0("res_ebola_sim_latest_id", gt))
  ebola_R[[gt]] <- read_latest(here("results"), paste0("res_ebola_sim_latest_R", gt))
  ebola_samples[[gt]] <- read_latest(here("results"), paste0("res_ebola_sim_latest_samples", gt))
  ebola_warnings[[gt]] <- read_latest(here("results"), paste0("res_ebola_sim_latest_warnings", gt))
  ebola_summaries[[gt]] <- read_latest(here("results"), paste0("res_ebola_sim_latest_summary", gt))
  # Add gt column
  ebola_id[[gt]]$gt <- gt
  ebola_R[[gt]]$gt <- gt
  ebola_samples[[gt]]$gt <- gt
  ebola_warnings[[gt]]$gt <- gt
  ebola_summaries[[gt]]$gt <- gt
}

ebola_id <- bind_rows(ebola_id)
ebola_R <- bind_rows(ebola_R)
ebola_samples <- bind_rows(ebola_samples)
# ebola_summaries <- bind_rows(ebola_summaries)

save_latest(ebola_id, here("results/resim_ebola"), paste0("resim_ebola_latest_all_id"))
save_latest(ebola_R, here("results/resim_ebola"), paste0("resim_ebola_latest_all_R"))
save_latest(ebola_samples, here("results/resim_ebola"), paste0("resim_ebola_latest_all_samples"))
# save_latest(ebola_warnings, here("results/ebolacase"), paste0("res_ebolascen", i, "_all_warnings"))
save_latest(ebola_summaries, here("results/resim_ebola"), paste0("resim_ebola_latest_all_summary"))


######################
#### CHOLERA-LIKE ####
######################

cholera_id <- list()
cholera_R <- list()
cholera_samples <- list()
cholera_warnings <- list()
cholera_summaries <- list()

for(gt in c(1:6)){
  cholera_id[[gt]] <- read_latest(here("results"), paste0("res_cholera_sim_latest_id", gt))
  cholera_R[[gt]] <- read_latest(here("results"), paste0("res_cholera_sim_latest_R", gt))
  cholera_samples[[gt]] <- read_latest(here("results"), paste0("res_cholera_sim_latest_samples", gt))
  cholera_warnings[[gt]] <- read_latest(here("results"), paste0("res_cholera_sim_latest_warnings", gt))
  cholera_summaries[[gt]] <- read_latest(here("results"), paste0("res_cholera_sim_latest_summary", gt))
  # Add gt column
  cholera_id[[gt]]$gt <- gt
  cholera_R[[gt]]$gt <- gt
  cholera_samples[[gt]]$gt <- gt
  cholera_warnings[[gt]]$gt <- gt
  cholera_summaries[[gt]]$gt <- gt
}

cholera_id <- bind_rows(cholera_id)
cholera_R <- bind_rows(cholera_R)
cholera_samples <- bind_rows(cholera_samples)
# cholera_summaries <- bind_rows(cholera_summaries)

save_latest(cholera_id, here("results/resim_cholera"), paste0("resim_cholera_latest_all_id"))
save_latest(cholera_R, here("results/resim_cholera"), paste0("resim_cholera_latest_all_R"))
save_latest(cholera_samples, here("results/resim_cholera"), paste0("resim_cholera_latest_all_samples"))
# save_latest(cholera_warnings, here("results/choleracase"), paste0("res_cholerascen", i, "_all_warnings"))
save_latest(cholera_summaries, here("results/resim_cholera"), paste0("resim_cholera_latest_all_summary"))








