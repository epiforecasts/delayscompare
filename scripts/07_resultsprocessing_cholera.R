library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

####################
#### CHOLERA-LIKE ####
####################

for(i in 1:16){

cholera_id <- list()
cholera_R <- list()
cholera_samples <- list()
cholera_warnings <- list()

for(gt in c(1:6)){
  cholera_id[[gt]] <- read_latest(here("results"), paste0("res_cholerascen", i, "_id", gt))
  cholera_R[[gt]] <- read_latest(here("results"), paste0("res_cholerascen", i, "_R", gt))
  cholera_samples[[gt]] <- read_latest(here("results"), paste0("res_cholerascen", i, "_samples", gt))
  cholera_warnings[[gt]] <- read_latest(here("results"), paste0("res_cholerascen", i, "_warnings", gt))
  # Add get column
  cholera_id[[gt]]$gt <- gt
  cholera_R[[gt]]$gt <- gt
  cholera_samples[[gt]]$gt <- gt
  cholera_warnings[[gt]]$gt <- gt
}

cholera_id <- bind_rows(cholera_id)
cholera_R <- bind_rows(cholera_R)
cholera_samples <- bind_rows(cholera_samples)

save_latest(cholera_id, here("results/cholera"), paste0("res_cholerascen", i, "_all_id"))
save_latest(cholera_R, here("results/cholera"), paste0("res_cholerascen", i, "_all_R"))
save_latest(cholera_samples, here("results/cholera"), paste0("res_cholerascen", i, "_all_samples"))
save_latest(cholera_warnings, here("results/cholera"), paste0("res_cholerascen", i, "_all_warnings"))

}








