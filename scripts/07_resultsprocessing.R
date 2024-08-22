
library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

######################
#### ebola-LIKE ####
######################

ebola_id <- list()
ebola_R <- list()
ebola_samples <- list()
ebola_warnings <- list()

for(gt in c(1:6)){
  ebola_id[[gt]] <- read_latest(here("results"), paste0("res_ebolascen1_id", gt))
  ebola_R[[gt]] <- read_latest(here("results"), paste0("res_ebolascen1_R", gt))
  ebola_samples[[gt]] <- read_latest(here("results"), paste0("res_ebolascen1_samples", gt))
  ebola_warnings[[gt]] <- read_latest(here("results"), paste0("res_ebolascen1_warnings", gt))
  # Add get column
  ebola_id[[gt]]$gt <- gt
  ebola_R[[gt]]$gt <- gt
  ebola_samples[[gt]]$gt <- gt
  ebola_warnings[[gt]]$gt <- gt
}

ebola_id <- bind_rows(ebola_id)
ebola_R <- bind_rows(ebola_R)
ebola_samples <- bind_rows(ebola_samples)

save_latest(ebola_id, here("results"), "ebolascen3_id")
save_latest(ebola_R, here("results"), "ebolascen3_R")
save_latest(ebola_samples, here("results"), "ebolascen3_samples")
save_latest(ebola_warnings, here("results"), "ebolascen3_warnings")








