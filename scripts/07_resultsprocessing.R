library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

disease <- "ebola"
rt_opts <- "latest"

#### Simulated data scenarios ####

for(i in c("project", "latest")){
  
  rt_opts <- i
  
  disease_id <- list()
  disease_R <- list()
  disease_samples <- list()
  disease_warnings <- list()
  
  for(gt in c(1:6)){
    disease_id[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_sim_", rt_opts, "_id", gt))
    disease_R[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_sim_", rt_opts, "_R", gt))
    disease_samples[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_sim_", rt_opts, "_samples", gt))
    disease_warnings[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_sim_", rt_opts, "_warnings", gt))
    # Add get column
    disease_id[[gt]]$gt <- gt
    disease_R[[gt]]$gt <- gt
    disease_samples[[gt]]$gt <- gt
    disease_warnings[[gt]]$gt <- gt
  }
  
  disease_id <- bind_rows(disease_id)
  disease_R <- bind_rows(disease_R)
  disease_samples <- bind_rows(disease_samples)
  
  save_latest(disease_id, here("results/sim"), paste0("res_", disease, "_sim_", rt_opts, "_scen", i, "_all_id"))
  save_latest(disease_R, here("results/sim"), paste0("res_", disease, "_sim_", rt_opts, "_scen", i, "_all_R"))
  save_latest(disease_samples, here("results/sim"), paste0("res_", disease, "_sim_", rt_opts, "_scen", i, "_all_samples"))
  save_latest(disease_warnings, here("results/sim"), paste0("res_", disease, "_sim_", rt_opts, "_scen", i, "_all_warnings"))
  
}

### Scenarios using resimulated data ###

for(i in c("project", "latest")){
  
  rt_opts <- i

disease_id <- list()
disease_R <- list()
disease_samples <- list()
disease_warnings <- list()
disease_summaries <- list()

for(gt in c(1:6)){
  disease_id[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_resim_", rt_opts, "_id", gt))
  disease_R[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_resim_", rt_opts, "_R", gt))
  disease_samples[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_resim_", rt_opts, "_samples", gt))
  disease_warnings[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_resim_", rt_opts, "_warnings", gt))
  disease_summaries[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_resim_", rt_opts, "_summary", gt))
  # Add gt column
  disease_id[[gt]]$gt <- gt
  disease_R[[gt]]$gt <- gt
  disease_samples[[gt]]$gt <- gt
  disease_warnings[[gt]]$gt <- gt
  disease_summaries[[gt]]$gt <- gt
}

disease_id <- bind_rows(disease_id)
disease_R <- bind_rows(disease_R)
disease_samples <- bind_rows(disease_samples)
# disease_summaries <- bind_rows(disease_summaries)

save_latest(disease_id, here("results/resim"), paste0(disease, "_resim_", rt_opts, "_all_id"))
save_latest(ebola_R, here("results/resim"), paste0(disease, "_resim_", rt_opts, "_all_R"))
save_latest(ebola_samples, here("results/resim"),  paste0(disease, "_resim_", rt_opts, "_all_samples"))
save_latest(ebola_summaries, here("results/resim"),  paste0(disease, "_resim_", rt_opts, "_all_summary"))

}

#### Case studies ####

for(i in c("project", "latest")){
  
  rt_opts <- i
  
  disease_id <- list()
  disease_R <- list()
  disease_samples <- list()
  disease_warnings <- list()
  
  for(gt in c(1:6)){
    disease_id[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_id", gt))
    disease_R[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_R", gt))
    disease_samples[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_samples", gt))
    disease_warnings[[gt]] <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts,  "_warnings", gt))
    # Add get column
    disease_id[[gt]]$gt <- gt
    disease_R[[gt]]$gt <- gt
    disease_samples[[gt]]$gt <- gt
    disease_warnings[[gt]]$gt <- gt
  }
  
  disease_id <- bind_rows(disease_id)
  disease_R <- bind_rows(disease_R)
  disease_samples <- bind_rows(disease_samples)
  
  save_latest(disease_id, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts,  "_all_id"))
  save_latest(disease_R, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_R"))
  save_latest(disease_samples, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_samples"))
  save_latest(disease_warnings, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_warnings"))
  
}

#### Weight priors ####

for(rt_opts in c("project", "latest")){
  
  disease_id <- list()
  disease_R <- list()
  disease_samples <- list()
  disease_warnings <- list()
  
  for(gt in c(1:6)){
    disease_id[[gt]] <- read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_id", gt))
    disease_R[[gt]] <- read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_R", gt))
    disease_samples[[gt]] <- read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_samples", gt))
    disease_warnings[[gt]] <- read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_warnings", gt))
    # Add get column
    disease_id[[gt]]$gt <- gt
    disease_R[[gt]]$gt <- gt
    disease_samples[[gt]]$gt <- gt
    disease_warnings[[gt]]$gt <- gt
  }
  
  disease_id <- bind_rows(disease_id)
  disease_R <- bind_rows(disease_R)
  disease_samples <- bind_rows(disease_samples)
  
  save_latest(disease_id, here("results/weightprior"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_all_id"))
  save_latest(disease_R, here("results/weightprior"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_all_R"))
  save_latest(disease_samples, here("results/weightprior"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_all_samples"))
  save_latest(disease_warnings, here("results/weightprior"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts,  "_all_warnings"))
  
}


for(rt_opts in c("project", "latest")){
  
  disease_id <- list()
  disease_R <- list()
  disease_samples <- list()
  disease_warnings <- list()
  
  for(gt in c(1:6)){
    disease_id[[gt]] <- read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_id", gt))
    disease_R[[gt]] <- read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_R", gt))
    disease_samples[[gt]] <- read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_samples", gt))
    disease_warnings[[gt]] <- read_latest(here("results/weightprior"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_warnings", gt))
    # Add get column
    disease_id[[gt]]$gt <- gt
    disease_R[[gt]]$gt <- gt
    disease_samples[[gt]]$gt <- gt
    disease_warnings[[gt]]$gt <- gt
  }
  
  disease_id <- bind_rows(disease_id)
  disease_R <- bind_rows(disease_R)
  disease_samples <- bind_rows(disease_samples)
  
  save_latest(disease_id, here("results/weightprior"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_all_id"))
  save_latest(disease_R, here("results/weightprior"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_all_R"))
  save_latest(disease_samples, here("results/weightprior"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_all_samples"))
  save_latest(disease_warnings, here("results/weightprior"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts,  "_all_warnings"))
  
}


