library(here)

source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

rt_opts <- "latest"

# Process all diseases
for(disease in c("ebola", "covid", "cholera")){

  message(paste("\n=== Processing", disease, "===\n"))

  #### Simulated data scenarios ####

  for(scen in c("const_low", "const_high", "inc", "dec")){

    disease_id <- list()
    disease_R <- list()
    disease_samples <- list()
    disease_warnings <- list()

    idx <- 1
    for(gt in 1:6){
      for(inc_val in 1:6){
        tryCatch({
          disease_id[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_", scen, "_", rt_opts, "_id", gt, inc_val))
          disease_R[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_", scen, "_", rt_opts, "_R", gt, inc_val))
          disease_samples[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_", scen, "_", rt_opts, "_samples", gt, inc_val))
          warnings_data <- read_latest(here("results"), paste0("res_", disease, "_", scen, "_", rt_opts, "_warnings", gt, inc_val))
          # Add gt and inc columns
          disease_id[[idx]]$gt <- gt
          disease_id[[idx]]$inc <- inc_val
          disease_R[[idx]]$gt <- gt
          disease_R[[idx]]$inc <- inc_val
          disease_samples[[idx]]$gt <- gt
          disease_samples[[idx]]$inc <- inc_val
          # Handle NULL warnings
          if(!is.null(warnings_data) && is.data.frame(warnings_data)) {
            warnings_data$gt <- gt
            warnings_data$inc <- inc_val
            disease_warnings[[idx]] <- warnings_data
          }
          idx <- idx + 1
        }, error = function(e) {
          message(paste("Missing:", disease, scen, "gt=", gt, "inc=", inc_val))
        })
      }
    }

    if(length(disease_id) > 0){
      disease_id <- bind_rows(disease_id)
      disease_R <- bind_rows(disease_R)
      disease_samples <- bind_rows(disease_samples)
      if(length(disease_warnings) > 0) {
        disease_warnings <- bind_rows(disease_warnings)
      }

      save_latest(disease_id, here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_id"))
      save_latest(disease_R, here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_R"))
      save_latest(disease_samples, here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_samples"))
      save_latest(disease_warnings, here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_warnings"))
      message(paste("Saved:", disease, scen, "with", nrow(disease_id), "rows"))
    }
  }

  #### Case studies ####

  disease_id <- list()
  disease_R <- list()
  disease_samples <- list()
  disease_warnings <- list()

  idx <- 1
  for(gt in 1:6){
    for(inc_val in 1:6){
      tryCatch({
        disease_id[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_id", gt, inc_val))
        disease_R[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_R", gt, inc_val))
        disease_samples[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_samples", gt, inc_val))
        warnings_data <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_warnings", gt, inc_val))
        # Add gt and inc columns
        disease_id[[idx]]$gt <- gt
        disease_id[[idx]]$inc <- inc_val
        disease_R[[idx]]$gt <- gt
        disease_R[[idx]]$inc <- inc_val
        disease_samples[[idx]]$gt <- gt
        disease_samples[[idx]]$inc <- inc_val
        # Handle NULL warnings
        if(!is.null(warnings_data) && is.data.frame(warnings_data)) {
          warnings_data$gt <- gt
          warnings_data$inc <- inc_val
          disease_warnings[[idx]] <- warnings_data
        }
        idx <- idx + 1
      }, error = function(e) {
        message(paste("Missing:", disease, "casestudy gt=", gt, "inc=", inc_val))
      })
    }
  }

  if(length(disease_id) > 0){
    disease_id <- bind_rows(disease_id)
    disease_R <- bind_rows(disease_R)
    disease_samples <- bind_rows(disease_samples)
    if(length(disease_warnings) > 0) {
      disease_warnings <- bind_rows(disease_warnings)
    }

    save_latest(disease_id, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_id"))
    save_latest(disease_R, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_R"))
    save_latest(disease_samples, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_samples"))
    save_latest(disease_warnings, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_warnings"))
    message(paste("Saved:", disease, "casestudy with", nrow(disease_id), "rows"))
  }

  #### Weight priors ####

  for(weight_prior in c("TRUE", "FALSE")){

    disease_id <- list()
    disease_R <- list()
    disease_samples <- list()
    disease_warnings <- list()

    idx <- 1
    for(gt in 1:6){
      for(inc_val in 1:6){
        tryCatch({
          disease_id[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_weightprior_", weight_prior, rt_opts, "_id", gt, inc_val))
          disease_R[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_weightprior_", weight_prior, rt_opts, "_R", gt, inc_val))
          disease_samples[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_weightprior_", weight_prior, rt_opts, "_samples", gt, inc_val))
          warnings_data <- read_latest(here("results"), paste0("res_", disease, "_weightprior_", weight_prior, rt_opts, "_warnings", gt, inc_val))
          # Add gt and inc columns
          disease_id[[idx]]$gt <- gt
          disease_id[[idx]]$inc <- inc_val
          disease_R[[idx]]$gt <- gt
          disease_R[[idx]]$inc <- inc_val
          disease_samples[[idx]]$gt <- gt
          disease_samples[[idx]]$inc <- inc_val
          # Handle NULL warnings
          if(!is.null(warnings_data) && is.data.frame(warnings_data)) {
            warnings_data$gt <- gt
            warnings_data$inc <- inc_val
            disease_warnings[[idx]] <- warnings_data
          }
          idx <- idx + 1
        }, error = function(e) {
          # Silently skip missing weightprior files
        })
      }
    }

    if(length(disease_id) > 0){
      disease_id <- bind_rows(disease_id)
      disease_R <- bind_rows(disease_R)
      disease_samples <- bind_rows(disease_samples)
      if(length(disease_warnings) > 0) {
        disease_warnings <- bind_rows(disease_warnings)
      }

      save_latest(disease_id, here("results/weightprior"), paste0("res_", disease, "_weightprior_", weight_prior, "_", rt_opts, "_all_id"))
      save_latest(disease_R, here("results/weightprior"), paste0("res_", disease, "_weightprior_", weight_prior, "_", rt_opts, "_all_R"))
      save_latest(disease_samples, here("results/weightprior"), paste0("res_", disease, "_weightprior_", weight_prior, "_", rt_opts, "_all_samples"))
      save_latest(disease_warnings, here("results/weightprior"), paste0("res_", disease, "_weightprior_", weight_prior, "_", rt_opts, "_all_warnings"))
      message(paste("Saved:", disease, "weightprior", weight_prior, "with", nrow(disease_id), "rows"))
    }
  }
}

message("\n=== Results processing complete ===\n")
