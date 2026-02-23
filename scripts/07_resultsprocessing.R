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
    disease_diagnostics <- list()

    idx <- 1
    for(gt in 1:6){
      for(inc_val in 1:6){
        tryCatch({
          disease_id[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_", scen, "_", rt_opts, "_id", gt, inc_val))
          disease_R[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_", scen, "_", rt_opts, "_R", gt, inc_val))
          disease_samples[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_", scen, "_", rt_opts, "_samples", gt, inc_val))
          warnings_data <- read_latest(here("results"), paste0("res_", disease, "_", scen, "_", rt_opts, "_warnings", gt, inc_val))
          summary_data <- read_latest_list(here("results"), paste0("res_", disease, "_", scen, "_", rt_opts, "_summary", gt, inc_val))
          disease_diagnostics[[idx]] <- extract_diagnostics(summary_data)
          disease_diagnostics[[idx]]$gt <- gt
          disease_diagnostics[[idx]]$inc <- inc_val
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
      disease_diagnostics <- bind_rows(disease_diagnostics)
      if(length(disease_warnings) > 0) {
        disease_warnings <- bind_rows(disease_warnings)
      }

      save_latest(disease_id, here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_id"))
      save_latest(disease_R, here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_R"))
      save_latest(disease_samples, here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_samples"))
      save_latest(disease_warnings, here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_warnings"))
      save_latest(disease_diagnostics, here("results/sim"), paste0("res_", disease, "_", scen, "_", rt_opts, "_all_diagnostics"))
      message(paste("Saved:", disease, scen, "with", nrow(disease_id), "rows"))
    }
  }

  #### Case studies ####

  disease_id <- list()
  disease_R <- list()
  disease_samples <- list()
  disease_warnings <- list()
  disease_diagnostics <- list()

  idx <- 1
  for(gt in 1:6){
    for(inc_val in 1:6){
      tryCatch({
        disease_id[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_id", gt, inc_val))
        disease_R[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_R", gt, inc_val))
        disease_samples[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_samples", gt, inc_val))
        warnings_data <- read_latest(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_warnings", gt, inc_val))
        summary_data <- read_latest_list(here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_summary", gt, inc_val))
        disease_diagnostics[[idx]] <- extract_diagnostics(summary_data)
        disease_diagnostics[[idx]]$gt <- gt
        disease_diagnostics[[idx]]$inc <- inc_val
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
    disease_diagnostics <- bind_rows(disease_diagnostics)
    if(length(disease_warnings) > 0) {
      disease_warnings <- bind_rows(disease_warnings)
    }

    save_latest(disease_id, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_id"))
    save_latest(disease_R, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_R"))
    save_latest(disease_samples, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_samples"))
    save_latest(disease_warnings, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_warnings"))
    save_latest(disease_diagnostics, here("results/casestudy"), paste0("res_", disease, "_casestudy_", rt_opts, "_all_diagnostics"))
    message(paste("Saved:", disease, "casestudy with", nrow(disease_id), "rows"))
  }

  #### Resim (re-simulated disease-like data with known GT/INC) ####

  disease_id <- list()
  disease_R <- list()
  disease_samples <- list()
  disease_warnings <- list()
  disease_diagnostics <- list()

  idx <- 1
  for(gt in 1:6){
    for(inc_val in 1:6){
      tryCatch({
        disease_id[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_resim_", rt_opts, "_id", gt, inc_val))
        disease_R[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_resim_", rt_opts, "_R", gt, inc_val))
        disease_samples[[idx]] <- read_latest(here("results"), paste0("res_", disease, "_resim_", rt_opts, "_samples", gt, inc_val))
        warnings_data <- read_latest(here("results"), paste0("res_", disease, "_resim_", rt_opts, "_warnings", gt, inc_val))
        summary_data <- read_latest_list(here("results"), paste0("res_", disease, "_resim_", rt_opts, "_summary", gt, inc_val))
        disease_diagnostics[[idx]] <- extract_diagnostics(summary_data)
        disease_diagnostics[[idx]]$gt <- gt
        disease_diagnostics[[idx]]$inc <- inc_val
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
        message(paste("Missing:", disease, "resim gt=", gt, "inc=", inc_val))
      })
    }
  }

  if(length(disease_id) > 0){
    disease_id <- bind_rows(disease_id)
    disease_R <- bind_rows(disease_R)
    disease_samples <- bind_rows(disease_samples)
    disease_diagnostics <- bind_rows(disease_diagnostics)
    if(length(disease_warnings) > 0) {
      disease_warnings <- bind_rows(disease_warnings)
    }

    save_latest(disease_id, here("results/sim"), paste0("res_", disease, "_resim_", rt_opts, "_all_id"))
    save_latest(disease_R, here("results/sim"), paste0("res_", disease, "_resim_", rt_opts, "_all_R"))
    save_latest(disease_samples, here("results/sim"), paste0("res_", disease, "_resim_", rt_opts, "_all_samples"))
    save_latest(disease_warnings, here("results/sim"), paste0("res_", disease, "_resim_", rt_opts, "_all_warnings"))
    save_latest(disease_diagnostics, here("results/sim"), paste0("res_", disease, "_resim_", rt_opts, "_all_diagnostics"))
    message(paste("Saved:", disease, "resim with", nrow(disease_id), "rows"))
  }

  #### Weight priors ####

  for(vary in c("gt", "inc", "both")){
    for(weight_prior in c("TRUE", "FALSE")){

      file_prefix <- paste0("res_", disease, "_weightprior_", weight_prior, "_", vary, "_", rt_opts, "_")

      tryCatch({
        disease_id <- read_latest(here("results"), paste0(file_prefix, "id"))
        disease_R <- read_latest(here("results"), paste0(file_prefix, "R"))
        disease_samples <- read_latest(here("results"), paste0(file_prefix, "samples"))
        warnings_data <- read_latest(here("results"), paste0(file_prefix, "warnings"))
        summary_data <- read_latest_list(here("results"), paste0(file_prefix, "summary"))
        disease_diagnostics <- extract_diagnostics(summary_data)

        disease_id$vary <- vary
        disease_R$vary <- vary
        disease_samples$vary <- vary
        disease_diagnostics$vary <- vary

        out_prefix <- paste0("res_", disease, "_weightprior_", weight_prior, "_", vary, "_", rt_opts, "_all_")
        save_latest(disease_id, here("results/weightprior"), paste0(out_prefix, "id"))
        save_latest(disease_R, here("results/weightprior"), paste0(out_prefix, "R"))
        save_latest(disease_samples, here("results/weightprior"), paste0(out_prefix, "samples"))
        save_latest(warnings_data, here("results/weightprior"), paste0(out_prefix, "warnings"))
        save_latest(disease_diagnostics, here("results/weightprior"), paste0(out_prefix, "diagnostics"))
        message(paste("Saved:", disease, "weightprior", weight_prior, "vary", vary, "with", nrow(disease_id), "rows"))
      }, error = function(e) {
        message(paste("Skipping:", disease, "weightprior", weight_prior, "vary", vary, "-", conditionMessage(e)))
      })
    }
  }
}

message("\n=== Results processing complete ===\n")
