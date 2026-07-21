#####################################
#### Wide-prior result aggregation ####
#####################################

# Aggregates per-timepoint widepriors files (one task per tp × vary) into
# combined per-(disease, vary) files under results/widepriors/, mirroring
# the layout used by results/weightprior/. weight_prior is FALSE only.
# Missing timepoints (failed/cancelled SLURM tasks) are skipped silently.

library(here)
source(here("scripts", "01_packages.R"))
source(here("R", "funcs_data.R"))

rt_opts <- "latest"
wp <- "FALSE"

dir.create(here("results/widepriors"), showWarnings = FALSE, recursive = TRUE)

for(disease in c("ebola", "covid", "cholera")){
  n_tp <- if(disease == "covid") 6 else 8

  for(vary in c("gt", "inc", "both")){
    prefix <- paste0("res_", disease, "_widepriors_", wp, "_", vary, "_", rt_opts, "_")

    id_list <- list(); R_list <- list(); samples_list <- list()
    warn_list <- list(); diag_list <- list(); timing_list <- list()

    for(tp in seq_len(n_tp)){
      suf <- paste0("_tp", tp, "-", tp)
      tryCatch({
        id_tp      <- read_latest(here("results"), paste0(prefix, "id", suf))
        R_tp       <- read_latest(here("results"), paste0(prefix, "R", suf))
        samples_tp <- read_latest(here("results"), paste0(prefix, "samples", suf))

        # Convergence filter: exclude this timepoint from samples/R/id if its fit is
        # poorly converged (max R-hat >= 1.05 or >= 100 divergences), matching the
        # impose-arm criterion. Keep the diagnostics record, flagged `converged`.
        diag_tp <- tryCatch(read_latest(here("results"), paste0(prefix, "diagnostics", suf)),
                            error = function(e) NULL)
        poor <- FALSE
        if(!is.null(diag_tp) && is.data.frame(diag_tp) && nrow(diag_tp) > 0){
          poor <- (is.finite(diag_tp$max_rhat) & diag_tp$max_rhat > 1.05) |
                  (is.finite(diag_tp$num_divergent) & diag_tp$num_divergent > 100)
          diag_tp$converged <- !poor
          diag_list[[tp]] <- diag_tp
        }

        if(!any(poor)){
          id_list[[tp]]      <- id_tp
          R_list[[tp]]       <- R_tp
          samples_list[[tp]] <- samples_tp

          warn <- tryCatch(read_latest(here("results"), paste0(prefix, "warnings", suf)),
                           error = function(e) NULL)
          if(!is.null(warn) && is.data.frame(warn)) warn_list[[tp]] <- warn

          tim <- tryCatch(read_latest(here("results"), paste0(prefix, "timing", suf)),
                          error = function(e) NULL)
          if(!is.null(tim) && is.data.frame(tim)) timing_list[[tp]] <- tim
        } else {
          message(sprintf("  Excluding %s vary=%s tp=%d (non-converged)", disease, vary, tp))
        }
      }, error = function(e){
        message(sprintf("  Missing: %s vary=%s tp=%d", disease, vary, tp))
      })
    }

    if(length(id_list) == 0){
      message(sprintf("Skip: %s vary=%s (no timepoints)", disease, vary))
      next
    }

    id_df      <- bind_rows(id_list)
    R_df       <- bind_rows(R_list)
    samples_df <- bind_rows(samples_list)
    diag_df    <- if(length(diag_list))   bind_rows(diag_list)    else NULL

    id_df$vary <- vary
    R_df$vary <- vary
    samples_df$vary <- vary
    if(!is.null(diag_df)) diag_df$vary <- vary

    out <- paste0(prefix, "all_")
    save_latest(id_df,      here("results/widepriors"), paste0(out, "id"))
    save_latest(R_df,       here("results/widepriors"), paste0(out, "R"))
    save_latest(samples_df, here("results/widepriors"), paste0(out, "samples"))
    if(!is.null(diag_df))
      save_latest(diag_df,  here("results/widepriors"), paste0(out, "diagnostics"))
    if(length(warn_list))
      save_latest(bind_rows(warn_list), here("results/widepriors"), paste0(out, "warnings"))
    if(length(timing_list)){
      tim_df <- bind_rows(timing_list); tim_df$vary <- vary
      save_latest(tim_df, here("results/timing"), paste0(out, "timing"))
    }

    n_tp_kept <- length(id_list)
    message(sprintf("Saved: %s vary=%s — %d/%d timepoints, %d id rows",
                    disease, vary, n_tp_kept, n_tp, nrow(id_df)))
  }
}

message("\n=== Wide-prior aggregation complete ===\n")
