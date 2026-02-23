library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("scripts", "datacollect_casestudy.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
args <- commandArgs(trailingOnly = TRUE)

disease <- args[1] # "cholera", "covid" or "ebola"
print(paste("Disease:", disease))

vary <- args[2] # "gt", "inc", or "both"
print(paste("Vary:", vary))

wp_arg <- args[3] # "TRUE" or "FALSE"
wp <- as.logical(wp_arg)
if (is.na(wp)) stop("weight_prior must be 'TRUE' or 'FALSE', got: ", wp_arg)
print(paste("weight_prior:", wp))

rt_opts <- if (length(args) >= 4 && args[4] != "") args[4] else "latest"
print(paste("rt_opts:", rt_opts))

# Optional timepoint range for single-timepoint jobs
timepoint_start <- if (length(args) >= 5 && args[5] != "") as.numeric(args[5]) else NULL
timepoint_end <- if (length(args) >= 6 && args[6] != "") as.numeric(args[6]) else NULL
if (!is.null(timepoint_start)) print(paste("Timepoint start:", timepoint_start))
if (!is.null(timepoint_end)) print(paste("Timepoint end:", timepoint_end))

# Suffix for output files
tp_suffix <- if (!is.null(timepoint_start) || !is.null(timepoint_end)) {
  paste0("_tp", timepoint_start %||% 1, "-", timepoint_end %||% 8)
} else ""

if (!disease %in% names(delays)) {
  stop("Invalid disease. Must be one of: ", paste(names(delays), collapse=", "))
}

d <- delays[[disease]]
case_data <- casestudydata[[disease]]

############### SCENARIOS #################

wp_label <- toupper(as.character(wp))
print(paste("Running weight_prior =", wp_label, ", vary =", vary))

res_disease <- sim_weightprior(case_data=case_data,
                           vary=vary,
                           gen_mean_mean=d$gen[["mean"]],
                           gen_mean_sd=d$gen[["mean_sd"]],
                           gen_sd_mean=d$gen[["sd"]],
                           gen_sd_sd=d$gen[["sd_sd"]],
                           gen_max=d$gen[["max"]],
                           inc_mean_mean=d$inc[["mean"]],
                           inc_mean_sd=d$inc[["mean_sd"]],
                           inc_sd_mean=d$inc[["sd"]],
                           inc_sd_sd=d$inc[["sd_sd"]],
                           inc_max=d$inc[["max"]],
                           rep_mean_mean=d$rep[["mean"]],
                           rep_mean_sd=d$rep[["mean_sd"]],
                           rep_sd_mean=d$rep[["sd"]],
                           rep_sd_sd=d$rep[["sd_sd"]],
                           rep_max=d$rep[["max"]],
                           freq_fc=4,
                           weeks_inc=12,
                           rt_opts_choice=rt_opts,
                           weight_prior=wp,
                           obs_scale=d$underreport,
                           timepoint_start=timepoint_start,
                           timepoint_end=timepoint_end)

if (nrow(res_disease$samples) > 0) {
  prefix <- paste0("res_", disease, "_weightprior_", wp_label, "_", vary, "_", rt_opts, "_")
  save_latest(res_disease[[1]], here("results"), paste0(prefix, "samples", tp_suffix))
  save_latest(res_disease[[2]], here("results"), paste0(prefix, "id", tp_suffix))
  save_latest(res_disease[[3]], here("results"), paste0(prefix, "R", tp_suffix))
  save_latest(res_disease[[4]], here("results"), paste0(prefix, "summary", tp_suffix))
  save_latest(res_disease[[5]], here("results"), paste0(prefix, "warnings", tp_suffix))
  save_latest(res_disease[[6]], here("results"), paste0(prefix, "timing", tp_suffix))
}
