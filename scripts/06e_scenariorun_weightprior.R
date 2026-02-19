library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("scripts", "datacollect_casestudy.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

## Load argument(s) ##
var <- commandArgs(trailingOnly = T)
gt <- as.numeric(var[1])
print(gt)

inc <- as.numeric(var[2])
print(inc)

rt_opts <- var[3] # "latest" or "project"
print(rt_opts)

disease <- var[4] # "cholera", "covid" or "ebola"
print(disease)

# Optional timepoint range for splitting long jobs
timepoint_start <- if (length(var) >= 5 && var[5] != "") as.numeric(var[5]) else NULL
timepoint_end <- if (length(var) >= 6 && var[6] != "") as.numeric(var[6]) else NULL
if (!is.null(timepoint_start)) print(paste("Timepoint start:", timepoint_start))
if (!is.null(timepoint_end)) print(paste("Timepoint end:", timepoint_end))

# Suffix for output files when splitting
tp_suffix <- if (!is.null(timepoint_start) || !is.null(timepoint_end)) {
  paste0("_tp", timepoint_start %||% 1, "-", timepoint_end %||% 8)
} else ""

if (!disease %in% names(delays)) {
  stop("Invalid disease. Must be one of: ", paste(names(delays), collapse=", "))
}

d <- delays[[disease]]

## Load data ##

case_data <- casestudydata[[disease]]
d <- delays[[disease]]

############### SCENARIOS #################

## under-reporting=yes, weight_prior=TRUE ##

res_disease <- sim_weightprior(case_data=case_data,
                           gt,
                           inc,
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
                           weight_prior=TRUE,
                           obs_scale=d$underreport,
                           timepoint_start=timepoint_start,
                           timepoint_end=timepoint_end)

if (nrow(res_disease$samples) > 0) {
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_samples", gt, inc, tp_suffix))
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_id", gt, inc, tp_suffix))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_R", gt, inc, tp_suffix))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_summary", gt, inc, tp_suffix))
  save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_warnings", gt, inc, tp_suffix))
  save_latest(res_disease[[6]], here("results"), paste0("res_", disease, "_weightprior_TRUE_", rt_opts, "_timing", gt, inc, tp_suffix))
}

## under-reporting=yes, weight_prior=FALSE ##

res_disease <- sim_weightprior(case_data=case_data,
                               gt,
                               inc,
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
                               weight_prior=FALSE,
                               obs_scale=d$underreport,
                               timepoint_start=timepoint_start,
                               timepoint_end=timepoint_end)

if (nrow(res_disease$samples) > 0) {
  save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_samples", gt, inc, tp_suffix))
  save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_id", gt, inc, tp_suffix))
  save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_R", gt, inc, tp_suffix))
  save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_summary", gt, inc, tp_suffix))
  save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_warnings", gt, inc, tp_suffix))
  save_latest(res_disease[[6]], here("results"), paste0("res_", disease, "_weightprior_FALSE_", rt_opts, "_timing", gt, inc, tp_suffix))
}