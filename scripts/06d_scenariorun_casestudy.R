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

# Optional: specify which timepoints to run (e.g., "1-4" or "5-8")
tp_arg <- var[5]
if (!is.na(tp_arg) && tp_arg != "all") {
  tp_parts <- strsplit(tp_arg, "-")[[1]]
  timepoint_range <- seq(as.numeric(tp_parts[1]), as.numeric(tp_parts[2]))
} else {
  timepoint_range <- NULL
}
print(paste("Timepoint range:", paste(timepoint_range, collapse="-")))

# Suffix for filenames when using timepoint range
tp_suffix <- if(!is.null(timepoint_range)) paste0("_tp", min(timepoint_range), "-", max(timepoint_range)) else ""

## Universal parameter values ##
freq_fc <- 4
weeks_inc <- 12

## Load data ##

case_data <- casestudydata[[disease]]
d <- delays[[disease]]

############### SCENARIOS #################

## under-reporting=yes ##

  res_disease <- sim_scenarios(case_data=case_data,
                           gt,
                           inc,
                           gen_mean=d$gen[["mean"]],
                           gen_sd=d$gen[["sd"]],
                           gen_max = d$gen[["max"]],
                           inc_mean = d$inc[["mean"]],
                           inc_sd = d$inc[["sd"]],
                           inc_max = d$inc[["max"]],
                           rep_mean = d$rep[["mean"]],
                           rep_sd = d$rep[["sd"]],
                           rep_max = d$rep[["max"]],
                           freq_fc=freq_fc,
                           weeks_inc=weeks_inc,
                           rt_opts_choice=rt_opts,
                           obs_scale=d$underreport,
                           timepoint_range=timepoint_range)

save_latest(res_disease[[2]], here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_id", gt, inc, tp_suffix))
save_latest(res_disease[[4]], here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_summary", gt, inc, tp_suffix))
save_latest(res_disease[[5]], here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_warnings", gt, inc, tp_suffix))
save_latest(res_disease[[6]], here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_timing", gt, inc, tp_suffix))

## Saving samples only ##
save_latest(res_disease[[1]], here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_samples", gt, inc, tp_suffix))
save_latest(res_disease[[3]], here("results"), paste0("res_", disease, "_casestudy_", rt_opts, "_R", gt, inc, tp_suffix))
