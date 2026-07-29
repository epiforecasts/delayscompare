library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
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

if (!disease %in% names(delays)) {
  stop("Invalid disease. Must be one of: ", paste(names(delays), collapse=", "))
}

# Optional: specify which timepoints to run (e.g., "1-4" or "5-8")
tp_arg <- var[5]
if (!is.na(tp_arg) && tp_arg != "all") {
  tp_parts <- strsplit(tp_arg, "-")[[1]]
  if (length(tp_parts) != 2) {
    stop("Invalid timepoint range format. Expected 'start-end' (e.g., '1-4')")
  }
  tp_start <- as.numeric(tp_parts[1])
  tp_end <- as.numeric(tp_parts[2])
  if (is.na(tp_start) || is.na(tp_end)) {
    stop("Invalid timepoint range: start and end must be numeric")
  }
  if (tp_start > tp_end) {
    stop("Invalid timepoint range: start must be <= end")
  }
  timepoint_range <- seq(tp_start, tp_end)
} else {
  timepoint_range <- NULL
}
print(paste("Timepoint range:", paste(timepoint_range, collapse="-")))

d <- delays[[disease]]

## Sampler settings (overridable via env vars for tighter re-runs)
adapt_delta <- as.numeric(Sys.getenv("ADAPT_DELTA", "0.99"))
max_treedepth <- as.integer(Sys.getenv("MAX_TREEDEPTH", "20"))
samples <- as.integer(Sys.getenv("SAMPLES", "3000"))
warmup <- as.integer(Sys.getenv("WARMUP", "250"))

## Load re-simulated disease-like data (from 05_simulatedata.R) ##
if (!file.exists(here("data", paste0(disease, "_sim_data.rds")))) {
  stop("Simulated data not found for ", disease,
       ". Please run 05_simulatedata.R first.")
}
sim_data <- read_latest(here("data"), paste0(disease, "_sim_data"))

# In required format for EpiNow2
sim_data_cases <- sim_data |> filter(variable == "reported_cases")
sim_data_cases <- sim_data_cases |>
  select(date, value) |>
  rename(confirm = value)

res_disease <- sim_scenarios(case_data = sim_data_cases,
                             gt,
                             inc,
                             gen_mean = d$gen[["mean"]],
                             gen_sd = d$gen[["sd"]],
                             gen_max = d$gen[["max"]],
                             inc_mean = d$inc[["mean"]],
                             inc_sd = d$inc[["sd"]],
                             inc_max = d$inc[["max"]],
                             rep_mean = d$rep[["mean"]],
                             rep_sd = d$rep[["sd"]],
                             rep_max = d$rep[["max"]],
                             freq_fc = 4,
                             weeks_inc = 12,
                             rt_opts_choice = rt_opts,
                             obs_scale = d$underreport,
                             timepoint_range = timepoint_range,
                             adapt_delta = adapt_delta,
                             max_treedepth = max_treedepth,
                             samples = samples,
                             warmup = warmup)

if (nrow(res_disease$samples) > 0) {
  save_scenario(res_disease, here("results", "raw"), disease, "resim", rt_opts, gt, inc)
}
