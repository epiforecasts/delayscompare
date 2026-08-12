## Horizon-sweep runner: fits one estimate_infections forecast per
## (disease, GT, INC, timepoint, replicate) on a replicate disease-like dataset, with
## an extended forecast horizon (HORIZON env, default 42d) so CRPS can be scored at
## 1-6 week horizons. INC is held correct (=4) in the reduced design; GT is varied.
## Output scenario label "hz_rep<r>" keeps replicates separate.

library(here)

source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))
source(here("R", "scenario_loop.R"))

var <- commandArgs(trailingOnly = T)
gt      <- as.numeric(var[1]); print(gt)
inc     <- as.numeric(var[2]); print(inc)
rt_opts <- var[3]
disease <- var[4]; print(disease)

if (!disease %in% names(delays)) {
  stop("Invalid disease. Must be one of: ", paste(names(delays), collapse=", "))
}

# timepoint range ("k-k")
tp_arg <- var[5]
if (!is.na(tp_arg) && tp_arg != "all") {
  tp_parts <- strsplit(tp_arg, "-")[[1]]
  timepoint_range <- seq(as.numeric(tp_parts[1]), as.numeric(tp_parts[2]))
} else {
  timepoint_range <- NULL
}

# replicate index
rep_id <- as.integer(var[6])
if (is.na(rep_id)) stop("Missing replicate id (arg 6)")

d <- delays[[disease]]

adapt_delta   <- as.numeric(Sys.getenv("ADAPT_DELTA", "0.99"))
max_treedepth <- as.integer(Sys.getenv("MAX_TREEDEPTH", "20"))
samples       <- as.integer(Sys.getenv("SAMPLES", "3000"))
warmup        <- as.integer(Sys.getenv("WARMUP", "500"))
horizon       <- as.integer(Sys.getenv("HORIZON", "42"))

sim_file <- here("data", paste0(disease, "_sim_data_rep", rep_id, ".rds"))
if (!file.exists(sim_file)) {
  stop("Replicate data not found: ", sim_file, ". Run 05d_simulatedata_replicates.R first.")
}
sim_data <- readRDS(sim_file)

sim_data_cases <- sim_data |> filter(variable == "reported_cases") |>
  select(date, value) |> rename(confirm = value)

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
                             warmup = warmup,
                             horizon = horizon)

if (nrow(res_disease$samples) > 0) {
  save_scenario(res_disease, here("results", "raw"), disease,
                paste0("hz_rep", rep_id), rt_opts, gt, inc)
}
