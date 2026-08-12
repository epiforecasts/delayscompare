## Replicate disease-like datasets for the horizon-sweep power analysis. Simulates
## N independent noise realisations per disease from the same fitted Rt trajectory
## (rt_<disease>.rds), so the medium-horizon misspecification effect can be resolved
## against simulation noise. Saves data/<disease>_sim_data_rep<r>.rds.
##
## N via env var NREPS (default 30).

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))

NREPS <- as.integer(Sys.getenv("NREPS", "30"))
message("Simulating ", NREPS, " replicates per disease")

## Per-disease simulate_infections configuration (mirrors 05_simulatedata.R)
rt_col <- function(rt) if ("R" %in% names(rt)) "R" else "median"
load_rt <- function(disease) {
  rt <- readRDS(here("data", paste0("rt_", disease, ".rds")))
  data.frame(date = rt$date, R = rt[[rt_col(rt)]])
}

sim_one <- function(disease) {
  R <- load_rt(disease)
  if (disease == "ebola") {
    simulate_infections(R = R, initial_infections = 5,
      generation_time = generation_time_opts(ebola_gen_time),
      delays = delay_opts(fix_parameters(ebola_inc_period)),
      obs = obs_opts(family = "poisson", scale = Fixed(0.83)))
  } else if (disease == "covid") {
    simulate_infections(R = R, initial_infections = 2500,
      generation_time = generation_time_opts(covid_gen_time),
      delays = delay_opts(covid_inc_period + covid_reporting_delay),
      obs = obs_opts(family = "poisson", scale = Fixed(0.4)))
  } else if (disease == "cholera") {
    simulate_infections(R = R, initial_infections = 75,
      generation_time = generation_time_opts(cholera_gen_time),
      delays = delay_opts(combined_delay_cholera),
      obs = obs_opts(family = "poisson", scale = Fixed(0.28)))
  }
}

for (disease in c("ebola", "covid", "cholera")) {
  for (r in seq_len(NREPS)) {
    set.seed(1000 * match(disease, c("ebola","covid","cholera")) + r)  # reproducible per disease/rep
    sd <- sim_one(disease)
    saveRDS(sd, here("data", paste0(disease, "_sim_data_rep", r, ".rds")))
  }
  message("  ", disease, ": ", NREPS, " replicates saved")
}
message("Done.")
