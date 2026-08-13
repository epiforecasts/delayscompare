## Replicate constant-Rt datasets for the horizon-sweep phase test. The resim
## trajectories all sit near R = 1, so they can't separate a phase effect from noise.
## The trivial constant-Rt scenarios give a clean sustained-phase contrast at matched
## |R - 1|: const_low = R 0.8 (sustained decline), const_hi = R 1.2 (sustained growth),
## each with the disease's own GT/delays. Comparing the GT-misspecification slope
## between them isolates whether the medium-horizon direction tracks epidemic phase.
##
## Saves data/<disease>_sim_data_const_low_rep<r>.rds and _const_hi_rep<r>.rds.
## N via env NREPS (default 5); constant-R origins are homogeneous so few reps suffice.

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))

NREPS <- as.integer(Sys.getenv("NREPS", "5"))
scen_R <- c(const_low = 0.8, const_hi = 1.2)
message("Simulating ", NREPS, " replicates per disease x {const_low, const_hi}")

sim_one <- function(disease, Rval) {
  d <- delays[[disease]]
  startdate <- startenddates[[disease]][["startdate"]]
  enddate   <- startenddates[[disease]][["enddate"]]
  gen_time   <- get(paste0(disease, "_gen_time"))
  inc_period <- get(paste0(disease, "_inc_period"))
  combined_delay <- if (exists(paste0(disease, "_reporting_delay")))
    inc_period + get(paste0(disease, "_reporting_delay")) else inc_period
  R <- data.frame(date = seq.Date(startdate, enddate, by = 1), R = Rval)
  simulate_infections(R = R, initial_infections = d$init_infs,
    generation_time = generation_time_opts(gen_time),
    delays = delay_opts(combined_delay),
    obs = obs_opts(family = "poisson", scale = Fixed(d$underreport)))
}

for (disease in c("ebola", "covid", "cholera")) {
  for (scen in names(scen_R)) {
    for (r in seq_len(NREPS)) {
      set.seed(2000 * match(disease, c("ebola","covid","cholera")) +
                 100 * match(scen, names(scen_R)) + r)
      sd <- sim_one(disease, scen_R[[scen]])
      saveRDS(sd, here("data", paste0(disease, "_sim_data_", scen, "_rep", r, ".rds")))
    }
  }
  message("  ", disease, ": ", NREPS, " reps x 2 scenarios saved")
}
message("Done.")
