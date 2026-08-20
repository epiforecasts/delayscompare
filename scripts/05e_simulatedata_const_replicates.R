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
## Trivial-trajectory scenarios for the horizon mechanism test. const_low/const_hi are
## constant R (zero curvature); inc/dec ramp R linearly 0.8<->1.2 (changing growth rate,
## nonzero curvature). Ordered so const seeds are unchanged when inc/dec are added.
scens <- c("const_low", "const_hi", "inc", "dec")
message("Simulating ", NREPS, " replicates per disease x {", paste(scens, collapse=", "), "}")

## per-scenario R(date) builder
build_R <- function(scen, dates) {
  n <- length(dates)
  Rval <- switch(scen,
    const_low = rep(0.8, n),
    const_hi  = rep(1.2, n),
    inc       = seq(0.8, 1.2, length.out = n),
    dec       = rev(seq(0.8, 1.2, length.out = n)))
  data.frame(date = dates, R = Rval)
}

sim_one <- function(disease, scen) {
  d <- delays[[disease]]
  dates <- seq.Date(startenddates[[disease]][["startdate"]],
                    startenddates[[disease]][["enddate"]], by = 1)
  gen_time   <- get(paste0(disease, "_gen_time"))
  inc_period <- get(paste0(disease, "_inc_period"))
  combined_delay <- if (exists(paste0(disease, "_reporting_delay")))
    inc_period + get(paste0(disease, "_reporting_delay")) else inc_period
  simulate_infections(R = build_R(scen, dates), initial_infections = d$init_infs,
    generation_time = generation_time_opts(gen_time),
    delays = delay_opts(combined_delay),
    obs = obs_opts(family = "poisson", scale = Fixed(d$underreport)))
}

for (disease in c("ebola", "covid", "cholera")) {
  for (scen in scens) {
    for (r in seq_len(NREPS)) {
      set.seed(2000 * match(disease, c("ebola","covid","cholera")) +
                 100 * match(scen, scens) + r)
      sd <- sim_one(disease, scen)
      saveRDS(sd, here("data", paste0(disease, "_sim_data_", scen, "_rep", r, ".rds")))
    }
  }
  message("  ", disease, ": ", NREPS, " reps x ", length(scens), " scenarios saved")
}
message("Done.")
