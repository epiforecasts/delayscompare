## Controlled turning-point trajectories for the delay-misspecification mechanism test.
## Three R profiles over a fixed 280-day window with a single change at day 140:
##   flat  : R = 1.2 throughout                 (no change   -> predict no delay effect)
##   bend  : R = 1.5 -> 1.1  (stays above 1)    (R changes, no incidence peak)
##   turn  : R = 1.3 -> 0.7  (crosses 1, peak)  (R changes, incidence turning point)
## bend vs turn separates "R changing" from "R crossing 1": if bend shows the effect too,
## delays matter wherever R changes, not only at the peak. Dense forecast origins later
## march across the change so the per-origin error can be plotted against local dR.
##
## Step-up mirrors of bend and turn (bendup 1.0 -> 1.3, turnup 0.7 -> 1.3) separate the
## direction of misspecification from the direction of the change. Starting from the
## same 100 infections, R = 0.7 for 105 days would leave almost no cases at the change,
## so for these the initial infections are scaled to give about 1000 infections on the
## change day (the renewal process is linear in the initial infections).
##
## Saves data/<disease>_sim_data_<scen>_rep<r>.rds. N via env NREPS (default 3);
## env SCENS (comma-separated) limits which scenarios are simulated.

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))

NREPS   <- as.integer(Sys.getenv("NREPS", "3"))
NDAYS   <- 210L
CHANGE  <- 105L      # day of the R change (mid-window)
INIT    <- 100       # small fixed seed: keeps counts bounded across the growth phase
AT_CHANGE <- 1000    # target infections on the change day for the step-up scenarios
scens   <- c("flat", "bend", "turn", "bendup", "turnup")
run_scens <- strsplit(Sys.getenv("SCENS", "flat,bend,turn"), ",")[[1]]
message("Simulating ", NREPS, " replicates per disease x {", paste(run_scens, collapse=", "), "}")

## step profile: R = a before the change day, b after. Growth is kept mild so the
## pre-change phase does not blow up over ~100 days (covid's short GT gives a high r).
##   flat 1.1 (no change) | bend 1.3->1.05 (stays >1) | turn 1.3->0.7 (crosses 1, peak)
step_R <- function(scen, n, change) {
  ab <- switch(scen,
    flat = c(1.10, 1.10),
    bend = c(1.30, 1.00),
    turn = c(1.30, 0.70),
    bendup = c(1.00, 1.30),
    turnup = c(0.70, 1.30))
  c(rep(ab[1], change), rep(ab[2], n - change))[seq_len(n)]
}

sim_one <- function(disease, scen, init = INIT) {
  d <- delays[[disease]]
  start <- startenddates[[disease]][["startdate"]]
  dates <- seq.Date(start, by = 1, length.out = NDAYS)
  gen_time   <- get(paste0(disease, "_gen_time"))
  inc_period <- get(paste0(disease, "_inc_period"))
  combined_delay <- if (exists(paste0(disease, "_reporting_delay")))
    inc_period + get(paste0(disease, "_reporting_delay")) else inc_period
  R <- data.frame(date = dates, R = step_R(scen, NDAYS, CHANGE))
  simulate_infections(R = R, initial_infections = init,
    generation_time = generation_time_opts(gen_time),
    delays = delay_opts(combined_delay),
    obs = obs_opts(family = "poisson", scale = Fixed(d$underreport)))
}

for (disease in c("ebola", "covid", "cholera")) {
  for (scen in run_scens) {
    init <- INIT
    if (scen %in% c("bendup", "turnup")) {
      pilot <- sim_one(disease, scen, init = 1)
      inf <- pilot[pilot$variable == "infections", ]
      init <- AT_CHANGE / inf$value[CHANGE]
      message("  ", disease, " ", scen, ": initial infections ", signif(init, 3))
    }
    for (r in seq_len(NREPS)) {
      set.seed(3000 * match(disease, c("ebola","covid","cholera")) +
                 100 * match(scen, scens) + r)
      sd <- sim_one(disease, scen, init = init)
      saveRDS(sd, here("data", paste0(disease, "_sim_data_", scen, "_rep", r, ".rds")))
    }
  }
  message("  ", disease, ": ", NREPS, " reps x ", length(run_scens), " scenarios saved")
}
message("Done. True R: flat=1.1; bend 1.3->1.0; turn 1.3->0.7; bendup 1.0->1.3; turnup 0.7->1.3; change at day ", CHANGE)
