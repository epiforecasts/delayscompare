## Probe cholera-GT init failures.
##
## Cholera literature GT (mean=8.51, sd=0.54) gives a Gamma shape ~248, an
## extremely peaked distribution. The wide-prior runs (both 25% and 10% prior
## SDs) failed to converge for this parameter — Stan reported repeated
## "Initialization failed". This script compares three parameterisations on
## a single timepoint to identify which (if any) reliably initialises.

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("scripts", "datacollect_casestudy.R"))
source(here("R", "funcs_data.R"))

d <- delays_widepriors$cholera
case_data <- casestudydata$cholera

## Use tp=4 (~midway, with enough data)
freq_fc <- 4; weeks_inc <- 12
start_date <- min(case_data$date)
end_date <- max(case_data$date)
target_dates <- seq.Date(start_date, end_date, by = paste(freq_fc, "weeks"))
scen_timepoints <- sapply(target_dates, function(t){
  av <- case_data$date[case_data$date >= t]
  if (length(av)) min(av) else as.Date(NA)
}) |> as.Date(origin="1970-01-01")
scen_timepoints <- scen_timepoints[!is.na(scen_timepoints)][-1]
if (length(scen_timepoints) > 8) scen_timepoints <- scen_timepoints[1:8]
tp <- 4
end_date <- scen_timepoints[tp]
case_segment <- case_data[case_data$date <= end_date, ]
case_segment <- case_segment[case_segment$date > (end_date - weeks_inc*7), ]
case_segment <- fill_missing(case_segment, missing_dates="ignore",
                             missing_obs="ignore", obs_column="confirm", by=NULL)
case_segment <- case_segment[order(case_segment$date), ]
cat("case_segment rows:", nrow(case_segment), "\n")

mean_m <- d$gen[["mean"]];  mean_s <- d$gen[["mean_sd"]]
sd_m   <- d$gen[["sd"]];    sd_s   <- d$gen[["sd_sd"]]
gmax   <- d$gen[["max"]]

shape   <- mean_m^2 / sd_m^2
rate    <- mean_m / sd_m^2
shape_s <- shape * sqrt((2*mean_s/mean_m)^2 + (2*sd_s/sd_m)^2)
rate_s  <- rate  * sqrt((mean_s/mean_m)^2     + (2*sd_s/sd_m)^2)

cat("shape =", shape, " (sd", shape_s, ")\n")
cat("rate  =", rate,  " (sd", rate_s,  ")\n")
cat("mean  =", mean_m," (sd", mean_s,  ")\n")
cat("sd    =", sd_m,  " (sd", sd_s,    ")\n\n")

run_one <- function(label, gen_time){
  inc <- LogNormal(mean=d$inc[["mean"]], sd=d$inc[["sd"]], max=d$inc[["max"]])
  rep <- LogNormal(mean=d$rep[["mean"]], sd=d$rep[["sd"]], max=d$rep[["max"]])
  cat("\n=== ", label, " ===\n")
  t0 <- Sys.time()
  res <- tryCatch(estimate_infections(
    case_segment,
    generation_time = generation_time_opts(gen_time, weight_prior = FALSE),
    delays = delay_opts(inc + rep, weight_prior = FALSE),
    obs = obs_opts(family = "negbin", scale = Fixed(d$underreport)),
    rt = rt_opts(future = "latest"),
    stan = stan_opts(samples = 1000, return_fit = FALSE,
                     control = list(adapt_delta = 0.99, max_treedepth = 15)),
    forecast = forecast_opts(horizon = 14),
    verbose = FALSE),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
  cat("elapsed:", round(as.numeric(Sys.time() - t0, units="secs"), 1), "s\n")
  if (!is.null(res)) {
    n_samp <- tryCatch(
      sum(!is.na(res$samples$value[res$samples$variable == "R"])),
      error = function(e) 0)
    rh <- tryCatch({
      diag <- res$summarised |> dplyr::filter(variable == "R") |> dplyr::pull(rhat)
      if (length(diag)) max(diag, na.rm = TRUE) else NA_real_
    }, error = function(e) NA_real_)
    cat("R-row samples:", n_samp, " max_rhat:", rh, "\n")
  } else {
    cat("(no result)\n")
  }
  invisible(res)
}

gt_shape_rate <- Gamma(shape = Normal(shape, shape_s),
                       rate  = Normal(rate,  rate_s),
                       max   = gmax)
res1 <- run_one("shape/rate Normal priors (current)", gt_shape_rate)

gt_mean_sd <- Gamma(mean = Normal(mean_m, mean_s),
                    sd   = Normal(sd_m,   sd_s),
                    max  = gmax)
res2 <- run_one("mean/sd Normal priors", gt_mean_sd)

## Wider SD prior on the sd parameter (cholera SD is tiny and the data can't
## tell it apart from zero — relax the prior so init has room to land).
gt_mean_sd_wide <- Gamma(mean = Normal(mean_m, mean_s),
                         sd   = Normal(sd_m,   sd_m * 0.5),
                         max  = gmax)
res3 <- run_one("mean/sd, SD prior at 50% (relaxed)", gt_mean_sd_wide)

## LogNormal prior on shape/rate — naturally positive so init lands in
## the feasible region. Convert (shape, shape_s) → log-mean / log-sd.
lognorm_pars <- function(m, s){
  v <- s^2; ml <- log(m^2 / sqrt(v + m^2)); sl <- sqrt(log(1 + v/m^2))
  list(meanlog = ml, sdlog = sl)
}
sp <- lognorm_pars(shape, shape_s)
rp <- lognorm_pars(rate,  rate_s)
gt_lognorm <- Gamma(shape = LogNormal(meanlog = sp$meanlog, sdlog = sp$sdlog),
                    rate  = LogNormal(meanlog = rp$meanlog, sdlog = rp$sdlog),
                    max   = gmax)
res4 <- run_one("shape/rate LogNormal priors (positive-bounded)", gt_lognorm)

## Last resort — declare the gen time as Fixed at the literature mean so we
## can quote a control for the regime-2 cholera GT case study.
gt_fixed <- Fixed(mean_m)
res5 <- run_one("Fixed at literature mean (no learning)", gt_fixed)
