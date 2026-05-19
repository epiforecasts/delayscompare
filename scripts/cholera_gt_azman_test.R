## Test cholera GT estimation with the Azman et al. (2018) prior:
## mean=5, sd=8 → gamma shape ≈ 0.39, a very dispersed distribution.
## Use the actual Yemen cholera case-study segment so the result tells us
## what we need for the paper (not synthetic).

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("scripts", "datacollect_casestudy.R"))
source(here("R", "funcs_data.R"))

d <- delays$cholera
## Azman et al. (2018) cholera GT mean, with SD reduced from 8 to 4 so the
## resulting gamma has shape > 1 (otherwise the density singularity at 0
## makes HMC sampling intractable). Shape = 25/16 ≈ 1.56.
d$gen[["mean"]]    <- 5
d$gen[["sd"]]      <- 4
d$gen[["mean_sd"]] <- 0.5  # 10% prior SD on the mean
d$gen[["sd_sd"]]   <- 0.4  # 10% prior SD on the SD
d$gen[["max"]]     <- 35
case_data <- casestudydata$cholera

mean_m <- d$gen[["mean"]];  mean_s <- d$gen[["mean_sd"]]
sd_m   <- d$gen[["sd"]];    sd_s   <- d$gen[["sd_sd"]]
gmax   <- d$gen[["max"]]
shape   <- mean_m^2 / sd_m^2
rate    <- mean_m / sd_m^2
shape_s <- shape * sqrt((2*mean_s/mean_m)^2 + (2*sd_s/sd_m)^2)
rate_s  <- rate  * sqrt((mean_s/mean_m)^2     + (2*sd_s/sd_m)^2)
cat("Azman: shape =", round(shape,3), "(prior sd", round(shape_s,3), ")\n")
cat("       rate  =", round(rate, 3), "(prior sd", round(rate_s, 3), ")\n\n")

## tp=4 segment from Yemen cholera weekly data
freq_fc <- 4; weeks_inc <- 12
start_date <- min(case_data$date); end_date <- max(case_data$date)
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
cat("case_segment rows:", nrow(case_segment), "\n\n")

gen_time <- Gamma(shape = Normal(shape, shape_s),
                  rate  = Normal(rate,  rate_s),
                  max   = gmax)
inc <- LogNormal(mean=d$inc[["mean"]], sd=d$inc[["sd"]], max=d$inc[["max"]])
rep <- LogNormal(mean=d$rep[["mean"]], sd=d$rep[["sd"]], max=d$rep[["max"]])

t0 <- Sys.time()
res <- tryCatch(estimate_infections(
  case_segment,
  generation_time = generation_time_opts(gen_time, weight_prior = FALSE),
  delays = delay_opts(inc + rep, weight_prior = FALSE),
  obs = obs_opts(family = "negbin", scale = Fixed(d$underreport)),
  rt = rt_opts(future = "latest"),
  stan = stan_opts(samples = 1000, return_fit = FALSE,
                   control = list(adapt_delta = 0.95, max_treedepth = 12)),
  forecast = forecast_opts(horizon = 14),
  verbose = FALSE),
  error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
cat("elapsed:", round(as.numeric(Sys.time() - t0, units="secs"), 1), "s\n")
if (!is.null(res)) {
  summ <- tryCatch(summary(res, type = "parameters"), error = function(e) NULL)
  if (!is.null(summ)) {
    rh <- summ |> dplyr::filter(grepl("delay_params", variable))
    print(rh, n = 10)
  }
}
