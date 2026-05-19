## Test whether cholera GT (mean=8.51, sd=0.54) can be recovered from data
## generated *with that exact GT*. If yes, the Yemen-data failure is an
## identifiability/data-quality issue. If no, it points at a bug or limitation
## in the framework's Gamma handling.
##
## Three conditions:
##   A. Daily cholera-like synthetic data → estimate cholera GT
##   B. Daily Ebola-like synthetic data, swap in cholera GT for generation → estimate cholera GT
##   C. Weekly-aggregated cholera-like synthetic data → estimate cholera GT
## In all cases we set shape ≈ 248 via the literature mean/sd.

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
source(here("R", "funcs_data.R"))

set.seed(1)

## Generate clean daily case data from a known SEIR-like process driven by Rt.
## We use a simple convolutional approach: Rt = 1.2 constant, sufficient cases.
gen_synthetic <- function(n_days = 84, R0 = 1.2, init = 500, gen_mean = 8.51, gen_sd = 0.54){
  gen_max <- 25
  k <- 0:gen_max
  shape <- gen_mean^2 / gen_sd^2
  rate  <- gen_mean / gen_sd^2
  pmf <- pgamma(k+1, shape=shape, rate=rate) - pgamma(k, shape=shape, rate=rate)
  pmf <- pmf / sum(pmf)
  I <- c(init, rep(0, n_days - 1))
  for (t in 2:n_days){
    look <- min(gen_max, t-1)
    inf <- sum(I[(t-1):(t-look)] * pmf[2:(look+1)])
    I[t] <- R0 * inf
  }
  data.frame(date = as.Date("2020-01-01") + 0:(n_days-1),
             confirm = round(I))
}

run_estimation <- function(label, case_segment, gen_max = 25){
  d <- delays$cholera
  mean_m <- d$gen[["mean"]];  mean_s <- d$gen[["mean_sd"]]
  sd_m   <- d$gen[["sd"]];    sd_s   <- d$gen[["sd_sd"]]
  shape   <- mean_m^2 / sd_m^2
  rate    <- mean_m / sd_m^2
  shape_s <- shape * sqrt((2*mean_s/mean_m)^2 + (2*sd_s/sd_m)^2)
  rate_s  <- rate  * sqrt((mean_s/mean_m)^2     + (2*sd_s/sd_m)^2)
  gen_time <- Gamma(shape = Normal(shape, shape_s),
                    rate  = Normal(rate,  rate_s),
                    max   = gen_max)
  ## No IP / rep — isolate the GT estimation problem from the convolution
  inc <- Fixed(0)
  rep <- Fixed(0)

  cat("\n=== ", label, " ===\n")
  cat("rows:", nrow(case_segment), " total cases:", sum(case_segment$confirm), "\n")
  t0 <- Sys.time()
  res <- tryCatch(estimate_infections(
    case_segment,
    generation_time = generation_time_opts(gen_time, weight_prior = FALSE),
    delays = delay_opts(inc + rep, weight_prior = FALSE),
    obs = obs_opts(family = "negbin", scale = Fixed(1.0)),
    rt = rt_opts(future = "latest"),
    stan = stan_opts(samples = 1000, return_fit = FALSE,
                     control = list(adapt_delta = 0.99, max_treedepth = 15)),
    forecast = forecast_opts(horizon = 14),
    verbose = FALSE),
    error = function(e) { cat("ERROR:", conditionMessage(e), "\n"); NULL })
  cat("elapsed:", round(as.numeric(Sys.time() - t0, units="secs"), 1), "s\n")
  if (!is.null(res)) {
    summ <- tryCatch(summary(res, type = "parameters"), error = function(e) NULL)
    if (!is.null(summ)) {
      rh <- summ |> dplyr::filter(grepl("delay_params", variable))
      print(rh, n = 10)
    } else {
      cat("(no summary obtained)\n")
    }
  }
}

## A. Daily clean synthetic data with cholera GT, no IP/rep
caseA <- gen_synthetic(n_days = 84, R0 = 1.2, init = 1000)
run_estimation("A: daily synthetic + cholera GT, no IP/rep", caseA)

## B. Weekly-aggregated version of the same data
caseB <- caseA
caseB$week_start <- as.Date(cut(caseB$date, "week"))
caseB <- aggregate(confirm ~ week_start, data = caseB, FUN = sum)
names(caseB)[1] <- "date"
run_estimation("B: weekly-aggregated synthetic + cholera GT, no IP/rep", caseB)
