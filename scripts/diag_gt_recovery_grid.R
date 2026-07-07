## Recovery / learning diagnostic — full grid, SLURM-array driven.
##
## For each cell: simulate case data from the TRUE generation time under an Rt
## trajectory, then estimate the GT (nonparametric Dirichlet or parametric gamma)
## with the prior centred at {0.5, 1, 2}x the true mean, and record whether the
## posterior GT moves toward the truth.
##   misspec = 1   -> recovery check (prior already correct)
##   misspec = 0.5 / 2 -> learning test (does the data pull a wrong prior to truth?)
## Constant Rt is the negative control (identifiability needs a change in growth
## rate); increasing/decreasing cross Rt = 1 and are the informative cases.
##
## One cell per array task. Locally: Rscript scripts/diag_gt_recovery_grid.R <id>

library(here)
suppressMessages(library(EpiNow2))
suppressMessages(library(dplyr))
suppressMessages(library(posterior))

## --- grid -------------------------------------------------------------------
traj_names <- c("constant_low", "constant_high", "increasing", "decreasing")
misspec    <- c(0.5, 1, 2)
reps       <- 1:3
np_conc    <- c(1, 5, 20, 50, 200)   # Dirichlet concentration
pa_width   <- c(0.10, 0.25, 0.50)    # parametric prior SD, relative to the parameter

grid <- rbind(
  expand.grid(method = "nonparametric", trajectory = traj_names, misspec = misspec,
              setting = np_conc, rep = reps, stringsAsFactors = FALSE),
  expand.grid(method = "parametric", trajectory = traj_names, misspec = misspec,
              setting = pa_width, rep = reps, stringsAsFactors = FALSE)
)

args <- commandArgs(trailingOnly = TRUE)
task <- suppressWarnings(as.integer(Sys.getenv("SLURM_ARRAY_TASK_ID",
          if (length(args)) args[[1]] else NA)))
if (is.na(task) || task < 1 || task > nrow(grid)) {
  stop("Provide a task id in 1..", nrow(grid),
       " via SLURM_ARRAY_TASK_ID or a command argument.")
}
row <- grid[task, ]

## --- fixed truth and observation delay --------------------------------------
true_mean_g <- 3.6; true_sd_g <- 3.1
gt_pmf_of <- function(m, s) { p <- discretise(Gamma(mean = m, sd = s, max = 15))$pmf; p[1] <- 0; p / sum(p) }
true_pmf  <- gt_pmf_of(true_mean_g, true_sd_g)
true_mean <- sum((seq_along(true_pmf) - 1) * true_pmf)
inc <- LogNormal(mean = 5.2, sd = 1.52, max = 15)

## --- simulate (seed per replicate) ------------------------------------------
set.seed(row$rep)
n_days <- 120
R <- switch(row$trajectory,
  constant_low  = rep(0.8, n_days), constant_high = rep(1.2, n_days),
  increasing    = seq(0.8, 1.2, length.out = n_days),
  decreasing    = seq(1.2, 0.8, length.out = n_days))
rt <- data.frame(date = as.Date("2020-01-01") + 0:(n_days - 1), R = R)
sim <- simulate_infections(R = rt, initial_infections = 2000,
  generation_time = generation_time_opts(NonParametric(pmf = true_pmf)),
  delays = delay_opts(fix_parameters(inc)), obs = obs_opts(family = "poisson", scale = Fixed(1)))
cases <- sim |> filter(variable == "reported_cases") |> transmute(date, confirm = round(value))

## --- learn arm with a prior centred at (misspec x true mean) -----------------
## Scale mean and SD together (constant CV), so the prior is a well-behaved gamma
## with the same shape at every misspecification rather than a shape < 1 spike.
prior_mean <- row$misspec * true_mean_g
prior_sd   <- row$misspec * true_sd_g
if (row$method == "nonparametric") {
  prior_pmf <- gt_pmf_of(prior_mean, prior_sd)
  gt_opts <- generation_time_opts(NonParametric(pmf = Dirichlet(prior = prior_pmf, concentration = row$setting)))
  prior_eff <- sum((seq_along(prior_pmf) - 1) * prior_pmf)
} else {
  shape_p <- (prior_mean / prior_sd)^2; rate_p <- prior_mean / prior_sd^2
  w <- row$setting
  gt_opts <- generation_time_opts(
    Gamma(shape = Normal(shape_p, w * shape_p), rate = Normal(rate_p, w * rate_p), max = 15),
    weight_prior = FALSE)
  prior_eff <- prior_mean
}

td <- 13
fit <- estimate_infections(data = cases, generation_time = gt_opts,
  delays = delay_opts(fix_parameters(inc)), rt = rt_opts(prior = LogNormal(mean = 1, sd = 0.5)),
  stan = stan_opts(samples = 1000, chains = 2, cores = 2, control = list(max_treedepth = td)))

## --- recovery + sampling diagnostics ----------------------------------------
sf <- fit$fit
dr <- as_draws_df(rstan::extract(sf, pars = "gt_rev_pmf", permuted = FALSE))
rev <- as.matrix(dr[, grep("gt_rev_pmf", names(dr), value = TRUE)])
fwd <- rev[, rev(seq_len(ncol(rev)))]
gm <- as.numeric(fwd %*% (seq_len(ncol(fwd)) - 1))
sp <- rstan::get_sampler_params(sf, inc_warmup = FALSE)

out <- as_tibble(row)
out$true_mean      <- round(true_mean, 2)
out$prior_mean     <- round(prior_eff, 2)
out$post_mean      <- round(mean(gm), 2)
out$post_lo        <- round(quantile(gm, 0.05), 2)
out$post_hi        <- round(quantile(gm, 0.95), 2)
out$divergences    <- sum(vapply(sp, function(x) sum(x[, "divergent__"]), numeric(1)))
out$treedepth_hits <- sum(vapply(sp, function(x) sum(x[, "treedepth__"] >= td), numeric(1)))

dir.create(here("results", "diag_grid"), showWarnings = FALSE, recursive = TRUE)
saveRDS(out, here("results", "diag_grid", sprintf("cell_%03d.rds", task)))
cat(sprintf("cell %03d: %s %s misspec %.1f setting %g -> prior %.2f post %.2f (%.2f-%.2f) td %d\n",
            task, row$method, row$trajectory, row$misspec, row$setting,
            out$prior_mean, out$post_mean, out$post_lo, out$post_hi, out$treedepth_hits))
