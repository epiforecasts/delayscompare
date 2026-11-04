## Pre-specified analysis of the dense-origin and step-up turning-point experiment
## (analysis plan, "Dense-origin and step-up turning-point experiment"). One model
## structure, fitted to each measure and trajectory family:
##
##   y ~ Σ_j [ term_j + Σ_t s(x, by = term_j × 1{class t}) ] + horizon
##       + s(disease, bs = "re") + s(origin, bs = "re")
##
## Step trajectories: x is days from the change to the origin, classes are bend, turn,
## bendup and turnup, and flat origins give the constants. Disease-like replicates:
## x is the observed growth change at the origin. With no flat reference there, the
## smooths (numeric by-variables are not centred) carry the constant themselves. The
## growth-score model stacks the reversal-weighted and the unweighted CRPS, with the
## score type interacting with every term, and on the disease-like replicates splits
## the class by the sign of the trend at the origin, which the reversal weight depends
## on. The hypotheses are contrasts on the fitted curves, with intervals from draws of
## the coefficients.
##
## Env: RESULTS_DIR (scored csv files, default results/), OUT_DIR (default results/),
## SMOOTH_BS ("tp", or "ad" for the adaptive-smooth sensitivity analysis).

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
suppressMessages({library(dplyr); library(tidyr); library(mgcv)})

in_dir  <- Sys.getenv("RESULTS_DIR", here("results"))
out_dir <- Sys.getenv("OUT_DIR", here("results"))
BS      <- Sys.getenv("SMOOTH_BS", "tp")
suffix  <- if (BS == "tp") "" else paste0("_", BS)

CHANGE      <- 105
STEP_WINDOW <- c(-21, 49)
TRAJ        <- c("bend", "turn", "bendup", "turnup")
TERMS       <- c("over_GT", "under_GT", "oneday_GT", "over_delay", "under_delay", "oneday_delay")
K0          <- 10
K_CAP_STEP  <- 16
N_DRAW      <- 2000
RE_TERMS    <- c("s(disease_id)", "s(origin_id)")

## the settings each contrast is evaluated at: term and its value
SETTINGS <- list(
  `doubled delay`   = list(term = "over_delay", value = log(2)),
  `halved delay`    = list(term = "under_delay", value = log(2)),
  `doubled GT`      = list(term = "over_GT", value = log(2)),
  `halved GT`       = list(term = "under_GT", value = log(2)),
  `one-day GT`      = list(term = "oneday_GT", value = 1),
  `no delay`        = list(term = "oneday_delay", value = 1))
HYP_SETTINGS <- c("doubled delay", "one-day GT", "no delay")
## observation-delay levels for H7, shorter then longer than the correct one
BIAS_SETTINGS <- list(
  `delay x0.25` = list(term = "under_delay", value = log(4), sign = 1),
  `delay x0.8`  = list(term = "under_delay", value = log(1.25), sign = 1),
  `no delay`    = list(term = "oneday_delay", value = 1, sign = 1),
  `delay x1.25` = list(term = "over_delay", value = log(1.25), sign = -1),
  `delay x2`    = list(term = "over_delay", value = log(2), sign = -1))

## ---- data ----------------------------------------------------------------------

files <- list.files(in_dir, full.names = TRUE, pattern = paste0(
  "^turningpoint_proper_(hzd_)?(ebola|covid|cholera)_(flat|bend|turn|bendup|turnup|resim)",
  "_rep[0-9]+\\.csv$"))
files <- files[file.size(files) > 0]
need <- c("trend", "crps_g", "twcrps_rev", paste0("bias_log_h", 1:6), "r_crps_log")
d <- lapply(files, function(f) {
  x <- readr::read_csv(f, show_col_types = FALSE)
  if (!all(need %in% names(x))) return(f)
  x |> mutate(run = if (grepl("^turningpoint_proper_hzd_", basename(f))) "hzd" else "hz")
})
old <- unlist(Filter(is.character, d))
if (length(old)) stop("scored by an older scorer, rescore first:\n", paste(basename(old), collapse = "\n"))
d <- bind_rows(d)

d <- d |>
  mutate(start = as.Date(sapply(disease, function(x) as.character(startenddates[[x]][["startdate"]]))),
         days_since = as.numeric(as.Date(origin) - start) - CHANGE,
         origin_key = paste(disease, scen, rep, origin, sep = "_"),
         lmult = ifelse(axis == "correct", 0, log(level)),
         oneday = as.numeric(axis != "correct" & is.na(level)),
         over = ifelse(oneday == 1, 0, pmax(lmult, 0)),
         under = ifelse(oneday == 1, 0, pmax(-lmult, 0)))
for (ax in c("GT", "delay")) for (t in c("over", "under", "oneday"))
  d[[paste0(t, "_", ax)]] <- ifelse(d$axis == ax, d[[t]], 0)

## observed growth change at the origin: |mean weekly growth over the last two weeks
## minus that over the two weeks before|, on log(weekly cases + 1), data up to the origin
obs_change <- function(disease, rep, origin) {
  x <- readRDS(here("data", paste0(disease, "_sim_data_rep", rep, ".rds")))
  x <- x[x$variable == "reported_cases", ]
  day <- as.numeric(as.Date(x$date) - as.Date(origin))
  lW <- sapply(c(0, -2, -4), function(k) {
    v <- x$value[day %in% (7 * (k - 1) + 1:7)]
    if (length(v) == 7) log1p(sum(v)) else NA_real_
  })
  abs((lW[1] - lW[2]) / 2 - (lW[2] - lW[3]) / 2)
}

step <- d |> filter(scen != "resim", days_since >= STEP_WINDOW[1], days_since <= STEP_WINDOW[2]) |>
  mutate(x = days_since, class = scen)
dl <- d |> filter(scen == "resim", rep <= 10)
dl_orig <- dl |> distinct(disease, rep, origin)
dl_orig$x <- mapply(obs_change, dl_orig$disease, dl_orig$rep, as.character(dl_orig$origin))
dl <- dl |> left_join(dl_orig, by = c("disease", "rep", "origin")) |> filter(!is.na(x))

## ---- model ---------------------------------------------------------------------

## c_<term>_<type>: the term's constant (its effect on a flat trajectory);
## s_<term>_<type>: its shift at origins with a declining trend, for the growth scores,
## whose reversal weight depends on that sign even where a trajectory's trend does not
## settle on one (bend after the change, bendup before it);
## b_<term>_<class>_<type>: its by-variable for the smooth in x on that class
add_design <- function(dd, classes, constants) {
  by_sign <- constants && length(unique(dd$type)) > 1
  for (ty in unique(dd$type)) for (tm in TERMS) {
    v <- dd[[tm]] * (dd$type == ty)
    if (constants) dd[[paste0("c_", tm, "_", ty)]] <- v
    if (by_sign) dd[[paste0("s_", tm, "_", ty)]] <- v * (dd$trend < 0)
    for (cl in classes) dd[[paste0("b_", tm, "_", cl, "_", ty)]] <- v * (dd$class == cl)
  }
  dd
}

build_formula <- function(dd, k, constants, horizon) {
  cs <- grep("^[cs]_", names(dd), value = TRUE)
  cs <- Filter(function(v) any(dd[[v]] != 0), cs)
  bys <- names(k)
  rhs <- c(if (length(unique(dd$type)) > 1) "type", if (horizon) "horizon",
           if (constants) cs,
           sprintf("s(x, by = %s, k = %d, bs = '%s')", bys, k, BS),
           "s(disease_id, bs = 're')", "s(origin_id, bs = 're')")
  as.formula(paste("y ~", paste(rhs, collapse = " + ")))
}

## Fit, then refit once with k doubled (up to the cap) for every smooth whose k-index
## suggests the basis dimension binds, as in the plan.
fit_model <- function(dd, fam, classes, constants, horizon, k_cap, name) {
  dd <- add_design(dd, classes, constants) |>
    mutate(disease_id = factor(paste(disease, type)), origin_id = factor(paste(origin_key, type)),
           type = factor(type))
  if (horizon) dd$horizon <- factor(dd$horizon)
  bys <- grep("^b_", names(dd), value = TRUE)
  bys <- Filter(function(v) any(dd[[v]] != 0), bys)
  k <- setNames(rep(K0, length(bys)), bys)
  fit <- function(k) bam(build_formula(dd, k, constants, horizon), data = dd, family = fam,
                         method = "fREML", discrete = TRUE)
  m <- fit(k)
  kc <- k.check(m)
  by_of <- sub("^s\\(x\\):", "", rownames(kc))
  binds <- kc[, "edf"] >= kc[, "k'"] - 2 & kc[, "k-index"] < 1 & kc[, "p-value"] < 0.05
  binds[is.na(binds)] <- FALSE
  if (any(binds)) {
    k[by_of[binds]] <- pmin(2 * k[by_of[binds]], k_cap)
    m <- fit(k)
    kc <- k.check(m)
  }
  kc <- data.frame(model = name, smooth = rownames(kc), k_used = unname(k[sub("^s\\(x\\):", "", rownames(kc))]),
                   as.data.frame(kc, check.names = FALSE), refitted = any(binds), row.names = NULL,
                   check.names = FALSE)
  list(m = m, data = dd, kcheck = kc, constants = constants, horizon = horizon, name = name)
}

## ---- contrasts -----------------------------------------------------------------

## Change in the linear predictor from setting one term to a value, at the rows of
## `at` (columns x, class, type), excluding the random effects: rows x draws matrix,
## and the estimate at the fitted coefficients.
delta_eta <- function(fm, at, term, value) {
  dd <- fm$data
  nd <- dd[rep(1, nrow(at)), , drop = FALSE]
  nd$x <- at$x; nd$class <- at$class
  nd$type <- factor(at$type, levels = levels(dd$type))
  dcols <- grep("^[cbs]_", names(dd), value = TRUE)
  nd[dcols] <- 0
  set <- nd
  declining <- if (is.null(at$trend)) rep(FALSE, nrow(at)) else at$trend < 0
  for (i in seq_len(nrow(at))) {
    cc <- paste0("c_", term, "_", at$type[i])
    ss <- paste0("s_", term, "_", at$type[i])
    bb <- paste0("b_", term, "_", at$class[i], "_", at$type[i])
    if (cc %in% dcols) set[[cc]][i] <- value
    if (ss %in% dcols && declining[i]) set[[ss]][i] <- value
    if (bb %in% dcols) set[[bb]][i] <- value
  }
  X <- predict(fm$m, set, type = "lpmatrix", exclude = RE_TERMS) -
    predict(fm$m, nd, type = "lpmatrix", exclude = RE_TERMS)
  list(est = as.numeric(X %*% coef(fm$m)), draws = X %*% t(fm$B))
}

summ <- function(est, draws) c(est = est, lower = unname(quantile(draws, 0.025)),
                               upper = unname(quantile(draws, 0.975)))

## mean of the effect over the rows of `at`, optionally averaged with equal weight
## over classes first
mean_effect <- function(fm, at, term, value, by_class = FALSE, f = identity) {
  e <- delta_eta(fm, at, term, value)
  w <- if (by_class) {
    n <- as.numeric(table(at$class)[at$class])
    1 / (n * length(unique(at$class)))
  } else rep(1 / nrow(at), nrow(at))
  list(est = sum(w * f(e$est)), draws = colSums(w * f(e$draws)))
}

## distinct origins of a family within a window of x, with their class
origins_in <- function(fm, lo = -Inf, hi = Inf, classes = NULL, type = levels(fm$data$type)[1]) {
  o <- fm$data |> filter(x >= lo, x <= hi) |> distinct(origin_key, x, class, trend) |>
    mutate(type = type)
  if (!is.null(classes)) o <- o |> filter(class %in% classes)
  o
}

## curves over the observed range of x per class, for reporting
curves <- function(fm, classes, settings, family) {
  bind_rows(lapply(names(settings), function(s) bind_rows(lapply(classes, function(cl) {
    rng <- range(fm$data$x[fm$data$class == cl])
    at <- data.frame(x = seq(rng[1], rng[2], length.out = 50), class = cl,
                     type = levels(fm$data$type)[1])
    e <- delta_eta(fm, at, settings[[s]]$term, settings[[s]]$value)
    data.frame(model = fm$name, family = family, setting = s, class = cl, x = at$x, est = e$est,
               lower = apply(e$draws, 1, quantile, 0.025), upper = apply(e$draws, 1, quantile, 0.975))
  }))))
}

with_draws <- function(fm) {
  set.seed(1)
  fm$B <- MASS::mvrnorm(N_DRAW, coef(fm$m), vcov(fm$m))
  fm
}

## ---- fits ----------------------------------------------------------------------

long_h <- function(dd, prefix) dd |>
  select(disease, origin_key, x, class, trend, all_of(TERMS), starts_with(prefix)) |>
  pivot_longer(starts_with(prefix), names_to = "horizon", values_to = "y", names_prefix = prefix) |>
  filter(!is.na(y)) |> mutate(type = "y")
long_g <- function(dd) dd |>
  select(disease, origin_key, x, class, trend, all_of(TERMS), crps_g, twcrps_rev) |>
  pivot_longer(c(crps_g, twcrps_rev), names_to = "type", values_to = "y") |>
  filter(!is.na(y)) |> mutate(type = ifelse(type == "twcrps_rev", "rev", "g"))

## H7 excludes the replicate and origin that suggested its direction
step_bias <- step |> filter(!(disease == "covid" & scen == "turn" & rep == 1 & days_since == 7))
dl_g <- dl |> mutate(class = ifelse(trend >= 0, "grow", "decl"))
dl <- dl |> mutate(class = "all")

fits <- list(
  crps_step = fit_model(long_h(step, "crps_log_h"), gaussian(), TRAJ, TRUE, TRUE, K_CAP_STEP, "crps_step"),
  crps_dl   = fit_model(long_h(dl, "crps_log_h"), gaussian(), "all", FALSE, TRUE, 2 * K0, "crps_dl"),
  growth_step = fit_model(long_g(step), tw(), TRAJ, TRUE, FALSE, K_CAP_STEP, "growth_step"),
  growth_dl   = fit_model(long_g(dl_g), tw(), c("grow", "decl"), FALSE, FALSE, 2 * K0, "growth_dl"),
  bias_step = fit_model(long_h(step_bias, "bias_log_h"), gaussian(), TRAJ, TRUE, TRUE, K_CAP_STEP, "bias_step"),
  r_step = fit_model(step |> transmute(disease, origin_key, x, class, trend, across(all_of(TERMS)),
                                       y = r_crps_log, type = "y") |> filter(!is.na(y)),
                     gaussian(), TRAJ, TRUE, FALSE, K_CAP_STEP, "r_step"),
  r_dl = fit_model(dl |> transmute(disease, origin_key, x, class, trend, across(all_of(TERMS)),
                                   y = r_crps_log, type = "y") |> filter(!is.na(y)),
                   gaussian(), "all", FALSE, FALSE, 2 * K0, "r_dl"))
fits <- lapply(fits, with_draws)
saveRDS(lapply(fits, `[[`, "m"), file.path(out_dir, paste0("gamm_turningpoint_dense", suffix, ".rds")))

## ---- hypotheses ----------------------------------------------------------------

hyp <- list()
add_hyp <- function(id, family, setting, quantity, s, test, pass) {
  hyp[[length(hyp) + 1]] <<- data.frame(hypothesis = id, family = family, setting = setting,
                                        quantity = quantity, est = s[["est"]], lower = s[["lower"]],
                                        upper = s[["upper"]], test = test, supported = pass)
}

## H5 and H6: ratio of expected score, wrong to correct delay, averaged over origins
## with a growing (H5) or declining (H6) trend; second part against the unweighted score
for (fam in c("step", "dl")) {
  fm <- fits[[paste0("growth_", fam)]]
  family <- if (fam == "step") "step" else "disease-like"
  for (h in c("H5", "H6")) {
    for (s in HYP_SETTINGS) {
      st <- SETTINGS[[s]]
      o <- origins_in(fm)
      o <- if (h == "H5") o[o$trend >= 0, ] else o[o$trend < 0, ]
      if (!nrow(o)) next
      r_rev <- mean_effect(fm, mutate(o, type = "rev"), st$term, st$value, f = exp)
      r_g   <- mean_effect(fm, mutate(o, type = "g"), st$term, st$value, f = exp)
      a <- summ(r_rev$est, r_rev$draws)
      add_hyp(h, family, s, "ratio, reversal-weighted CRPS", a, "lower > 1", a[["lower"]] > 1)
      add_hyp(h, family, s, "ratio, unweighted growth CRPS", summ(r_g$est, r_g$draws), "reported", NA)
      b <- summ(r_rev$est / r_g$est, r_rev$draws / r_g$draws)
      add_hyp(h, family, s, "ratio of ratios, weighted / unweighted", b, "lower > 1", b[["lower"]] > 1)
    }
  }
}

## H1: disease-like, effect at the 90th percentile of the observed change minus at none
fm <- fits$crps_dl
q90 <- unname(quantile(fm$data |> distinct(origin_key, x) |> pull(x), 0.9))
for (s in HYP_SETTINGS) {
  e <- delta_eta(fm, data.frame(x = c(0, q90), class = "all", type = "y"), SETTINGS[[s]]$term, SETTINGS[[s]]$value)
  a <- summ(e$est[2] - e$est[1], e$draws[2, ] - e$draws[1, ])
  add_hyp("H1", "disease-like", s, sprintf("effect at x = %.3f (q90) minus at 0", q90), a, "lower > 0", a[["lower"]] > 0)
}

## H2 and H3: step, averaged with equal weight over the four trajectories
fm <- fits$crps_step
win <- function(lo, hi, s) mean_effect(fm, origins_in(fm, lo, hi, TRAJ), SETTINGS[[s]]$term,
                                       SETTINGS[[s]]$value, by_class = TRUE)
for (s in HYP_SETTINGS) {
  before <- win(-14, -2, s); early <- win(1, 13, s); late <- win(16, 34, s)
  a <- summ(before$est, before$draws)
  add_hyp("H2", "step", s, "effect, 14 to 2 days before", a, "interval within ±0.05",
          a[["lower"]] > -0.05 && a[["upper"]] < 0.05)
  b <- summ(early$est - before$est, early$draws - before$draws)
  add_hyp("H3", "step", s, "1 to 13 days after minus 14 to 2 before", b, "lower > 0", b[["lower"]] > 0)
  c <- summ(early$est - late$est, early$draws - late$draws)
  add_hyp("H3", "step", s, "1 to 13 days after minus 16 to 34 after", c, "lower > 0", c[["lower"]] > 0)
}

## H7: signed error over 1 to 13 days after the change, turn and turnup separately;
## shorter delays over-forecast after a peak and longer ones under-forecast, reversed
## after a trough
fm <- fits$bias_step
for (traj in c("turn", "turnup")) for (s in names(BIAS_SETTINGS)) {
  st <- BIAS_SETTINGS[[s]]
  sgn <- st$sign * (if (traj == "turn") 1 else -1)
  e <- mean_effect(fm, origins_in(fm, 1, 13, traj), st$term, st$value)
  a <- summ(e$est, e$draws)
  add_hyp("H7", paste("step", traj), s, "signed error, 1 to 13 days after",
          a, if (sgn > 0) "lower > 0" else "upper < 0",
          if (sgn > 0) a[["lower"]] > 0 else a[["upper"]] < 0)
}

hyp <- bind_rows(hyp)
readr::write_csv(hyp, file.path(out_dir, paste0("gamm_turningpoint_dense_hypotheses", suffix, ".csv")))
readr::write_csv(bind_rows(lapply(fits, `[[`, "kcheck")),
                 file.path(out_dir, paste0("gamm_turningpoint_dense_kcheck", suffix, ".csv")))

## ---- curves and figure ---------------------------------------------------------

cv <- bind_rows(
  curves(fits$crps_step, TRAJ, SETTINGS, "step"),
  curves(fits$crps_dl, "all", SETTINGS, "disease-like"),
  curves(fits$bias_step, TRAJ, BIAS_SETTINGS, "step"),
  curves(fits$r_step, TRAJ, SETTINGS, "step"),
  curves(fits$r_dl, "all", SETTINGS, "disease-like"))
readr::write_csv(cv, file.path(out_dir, paste0("gamm_turningpoint_dense_curves", suffix, ".csv")))

cat("\n=== hypotheses ===\n")
print(hyp |> mutate(across(c(est, lower, upper), ~ round(.x, 3))), row.names = FALSE)
cat("\n=== smooths where the basis dimension may bind (after any refit) ===\n")
kc <- bind_rows(lapply(fits, `[[`, "kcheck"))
print(kc |> filter(`k-index` < 1, `p-value` < 0.05, edf >= `k'` - 2), row.names = FALSE)

p <- ggplot2::ggplot(cv |> filter(model == "crps_step", setting %in% HYP_SETTINGS),
                     ggplot2::aes(x, est, colour = class, fill = class)) +
  ggplot2::geom_hline(yintercept = 0, colour = "grey70") +
  ggplot2::geom_vline(xintercept = 0, colour = "grey50") +
  ggplot2::geom_ribbon(ggplot2::aes(ymin = lower, ymax = upper), alpha = 0.15, colour = NA) +
  ggplot2::geom_line() +
  ggplot2::facet_wrap(~ setting, scales = "free_y") +
  ggplot2::labs(x = "days from the change in R to the forecast origin",
                y = "change in log-scale CRPS against the correct delay", colour = NULL, fill = NULL) +
  ggplot2::theme_bw()
ggplot2::ggsave(file.path(out_dir, paste0("fig_turningpoint_dense_crps", suffix, ".png")), p,
                width = 10, height = 4, dpi = 150)
