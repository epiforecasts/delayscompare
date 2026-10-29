## Turning-point experiment, scored without conditioning on the outcome.
##
## Stratifying ordinary CRPS by how much R changed over the forecast window conditions
## the evaluation on (a smoothed function of) the target, which breaks propriety (the
## forecaster's dilemma). Instead, following the analysis plan, everything is computed
## from the log-scale outcomes already scored, log(daily cases + 1) at one to six weeks
## after the origin, or from weekly growth rates of the same forecasts:
##   (1) sensitivity: energy distance between the six-week log-scale path under each
##       misspecified delay and under the correct delay at the same origin. No outcome
##       enters, so it can be summarised by any label, including turning points.
##   (2) skill: log-scale CRPS at each weekly horizon (primary), and threshold- and
##       outcome-weighted scores on weekly growth rates that emphasise growth departing
##       from the growth known at the origin. The emphasis sits inside each score,
##       applied identically to observed and forecast paths, so the scores stay proper
##       and are averaged over all origins.
##
## Also records the error in R at the origin under each delay against the true R, the
## mechanism by which a wrong delay would cost skill once a change reaches the data.
##
## Usage: Rscript scripts/score_turningpoint_proper.R <disease> <scen> <rep> [label]
## <scen> is flat, bend, turn, bendup or turnup (step trajectories) or resim
## (disease-like replicates). <label> is the run label of the fits (default hz; hzd for
## the dense-origin runs).
## Writes results/turningpoint_proper_<disease>_<scen>_rep<rep>.csv, with the label
## after "proper" when it is not hz.

library(here)
source(here("scripts", "01_packages.R"))
source(here("scripts", "02b_definedelays.R"))
library(dplyr)
library(scoringRules)

args <- commandArgs(trailingOnly = TRUE)
disease <- args[1]; scen <- args[2]; rep_arg <- as.integer(args[3])
label <- if (length(args) >= 4) args[4] else "hz"
stopifnot(disease %in% c("ebola", "covid", "cholera"),
          scen %in% c("flat", "bend", "turn", "bendup", "turnup", "resim"), !is.na(rep_arg))

rt_opts <- "latest"
raw_dir <- here("results", "raw")
mult    <- c(`1` = NA, `2` = 0.25, `3` = 0.8, `4` = 1, `5` = 1.25, `6` = 2)
CHANGE  <- 105
WEEKS   <- 1:6
N_ED    <- 300   # forecast paths per condition for the energy distance (O(n^2))
N_SC    <- 1000  # forecast samples per condition for the scores
N_VS    <- 300   # for the variogram score, whose cost grows with the square of this

## Change in true R over the 42 days after and before the origin. Used only to label
## origins for the sensitivity summary, never to select which scores are compared.
if (scen == "resim") {
  rt <- readRDS(here("data", paste0("rt_", disease, ".rds")))
  rt <- data.frame(date = as.Date(rt$date), R = if ("R" %in% names(rt)) rt$R else rt$median)
  R_range <- function(a, b) {
    w <- rt$R[rt$date >= a & rt$date <= b]
    if (length(w) < 2) NA_real_ else diff(range(w))
  }
  dR_of <- function(origin) c(fc = R_range(origin, origin + 42), est = R_range(origin - 42, origin))
  true_R_on <- function(date) rt$R[match(as.Date(date), rt$date)]
} else {
  ab_of <- list(flat = c(1.10, 1.10), bend = c(1.30, 1.00), turn = c(1.30, 0.70),
                bendup = c(1.00, 1.30), turnup = c(0.70, 1.30))
  true_R_at <- function(day) ifelse(day < CHANGE, ab_of[[scen]][1], ab_of[[scen]][2])
  dR_of <- function(origin) {
    day0 <- as.numeric(origin - startenddates[[disease]][["startdate"]])
    c(fc = abs(true_R_at(day0 + 42) - true_R_at(day0)),
      est = abs(true_R_at(day0) - true_R_at(day0 - 84)))
  }
  true_R_on <- function(date) true_R_at(as.numeric(as.Date(date) - startenddates[[disease]][["startdate"]]))
}
scen_infix <- if (scen == "resim") "" else paste0("_", scen)

THRESH <- c(0.05, 0.1, 0.2)

## A turn is weekly growth departing from g0, the growth known at the origin. These
## chaining and weight functions are fixed in the analysis plan and applied identically
## to the observed and forecast growth paths.
chain_g    <- function(g, g0, thr) ifelse(abs(g - g0) > thr, g, g0)
chain_path <- function(g0, thr) function(g) if (max(abs(g - g0)) > thr) g else rep(g0, length(g))
w_path     <- function(g0, thr) function(g) plogis((max(abs(g - g0)) - thr) / (0.3 * thr))

energy_distance <- function(X, Y) {
  nx <- nrow(X); ny <- nrow(Y)
  dxy <- as.matrix(dist(rbind(X, Y)))
  2 * mean(dxy[seq_len(nx), nx + seq_len(ny)]) -
    mean(dxy[seq_len(nx), seq_len(nx)]) - mean(dxy[nx + seq_len(ny), nx + seq_len(ny)])
}

converged <- function(stub, suf) {
  df <- file.path(raw_dir, paste0(stub, "_diagnostics", suf))
  if (!file.exists(df)) return(TRUE)
  d <- readRDS(df)
  !is.na(d$max_rhat) && d$max_rhat <= 1.05 && (is.na(d$num_divergent) || d$num_divergent <= 100)
}

## week k after the origin covers days 7(k-1)+1 .. 7k; week 0 is the last observed week
week_of <- function(date, origin) ceiling(as.numeric(as.Date(date) - origin) / 7)
H_DAYS  <- 7 * WEEKS

## Forecast samples as two (samples x 6) matrices: L, log(daily cases + 1) at one to six
## weeks after the origin (the scored outcomes), and G, weekly growth rates
## log(W_k + 1) - log(W_(k-1) + 1) with W_0 the observed last week.
forecast_mats <- function(stub, suf, origin, lW0) {
  x <- readRDS(file.path(raw_dir, paste0(stub, "_samples", suf)))
  x <- x[x$type == "forecast", ]
  x$day <- as.numeric(as.Date(x$date) - origin)
  x <- x[x$day %in% seq_len(max(H_DAYS)), ]
  if (!nrow(x)) return(NULL)
  m <- tidyr::pivot_wider(x[, c("sample", "day", "prediction")],
                          names_from = day, values_from = prediction)
  if (!all(as.character(seq_len(max(H_DAYS))) %in% names(m))) return(NULL)
  m <- as.matrix(m[, as.character(seq_len(max(H_DAYS)))])
  m <- m[stats::complete.cases(m), , drop = FALSE]
  W <- sapply(WEEKS, function(k) rowSums(m[, 7 * (k - 1) + 1:7, drop = FALSE]))
  lW <- log1p(W)
  list(L = log1p(m[, H_DAYS, drop = FALSE]), G = lW - cbind(lW0, lW[, -length(WEEKS)]))
}

## log R at the origin, from the estimate on partial data, against the true log R
r_at_origin <- function(stub, suf, origin) {
  f <- file.path(raw_dir, paste0(stub, "_R", suf))
  if (!file.exists(f)) return(c(r_crps_log = NA_real_, r_err_log = NA_real_))
  x <- readRDS(f)
  v <- log(x$value[as.Date(x$date) == origin & x$type != "forecast"])
  y <- log(true_R_on(origin))
  if (!length(v) || is.na(y)) return(c(r_crps_log = NA_real_, r_err_log = NA_real_))
  c(r_crps_log = crps_sample(y, v), r_err_log = stats::median(v) - y)
}

files <- list.files(raw_dir, pattern = paste0("^", disease, "_", label, scen_infix, "_rep[0-9]+_",
  rt_opts, "_samples_gt[0-9]+_inc[0-9]+_tp[0-9]+\\.rds$"))
k <- stringr::str_match(files, "_rep([0-9]+)_.*_samples_gt([0-9]+)_inc([0-9]+)_tp([0-9]+)\\.rds$")
runs <- data.frame(rep = as.integer(k[, 2]), gt = as.integer(k[, 3]),
                   inc = as.integer(k[, 4]), tp = as.integer(k[, 5]))
## the two misspecification axes; gt4/inc4 is the correct specification on both
runs <- runs[(runs$inc == 4 | runs$gt == 4) & runs$rep == rep_arg, ]

set.seed(1)
rows <- list()
for (g in split(runs, list(runs$rep, runs$tp), drop = TRUE)) {
  rep_id <- g$rep[1]; tp <- g$tp[1]
  stub <- paste0(disease, "_", label, scen_infix, "_rep", rep_id, "_", rt_opts)
  suf_of <- function(gt, inc) paste0("_gt", gt, "_inc", inc, "_tp", tp, ".rds")
  if (!any(g$gt == 4 & g$inc == 4) || !converged(stub, suf_of(4, 4))) next

  origin <- as.Date(readRDS(file.path(raw_dir, paste0(stub, "_id", suf_of(4, 4))))$forecast_origin[1])
  dR <- dR_of(origin)
  obs <- readRDS(here("data", paste0(disease, "_sim_data", scen_infix, "_rep", rep_id, ".rds")))
  obs <- obs[obs$variable == "reported_cases", ]
  obs$day <- as.numeric(as.Date(obs$date) - origin)
  obs_day <- function(days) obs$value[match(days, obs$day)]
  wk <- sapply(-1:6, function(k) sum(obs_day(7 * (k - 1) + 1:7)))
  if (anyNA(wk) || anyNA(obs_day(H_DAYS))) next
  lW_obs <- log1p(wk)                      # weeks -1, 0, 1..6
  g0 <- lW_obs[2] - lW_obs[1]              # growth known at the origin
  y_L <- log1p(obs_day(H_DAYS))
  y_G <- diff(lW_obs)[-1]                  # growth in weeks 1..6

  ref <- forecast_mats(stub, suf_of(4, 4), origin, lW_obs[2])
  if (is.null(ref) || nrow(ref$L) < N_ED) next
  ref_ed <- ref$L[sample(nrow(ref$L), N_ED), ]

  for (i in seq_len(nrow(g))) {
    gt <- g$gt[i]; inc <- g$inc[i]
    if (!converged(stub, suf_of(gt, inc))) next
    fm <- if (gt == 4 && inc == 4) ref else forecast_mats(stub, suf_of(gt, inc), origin, lW_obs[2])
    if (is.null(fm) || nrow(fm$L) < N_ED) next
    idx <- sample(nrow(fm$L), min(N_SC, nrow(fm$L)))
    L <- fm$L[idx, , drop = FALSE]; G <- fm$G[idx, , drop = FALSE]
    axis <- if (gt == 4 && inc == 4) "correct" else if (inc == 4) "GT" else "delay"

    crps_log <- sapply(WEEKS, function(h) crps_sample(y_L[h], L[, h]))
    weighted <- unlist(lapply(THRESH, function(thr) {
      tag <- sub("\\.", "", format(thr, nsmall = 2))
      stats::setNames(c(
        mean(sapply(WEEKS, function(h)
          crps_sample(chain_g(y_G[h], g0, thr), chain_g(G[, h], g0, thr)))),
        twes_sample(y_G, t(G), chain_func = chain_path(g0, thr)),
        owes_sample(y_G, t(G), weight_func = w_path(g0, thr)),
        owvs_sample(y_G, t(G[seq_len(min(N_VS, nrow(G))), , drop = FALSE]),
                    weight_func = w_path(g0, thr))),
        paste0(c("twcrps_g_", "twes_g_", "owes_g_", "owvs_g_"), tag))
    }))
    rows[[length(rows) + 1]] <- data.frame(
      disease = disease, scen = scen, rep = rep_id, tp = tp, origin = origin,
      dR_fc = dR[["fc"]], dR_est = dR[["est"]],
      gt = gt, inc = inc, axis = axis,
      level = if (axis == "GT") mult[as.character(gt)] else if (axis == "delay") mult[as.character(inc)] else 1,
      ed = if (axis == "correct") 0 else energy_distance(fm$L[sample(nrow(fm$L), N_ED), ], ref_ed),
      med_shift6 = stats::median(fm$L[, 6]) - stats::median(ref$L[, 6]),
      g0 = g0, max_dev_obs = max(abs(y_G - g0)),
      as.list(stats::setNames(crps_log, paste0("crps_log_h", WEEKS))),
      crps_log_mean = mean(crps_log),
      es_g = es_sample(y_G, t(G)),
      as.list(weighted),
      as.list(r_at_origin(stub, suf_of(gt, inc), origin)))
  }
}
out <- bind_rows(rows)
label_infix <- if (label == "hz") "" else paste0(label, "_")
readr::write_csv(out, here("results", paste0("turningpoint_proper_", label_infix, disease, "_", scen,
                                              "_rep", rep_arg, ".csv")))
message(disease, " ", scen, " rep ", rep_arg, ": ", nrow(out), " rows from ", length(unique(paste(out$rep, out$tp))), " origins")
