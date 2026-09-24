## Does estimating the generation time (weight_prior=FALSE, wide parametric prior)
## beat imposing the literature value, at dense weekly case-study origins? Two
## questions per origin: (1) does the CRPS improve relative to the matched
## literature-imposed condition; (2) does the GT posterior actually move away from
## the literature prior, and in which direction. Pairs the dense weekly GT-learn
## arm (06f + weekly SLURM array) against the matched literature-impose arm
## (06d, gt=4/inc=4, same weekly origins).

library(here)
suppressMessages({library(dplyr); library(ggplot2)})

diseases <- c("covid", "ebola", "cholera")
rt_opts  <- "latest"
lit_gt_mean <- c(covid = 3.6, ebola = 16.2, cholera = 5.0)   # Table 1 literature values

score_disease <- function(disease) {
  ## --- literature-imposed arm (06d, results/raw, weekly origins, gt=4 inc=4) ---
  lit_files <- list.files(here("results", "raw"),
    pattern = paste0("^", disease, "_casestudy_", rt_opts, "_diagnostics_gt4_inc4_tp[0-9]+\\.rds$"))
  lit_tps <- as.integer(sub(".*_tp([0-9]+)\\.rds$", "\\1", lit_files))

  ## --- GT-learn arm (06f, results/, weekly origins) ---
  learn_files <- list.files(here("results"),
    pattern = paste0("^res_", disease, "_widepriors_FALSE_gt_", rt_opts, "_diagnostics_tp[0-9]+-[0-9]+\\.rds$"))
  learn_tps <- as.integer(sub(".*_tp([0-9]+)-.*$", "\\1", learn_files))

  common_tp <- sort(intersect(lit_tps, learn_tps))
  message("  ", disease, ": ", length(common_tp), " matched origins (",
          "literature ", length(lit_tps), ", learn ", length(learn_tps), ")")
  if (length(common_tp) == 0) return(NULL)

  ## true cases for scoring
  source(here("scripts", "01_packages.R"))
  source(here("scripts", "02b_definedelays.R"))
  source(here("scripts", "datacollect_casestudy.R"))
  obs <- casestudydata[[disease]] |> transmute(date = as.Date(date), true_value = confirm)

  rows <- list()
  for (tp in common_tp) {
    ## convergence filter, both arms
    dl <- readRDS(here("results", "raw", paste0(disease, "_casestudy_", rt_opts,
      "_diagnostics_gt4_inc4_tp", tp, ".rds")))
    dg <- readRDS(here("results", paste0("res_", disease, "_widepriors_FALSE_gt_", rt_opts,
      "_diagnostics_tp", tp, "-", tp, ".rds")))
    ok <- function(d) !is.na(d$max_rhat) && d$max_rhat <= 1.05 &&
      (is.na(d$num_divergent) || d$num_divergent <= 100)
    if (!ok(dl) || !ok(dg)) { message("    tp", tp, ": excluded (non-convergent)"); next }

    ## forecast CRPS, both arms, 1-2 wk horizon (matches the case-study standard)
    id_lit <- readRDS(here("results", "raw", paste0(disease, "_casestudy_", rt_opts,
      "_id_gt4_inc4_tp", tp, ".rds")))
    s_lit  <- readRDS(here("results", "raw", paste0(disease, "_casestudy_", rt_opts,
      "_samples_gt4_inc4_tp", tp, ".rds")))
    id_learn <- readRDS(here("results", paste0("res_", disease, "_widepriors_FALSE_gt_", rt_opts,
      "_id_tp", tp, "-", tp, ".rds")))
    s_learn  <- readRDS(here("results", paste0("res_", disease, "_widepriors_FALSE_gt_", rt_opts,
      "_samples_tp", tp, "-", tp, ".rds")))
    origin <- as.Date(id_lit$forecast_origin[1])

    score_one <- function(s, origin) {
      sc <- s |> filter(type == "forecast") |> left_join(obs, by = "date") |>
        filter(!is.na(true_value)) |>
        mutate(h = as.numeric(date - origin) / 7) |> filter(h > 0, h <= 2)
      if (nrow(sc) == 0) return(NA_real_)
      f <- tryCatch(scoringutils::as_forecast_sample(sc,
        forecast_unit = "date", observed = "true_value",
        predicted = "prediction", sample_id = "sample") |>
        scoringutils::transform_forecasts(fun = scoringutils::log_shift, offset = 1, label = "log") |>
        scoringutils::score(), error = function(e) NULL)
      if (is.null(f)) NA_real_ else mean(f$crps[f$scale == "log"])
    }
    crps_lit   <- score_one(s_lit,   origin)
    crps_learn <- score_one(s_learn, origin)

    ## GT posterior vs literature prior
    gt_file <- here("results", paste0("res_", disease, "_widepriors_FALSE_gt_", rt_opts,
      "_gt_tp", tp, "-", tp, ".rds"))
    gt_post <- if (file.exists(gt_file)) readRDS(gt_file)$gt_mean else NA_real_

    rows[[length(rows) + 1]] <- data.frame(disease = disease, tp = tp, origin = origin,
      crps_literature = crps_lit, crps_learn = crps_learn,
      gt_posterior_mean = mean(gt_post, na.rm = TRUE),
      gt_posterior_median = median(gt_post, na.rm = TRUE),
      gt_literature = lit_gt_mean[disease])
  }
  bind_rows(rows)
}

dat <- bind_rows(lapply(diseases, score_disease))
readr::write_csv(dat, here("results", "casestudy_learn_vs_impose_weekly.csv"))

cat("\n=== CRPS: learn vs literature, by disease (matched, converged origins) ===\n")
print(dat |> group_by(disease) |>
  summarise(n = n(), mean_crps_literature = mean(crps_literature, na.rm = TRUE),
           mean_crps_learn = mean(crps_learn, na.rm = TRUE),
           learn_wins = sum(crps_learn < crps_literature, na.rm = TRUE)), n = 10)

cat("\n=== GT posterior mean vs literature value, by disease ===\n")
print(dat |> group_by(disease) |>
  summarise(gt_literature = first(gt_literature),
           mean_posterior = mean(gt_posterior_mean, na.rm = TRUE),
           sd_posterior = sd(gt_posterior_mean, na.rm = TRUE)))

cat("\n=== per-origin detail ===\n")
print(as.data.frame(dat |> arrange(disease, tp) |>
  transmute(disease, tp, origin, crps_literature = round(crps_literature, 3),
           crps_learn = round(crps_learn, 3), gt_post = round(gt_posterior_mean, 2),
           gt_lit = gt_literature)), row.names = FALSE)

## Figure: CRPS (learn vs literature) and GT posterior (vs literature value) over time
p1 <- ggplot(dat, aes(origin)) +
  geom_line(aes(y = crps_literature, colour = "literature")) +
  geom_point(aes(y = crps_literature, colour = "literature")) +
  geom_line(aes(y = crps_learn, colour = "estimated")) +
  geom_point(aes(y = crps_learn, colour = "estimated")) +
  facet_wrap(~ disease, scales = "free", ncol = 1) +
  scale_colour_manual(values = c(literature = "#1b9e77", estimated = "#d95f02")) +
  labs(y = "forecast log-CRPS (1-2wk)", x = NULL, colour = "GT condition",
       title = "Estimating vs imposing the generation time, weekly case-study origins") +
  theme_bw()
ggsave(here("results", "fig_casestudy_learn_vs_impose_crps.png"), p1, width = 8, height = 8, dpi = 150)

p2 <- ggplot(dat, aes(origin, gt_posterior_mean)) +
  geom_hline(aes(yintercept = gt_literature), linetype = "dashed", colour = "grey50") +
  geom_line(colour = "#7570b3") + geom_point(colour = "#7570b3") +
  facet_wrap(~ disease, scales = "free", ncol = 1) +
  labs(y = "posterior mean generation time (days)\n(dashed = literature value)", x = NULL,
       title = "Does the GT posterior move away from the literature prior?") +
  theme_bw()
ggsave(here("results", "fig_casestudy_gt_posterior_overtime.png"), p2, width = 8, height = 8, dpi = 150)
message("Saved: fig_casestudy_learn_vs_impose_crps.png, fig_casestudy_gt_posterior_overtime.png")
