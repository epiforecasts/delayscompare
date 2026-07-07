## Aggregate the per-cell results of the recovery/learning diagnostic grid.
## Run after the SLURM array (scripts/diag_gt_recovery_grid.R) completes.

library(here)
suppressMessages(library(dplyr))

files <- list.files(here("results", "diag_grid"), pattern = "^cell_.*\\.rds$", full.names = TRUE)
if (length(files) == 0) stop("No cell results under results/diag_grid/")
d <- bind_rows(lapply(files, readRDS))
cat(nrow(d), "cells across", length(unique(interaction(d$method, d$trajectory))), "method x trajectory combos\n")

## average over replicates; treedepth saturation as a fraction of the 2000 draws
summ <- d |>
  group_by(method, trajectory, misspec, setting) |>
  summarise(
    prior_mean  = mean(prior_mean),
    post_mean   = mean(post_mean),
    post_lo     = mean(post_lo),
    post_hi     = mean(post_hi),
    td_sat_frac = mean(treedepth_hits) / 2000,
    divergences = mean(divergences),
    .groups = "drop"
  )

saveRDS(d, here("results", "diag_grid_all.rds"))
saveRDS(summ, here("results", "diag_grid_summary.rds"))
print(as.data.frame(summ), row.names = FALSE)
