# Deferred items after the timepoint-parallel refactor

The main scenario pipeline (constRt / incdecRt / casestudy for all three diseases)
is fully migrated to the timepoint-parallel, deterministic `results/raw/` +
`results/agg/` scheme with run-time diagnostics. Two areas were deliberately left
for follow-up.

## 1. Learn-arm path (on hold pending the prior-width decision)

`sim_weightprior` (R/scenario_loop.R), `06f_scenariorun_widepriors.R`,
`07b_widepriors_aggregate.R` and `fig_widepriors.R` still use the OLD conventions:
`sim_weightprior` returns a `summary` list with `return_fit = FALSE`, and `07b`
calls `extract_diagnostics()` (removed from funcs_data.R) on `_summary` files that
the new pipeline no longer writes. Before the learn arm is run, apply the same
treatment used on `sim_scenarios`:

- `return_fit = TRUE` + `fit_diagnostics()` at run time; return `diagnostics`, drop
  the heavy `summary` (or keep it only if the estimated-delay posterior is needed);
- save via `save_scenario()` into `results/raw/` with the deterministic per-tp names;
- rewrite `07b` to read raw per-tp files and write `results/agg/`, dropping the
  `extract_diagnostics`/`_summary` path.

## 2. Figure faceting by horizon and scale

Scoring now emits CRPS for BOTH scales (natural, log) and BOTH horizons (1- and
2-week). The heatmap/plot scripts (`fig1.R`, `fig_casestudy.R`) do not yet facet by
`scale`/`horizon`, so they aggregate over them. `fig_resim.R` and `fig_widepriors.R`
still contain their own inline single-scale (`scale == "log"`) / single-horizon
(`date == max(date)`) scoring. Update these to facet/choose scale + horizon when the
analysis figures are finalised.
