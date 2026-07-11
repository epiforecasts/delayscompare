# Re-run and convergence provenance

A record of the failures and sampler tuning across the full 2592-task run, for
reproducibility and for quantifying reliability in the paper. Underlying data:
`results/rerun_log.tsv` (per-round timeouts) and `results/tighter_sampler_cells.tsv`
(cells run with a non-default sampler).

## Timeout failures — transient, not systematic

The original run (24h wall) timed out on **18 of 2592 tasks**. They are spread
across the grid, not concentrated at the pathological corners:

- by disease: covid 8, cholera 6, ebola 4 (covid has the *most*, despite its
  well-behaved generation time);
- by generation-time misspecification level: gt1(3), gt2(2), gt3(4), gt4-correct(4),
  gt5(4), gt6(1) — essentially uniform.

Every one of the 18 completed when re-run on a **48h wall** (0 failures across the
retry batches). So the timeouts were wall-time-limited — a cell that happened to
draw a slow-sampling geometry or a slow node needed more than 24h — rather than
genuine non-termination. There is no systematic set of non-completing cells.

## Convergence tuning — which cells, and the effect

At the default sampler (`adapt_delta=0.99`, `max_treedepth=20`), **125 fits** were
poorly converged (>100 divergences OR max R-hat > 1.05). These were re-run with a
tighter sampler (`adapt_delta=0.999`, `max_treedepth=25`); the 121 affected tasks
are listed in `results/tighter_sampler_cells.tsv` (72 covid, 37 cholera, 12 ebola).

Before → after (with 100 of the 121 re-fitted so far; the 21 hardest no-delay
corners were still running at time of writing and will lower these further):

| Metric | Default | Tighter |
|---|---|---|
| Poorly converged (>100 div OR R-hat>1.05) | 125 | 39 |
| max R-hat > 1.05 | 121 | 38 |
| >100 divergences | 22 | 8 |
| >500 divergences | 7 | 2 |

The residual poorly-converged fits concentrate at gt1 (no-delay generation time),
where the model is most severely misspecified — that non-convergence is the
expected identifiability degradation, reported rather than filtered (plan
decision 7).

## Note on the min-ESS diagnostic

`min_ess` (minimum effective sample size over all parameters) is **not** a usable
convergence signal here: it is taken over every parameter including fixed /
deterministic ones whose ESS is degenerate, so it reads as tiny (<100) for ~97% of
fits regardless of sampling quality. Use max R-hat and divergence count instead.
