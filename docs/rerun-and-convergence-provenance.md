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

Poorly-converged fits (>100 divergences OR max R-hat > 1.05) fell at each round of
the escalation ladder — the settings for each cell are in
`results/tighter_sampler_cells.tsv`:

| Round | Sampler | Poorly converged |
|---|---|---|
| default | `adapt_delta=0.99`, td=20, 3000 samples | 125 |
| ROUND1 | `adapt_delta=0.999`, td=25 | 39 |
| ROUND2 + CONV2 | `adapt_delta=0.9999`, td=30 / 0.999 at 96h wall | 18 |
| CONV3 | `adapt_delta=0.999`, td=25, **6000 samples**, 96h wall | **14** (floor) |

So of 3384 fits, **99.6% (3370) are well-converged** at this point. The residual
**~14 fits (13 tasks)** are dominated by **gt1 (imposed ~1-day generation time)** —
12 of the 14, plus one gt2 and one gt3.

This residual is **not** a generation-time identifiability problem: in the
misspecification grid the generation time is *imposed* (`Fixed(1)` at gt1), so it is
not an estimated parameter. The poor mixing instead comes from forcing a near-zero
generation interval onto data with a much longer true one, which drives the
reproduction-number / Gaussian-process posterior into a stiff, hard-to-sample
geometry. That is a sampling problem, so it is expected to yield to more effort
(more samples, longer warmup, fresh seeds) rather than being a fundamental floor.

## Requirement: converge every cell before analysis

Analysis begins only once **all runs are in and converged** (no non-converged fits,
no runaway divergences). So the residual is not accepted or scored as-is — the retry
ladder continues on the efficient recipe (`adapt_delta=0.999` + generous wall time +
fresh-seed restarts + more samples) until zero cells remain poorly converged. Each
retry round's cells and settings are logged in `tighter_sampler_cells.tsv`.

## Note on the min-ESS diagnostic

`min_ess` (minimum effective sample size over all parameters) is **not** a usable
convergence signal here: it is taken over every parameter including fixed /
deterministic ones whose ESS is degenerate, so it reads as tiny (<100) for ~97% of
fits regardless of sampling quality. Use max R-hat and divergence count instead.
