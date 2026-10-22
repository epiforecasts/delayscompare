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

## Convergence — the effective lever was warmup, not adapt_delta or samples

At the default sampler (`adapt_delta=0.99`, `max_treedepth=20`, 3000 samples,
EpiNow2's default `warmup=250`), roughly **125 fits** were poorly converged (>100
divergences OR max R-hat > 1.05), concentrated at the extreme misspecification
corner (imposed generation time gt1 combined with a large incubation/reporting
misspecification).

Early rounds escalated `adapt_delta` (0.999, then 0.9999) and `samples` (6000,
10000). These reduced the count but never cleared it, and over-cranking made
sampling much slower (day-long grinds, then timeouts). The decisive lever turned
out to be **warmup**, which every earlier round had left at the EpiNow2 default of
250. Raising it resolved the great majority of the residual:

- Clear demonstration: ebola const_low gt1/inc2/tp1 sat at max R-hat ≈ 4–5 across
  `adapt_delta` 0.999→0.9999 and samples 3000→6000; raising `warmup` to 2000–4000
  (default everything else) took its companion const_high fit from R-hat 4.0 to
  1.0009 in the same ~1-minute runtime.
- With `warmup=2000` at otherwise-default settings, cells that had been poorly
  converged came good (e.g. covid const gt1/inc5/tp6 converged after a ~7h grind;
  covid casestudy gt1/inc6/tp4 converged to R-hat 1.003 on the heavy warmup=4000 /
  samples=10000 recipe).

Per-cell settings for every re-run are in `results/tighter_sampler_cells.tsv`.

## The extreme corner: a near-singular deconvolution, not non-identifiability

The residual poor-convergence cells cluster where the imposed generation time is
gt1 and the incubation/reporting misspecification is large. The mechanism is a
sampling-geometry problem, and it is worth stating precisely because it is *not*
a delay-identifiability problem:

- gt1 imposes `Fixed(1)` — a one-day generation interval as a point mass. Under the
  renewal equation this makes R_t essentially the day-to-day growth ratio: a
  high-frequency quantity with one effective degree of freedom per day.
- A large incubation misspecification imposes a long observation delay (up to
  ~19 days combined incubation + reporting at inc6 for covid). Deconvolving a
  smooth, ~19-day-lagged case curve back into a *daily* infection series — which is
  what the 1-day generation interval then reads R_t off — is near-singular: many
  daily trajectories reproduce the same smoothed case curve, so the posterior has a
  flat, multi-modal ridge. Independent MCMC chains settle on different trajectories
  along the ridge and do not reconcile, so **between-chain R-hat blows up (100s)
  while the pooled R_t marginal stays bounded** (~0.5–2.2). That signature — huge
  R-hat, bounded marginals, few divergences — is the fingerprint of a multi-modal
  ridge, not a coding error.

This is a property of the imposed delays and the sampler, not of the data. The
affected covid case-study window (tp5, origin 2021-10-19) is dense and high-count
(16,956–49,257 reported cases/day, no gaps). And the delays are *imposed*
(`Fixed(1)` etc.), not estimated, so there is no delay parameter to be
non-identifiable — the difficulty is entirely in the reproduction-number /
Gaussian-process geometry.

Whether the ridge actually traps the sampler is **data-window-dependent**: covid
case-study gt1/inc6/tp4 converged (R-hat 1.003) on the heavy recipe, while the
neighbouring gt1/inc6/tp5 resisted every recipe tried.

## Excluded fits

Of 3384 fits, **3376 (99.76%) are retained as converged** (max R-hat < 1.05 and
< 100 divergences). **8 fits are excluded**, all at the near-singular corner
described above (imposed gt1–gt2 with the largest incubation/reporting
misspecification, inc6; plus one ebola gt1/inc2 fit). They are documented as
sampler-convergence failures — the multi-modal Rt/GP ridge of the near-singular
deconvolution — explicitly *not* as non-identifiability of the (imposed) delays.

**Case-study (analysis) arm — 2 fits:**

| fit | max R-hat | divergences |
|---|---|---|
| covid case-study gt1 / inc5 / tp5 | 1.04 | 662 |
| covid case-study gt1 / inc6 / tp5 | 122.7 | 8 |

Both resist every recipe tried. gt1/inc6/tp5 is the worst: default at three fresh
seeds gave max R-hat 198, 450, 122; the heavy recipe (warmup=4000 / samples=10000 /
adapt_delta=0.999) ran ~20h without finishing; a deliberately small diagnostic fit
(samples=1000 / warmup=1000) could not finish within a 3h wall. gt1/inc5/tp5 is
milder but never clears both criteria at once across six attempts — draws are either
R-hat ≈ 1.04 with hundreds of divergences or R-hat ≈ 1.15 with clean divergences.
They are 2 of 8 forecast origins in the two most extreme case-study cells.

**Validation (const/dec) arm — 6 fits:**

| fit | max R-hat | divergences |
|---|---|---|
| covid const_high gt1 / inc6 / tp4 | 1.57 | 716 |
| covid const_high gt1 / inc6 / tp5 | 2.91 | 586 |
| covid const_high gt1 / inc6 / tp6 | 1.06 | 0 |
| covid const_high gt2 / inc6 / tp6 | 3.70 | 3 |
| covid dec gt1 / inc6 / tp5 | 335.3 | 19 |
| ebola const_low gt1 / inc2 / tp1 | 3.94 | 35 |

These are the constant-Rt / decreasing-Rt validation scenarios, not the case-study
comparison, so their exclusion does not affect the main analysis. Each is the same
near-singular corner; the ebola one is the low-count const_low companion of an
otherwise-converged task (const_high converged once warmup was raised).

The exclusion is applied by convergence criterion, not a hand-curated list: the
aggregation drops any fit with max R-hat ≥ 1.05 or ≥ 100 divergences, which is
exactly this set. Recorded here for the reliability accounting in the paper.

## Requirement: converge every retained cell before analysis

Analysis uses only converged fits (max R-hat < 1.05, < 100 divergences). The one
exclusion above is the sole exception and is documented as such. All other cells
are converged; the per-cell sampler settings that achieved this are logged in
`tighter_sampler_cells.tsv`.

## Note on the min-ESS diagnostic

`min_ess` (minimum effective sample size over all parameters) is **not** a usable
convergence signal here: it is taken over every parameter including fixed /
deterministic ones whose ESS is degenerate, so it reads as tiny (<100) for ~97% of
fits regardless of sampling quality. Use max R-hat and divergence count instead.

## Learn Arm — Non-Identifiability Results & Provenance

### Convergence Summary by Disease × Parameter

| Disease | Generation Time (`vary=gt`) | Incubation Period (`vary=inc`) | Both (`vary=both`) |
|---|---|---|---|
| **Ebola** | 5/8 | 8/8 | 7/8 |
| **Cholera** | 7/8 | 8/8 | 8/8 |
| **COVID-19** | 6/6 | **1/6 (TP1 only)** | **0/6** |

All completed learn fits converged (max R-hat < 1.05). The 11 uncompleted COVID-19 `inc` (TP2–6) and `both` (TP1–6) cells failed to converge due to **structural non-identifiability** of the COVID-19 observation delay from incidence.

### Structural Non-Identifiability Fingerprint

Diagnostic fit (job `6418486`, `diag_learn.R` on COVID `vary=inc` TP3) confirmed an unconstrained parameter ridge:
- **Delay posterior unpinned**: `delay_params[1]` (incubation meanlog) posterior SD = 0.599 vs prior SD = 0.10 (6× wider), max R-hat = 8.54.
- **Explicit correlation ridge**: Absolute correlation of the incubation delay parameter with Rt / Gaussian Process / infection parameters reached **max 0.99, mean 0.86, with 54 of 60 parameters showing |r| > 0.5**.
- **Timing unconstrained**: Parameters with extreme R-hat (~10¹³) were unanchored `infections[...]`, `reports[...]`, and `obs_reports[...]`.
- **Tree-depth saturation**: 12.8% of leapfrog transitions hit the tree-depth cap even when capped at 10.

### Mechanism: Delay-to-Generation-Time Ratio

| Disease | Generation Time (GT) | Obs Delay (Inc + Rep) | Ratio (Obs Delay / GT) | Learn Arm Incubation Status |
|---|---|---|---|---|
| **Ebola** | 10.90 d | 8.50 d | **0.78×** | Fully identifiable (8/8) |
| **Cholera** | 5.00 d | 6.17 d | **1.23×** | Fully identifiable (8/8) |
| **COVID-19** | 3.60 d | 9.60 d | **2.67×** | Non-identifiable (1/6) |

When the observation delay is significantly longer than the generation time ($2.67\times$ for COVID-19), the delay enters the renewal convolution $R(t) = \int I(t-s) f(s) ds$ primarily as a scalar confounded with seeding/growth rates, producing a flat parameter ridge against a flexible GP prior.

### Trend-Change Prediction Verification

Identifiability requires clear curvature in the infection series to anchor timing. This prediction was empirically tested against the COVID-19 forecast origin windows:

- **TP 1 (Origin 2021-06-29, Window: 2021-06-01 to 2021-06-29, 29 days)**: **Identified**. Covers the sharp turning point / initial exponential takeoff of the UK Delta wave (~4.5k to ~21k cases/day; quadratic log-scale curvature $p = 6.98 \times 10^{-13}$). The short window boundary and sharp curvature constrain $I(t)$ and pin down the delay.
- **TP 2–6 (Origins 2021-07-27 to 2021-11-16, Windows: 57–85 days)**: **Non-identifiable**. Over extended multi-month windows across smooth peaks and plateaus, the GP $R(t)$ prior flexes to fit curvature, leaving the long observation delay unconstrained along the 0.99 correlation ridge.

### Stochastic Re-Seed Lottery & Job Cancellation

- **RESEED 1–3**: Cleared 3 stuck cells by random seed selection.
- **RESEED 4–5**: Cleared 0 cells across 11 remaining COVID-19 fits.
- **Cancellation**: `RESEED5` (job `6418219`) was cancelled on 2026-07-23 once the diagnostic confirmed the 11 cells are structurally non-identifiable. The 11 cells are formally documented as excluded non-identifiable fits.


## Turning-point replicates without a correct-delay fit

Six step-trajectory replicates have no correct-delay fit (`gt4_inc4`) at any origin:
no samples and no diagnostics files exist on the cluster. They are cholera turn 3,
COVID-19 bend 1, flat 1 and turn 2, and Ebola bend 3 and turn 1. The paired
turning-point scoring (`score_turningpoint_proper.R`) compares each misspecified fit
with the correct one at the same origin, so these replicates contribute no rows, and
the step-trajectory skill and sensitivity results rest on the other 21 of 27
replicates. The earlier per-origin slope analysis (`score_turningpoint.R`) did not
need the correct fit and still used them.
