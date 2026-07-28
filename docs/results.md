# Results

Findings from the delayscompare analysis. Methods live in `analysis_plan.md`;
convergence/exclusion provenance in `rerun-and-convergence-provenance.md`; this
document holds the substantive results. Sections marked TBD are not yet analysed.

## 1. Learn arm: the observation delay is not identifiable for covid

Estimating the delay distribution from incidence (the *learn* arm, `weight_prior=FALSE`)
succeeds for the **generation time** but fails for the **observation delay** in covid,
while ebola and cholera identify both. Completion by disease × varied quantity:

| disease | learn generation time | learn incubation | learn both |
|---|---|---|---|
| covid | 6/6 | **1/6 (tp1 only)** | **0/6** |
| ebola | 5/8 | 8/8 | 7/8 |
| cholera | 7/8 | 8/8 | 8/8 |

The stuck covid `inc`/`both` fits do not converge at any sampler setting or seed. The
single covid `inc` cell that does complete (tp1) is *not* an exception: it converges
only because its sparse window leaves the delay prior-dominated (posterior = prior;
see Diagnostic evidence), so covid's observation delay is never actually informed by
the data — "completed" here does not mean "identified".

### Diagnostic evidence
Short tree-depth-capped fits of stuck cells (jobs 6418486 tp3, 6467755 tp1) plus a
production-settings fit of the one cell that converges (tp1, job 6467858) localise
the failure to the incubation delay and show it is never informed by the data:

- **The delay is confounded with Rt/infection timing — the ridge.** In both a stuck
  cell (origin 2021-08-24) and the converging tp1 cell (origin 2021-06-29), the
  crippled short chains show |correlation| of the incubation parameter with the
  Rt/GP/infection parameters up to **0.99** (mean 0.80–0.86). The worst-mixing
  parameters are the infection and reported-case time series (`infections[...]`,
  `reports[...]`) — the model cannot place infections in time.
- **The data does not inform the delay.** At production settings the tp1 fit
  converges cleanly (R-hat 1.01, 0% tree-depth saturation, 7 divergences) and its
  incubation posterior is **exactly the prior**: mean 1.607 (prior 1.608), SD 0.100
  (prior 0.10). Zero information gained — tp1 samples only because its sparse 29-point
  window lets the prior dominate.
- **More data makes it intractable, not identified.** The later windows (tp2–6, ~84
  points) engage the confounded likelihood and stop sampling — the sampler grinds at
  the tree-depth cap (production td=20 → up to 2²⁰ leapfrog steps/iteration → runs of
  days), R-hat ~10¹³ on the timing parameters, and does not converge under any
  setting or seed.

So covid's observation delay is **prior-dominated where samplable (tp1) and
computationally intractable otherwise (tp2–6)** — never identified from case counts.
(Reproduce: `diag_learn.R`, `diag_learn_tp1.R`, `diag_learn_tp1_prod.R` on the cluster.)

### Mechanism
Reported cases are infections convolved with the delay, R(t) = ∫ I(t−s) f(s) ds.
During exponential growth I(t) = I₀e^{rt}, this collapses to
R(t) = [I₀ · M_f(−r)] · e^{rt}: the delay enters only through a scalar confounded with
the seeding level, so on any log-linear stretch it is non-identifiable and only the
growth rate r is recoverable. The delay becomes identifiable **only at trend changes
(curvature)** — a peak, trough or bend is the landmark that fixes where the delay
places the observed signal relative to infections. A flexible Rt/GP worsens this by
manufacturing curvature to explain the peak's timing instead of the delay. The
generation time stays identifiable because it is pinned by the growth *rate*, present
even under pure exponential growth.

### Why covid specifically
The effect scales with the ratio of observation delay to generation time:

| disease | generation time | observation delay | ratio |
|---|---|---|---|
| covid | ~3.6 d | ~9.6 d (5.2 + 4.4) | **~2.7** |
| ebola | ~10.9 d | ~8.5 d | ~0.8 |
| cholera | ~5 d | ~6.2 d (1.8 + 4.4) | ~1.2 |

covid's delay is far longer than its transmission timescale, so the case signal is
smeared over ~10 days while dynamics turn over every ~3.6 days. This is the
natural-parameter version of the impose arm's short-GT × long-delay near-singular
corner (§2).

### Why trend changes do not rescue it here
In principle the delay is identifiable at trend changes (curvature is the landmark;
§Mechanism). It was tempting to predict that the one covid `inc` cell that converged
(tp1) did so because its window contains a trend change — but the diagnostics refute
that: tp1's incubation posterior equals its prior (no information), and the delay↔Rt
correlation is ~0.99 at tp1 as well. The reason is the **flexible Rt/GP**: it can
manufacture curvature at any time, so whatever bends the covid case curve contains are
explained by moving Rt rather than by the delay. The curvature that could identify the
delay is absorbed by the reproduction-number process. Identifying the delay would
require constraining the Rt/GP (a stiffer prior / longer length-scale) or external
timing data — not simply a window with a bend.

### Implication
For a pathogen like covid (short generation time, long reporting delay) the
observation delay cannot be learned from case counts and must be imposed; the
generation time can be learned. This gives the impose-vs-learn comparison a clean
asymmetry.

## 2. Reliability accounting (impose arm)

Of 3384 impose-arm fits, **3376 (99.76%) converged** (max R-hat < 1.05 and < 100
divergences). **8 fits excluded** at the extreme gt1–gt2 × inc6 corner — a
near-singular deconvolution (imposed one-day generation time against a long imposed
observation delay), the same short-GT × long-delay mechanism as §1. Enumerated with
per-fit R-hat/divergences in `rerun-and-convergence-provenance.md`. The effective
sampler lever was warmup, not adapt_delta or samples.

## 3. Q1 — sensitivity of forecast skill to delay misspecification (TBD)

Figures generated (`fig1_<disease>_crps_heatmap.png`, rank, marginal) but the formal
read (log-CRPS score model per the analysis-plan Evaluation section) is not yet done.
Do not state conclusions until scored.

## 4. Q2 — impose vs learn (TBD)

Case-study condition CRPS computed (`score_casestudy_conditions.R`); case-study
heatmaps generated (`fig_casestudy_*`).

**Learn arm aggregated** (`results/widepriors/`, via `07b_widepriors_aggregate.R`
with the convergence filter; figure `fig_widepriors_crps.png`). Cells retained by
disease × varied quantity:

| disease | gt | inc | both |
|---|---|---|---|
| ebola | 8/8 | 8/8 | 8/8 |
| covid | 6/6 | **1/6 (tp1)** | **0/6 (excluded)** |
| cholera | 7/8 | 8/8 | 8/8 |

covid `both` has no aggregated cell (all non-identifiable, §1); covid `inc` is tp1
only. ebola is complete; cholera is complete bar `gt/tp7` (one stuck cell). The
covid asymmetry — generation time learnable, observation delay not — carries into the
impose-vs-learn comparison. Formal log-CRPS comparison across impose vs learn still
TBD.
