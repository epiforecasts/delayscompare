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

The stuck covid `inc`/`both` fits do not converge at any sampler setting or seed —
they are structurally non-identifiable.

### Diagnostic evidence
A short tree-depth-capped fit of a stuck covid `vary=inc` cell (covid case-study,
forecast origin 2021-08-24) shows:

- **Incubation parameter unpinned**: posterior SD of the incubation mean (log scale)
  = 0.60 vs prior SD = 0.10 — the posterior is *wider* than the prior, with R-hat 8.5.
- **Delay ↔ timing ridge**: |correlation| of the incubation parameter with the
  Rt / Gaussian-process / infection parameters is **max 0.99, mean 0.86, 54/60
  parameters > 0.5**.
- **Timing undetermined**: the worst-mixing parameters are the infection and
  reported-case time series (`infections[...]`, `reports[...]`), R-hat ~10¹³.
- **Sampler grind**: 12.8% of transitions hit the tree-depth cap even at 10
  (production uses 20 → 2²⁰ leapfrog steps per iteration → runs of days).

(Reproduce: `diag_learn.R` on the cluster; diagnostic job 6418486.)

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

### Prediction (not yet tested)
The one covid `inc` cell that identified (tp1) should be the forecast-origin window
containing a clear trend change; the stuck tp2–6 should sit on smoother, near
log-linear stretches. Testable against `casestudydata[["covid"]]` (see next-steps in
`HANDOFF.md` §6.6).

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
heatmaps generated (`fig_casestudy_*`). The learn-arm side needs aggregation
(`07b_widepriors_aggregate.R` → `fig_widepriors.R`) and, given §1, will be
asymmetric (covid: generation time learnable, observation delay not). Formal
comparison TBD.
