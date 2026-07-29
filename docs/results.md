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

## 3. Q1 — sensitivity of forecast skill to delay misspecification

Two families of simulated scenarios, each a 6×6 GT×INC misspecification grid scored
against known ground truth:

- **Artificial Rt trajectories** (const<1, const>1, increasing, decreasing) —
  `fig1_<disease>_*` (heatmap, rank, marginal).
- **Disease-like re-simulated trajectories** (ebola-like / covid-like /
  cholera-like), regenerated fresh (03 → 05 under current code) then run over the
  grid — `fig_resim_<disease>_crps_heatmap.png`, `fig_resim_combined_crps.png`,
  marginal. This is the realistic-dynamics counterpart to the trivial trajectories.

**Resim convergence** (warmup=500): ~99% converged. Retained cells — ebola 36/36,
cholera 36/36, covid 30/36 (the gt6 = ×2 generation-time row was still finishing at
figure time; re-aggregate to fill it). ~10 scattered cells excluded by the
convergence filter, *not* concentrated at the gt1×inc6 corner (the simulated data is
smoother than real surveillance series, so the near-singular corner that broke the
case-study/learn arms does not bite here).

### Sensitivity score model
Per-origin log-scale CRPS (samples equalised to 2000) across all simulated scenarios
(trivial + disease-like), modelled with a GAMM (`log_crps ~ s(gt, by=disease) +
s(inc, by=disease) + trajectory + s(time, by=disease) + horizon`, `mgcv::bam`);
marginal log-CRPS vs misspecification level with 95% CIs, no significance testing
(`scripts/score_q1_gamm.R`; `fig_q1_sensitivity.png`).

**Forecast skill is largely insensitive to delay misspecification.** The marginal
log-CRPS moves by < ~0.02 across the *entire* GT and INC misspecification range
(no-delay → ×2), with CIs that overlap across levels for almost every
disease × axis — i.e. no clear difference between correct and misspecified delays.
By far the largest term is `s(time)` (variation across forecast origins), which
dwarfs the misspecification effects. This is the informative-null outcome the plan
anticipated: for short-term *case* forecasting, getting the generation time or
observation delay wrong barely changes skill.

Weak exceptions, all small: covid incubation misspecification is slightly harmful at
the extreme (inc = ×2), and covid generation-time over-specification is marginally
*better* — both tiny. **cholera is flat on both axes** (weekly data, incubation far
below the weekly bin, so delay misspecification is undetectable — as expected).

Consistent with Q2 (§4): informing the model about delays does little for short-term
case forecasts. Caveat: the simulated series are easy to forecast, so absolute CRPS
is tiny; the Rt-scoring arm of Q1 (synthetic-only ground-truth Rt) is still TBD, and
covid's resim gt6 row was pending at fit time (re-run when complete).

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
only. ebola is complete; cholera is complete bar `gt/tp7` (one stuck cell).

### Impose-vs-learn score model
Per-origin log-scale CRPS (posterior samples equalised to 2000), modelled with a
GAMM (`log_crps ~ condition*disease + s(time, by=disease) + horizon`,
`mgcv::bam`); condition marginal effects with 95% CIs, no significance testing
(`scripts/score_q2_gamm.R`; `fig_q2_marginal_effects.png`). Marginal log-CRPS
(1-week horizon; lower = better):

| disease | no-input | impose | learn GT | learn delay | learn both |
|---|---|---|---|---|---|
| ebola | 0.27 | 0.20 | 0.20 | 0.20 | 0.20 |
| covid | **0.21** | 0.51 | 0.48 | 0.36† | — |
| cholera | −0.11 | **−0.13** | 0.15 | 0.14 | 0.13 |

†covid learn-delay is tp1 only → very wide CI [−0.08, 0.80], uninformative.

**Learning never beats imposing.**
- **ebola**: impose ≈ learn ≈ 0.20, all CIs overlapping — no clear difference
  between imposing and learning (informed slightly better than no-input, overlapping).
- **covid**: no-input (0.21, CI [0.03, 0.39]) is clearly better than impose (0.51,
  [0.32, 0.69]) and learn-GT (0.48) — the CIs barely overlap. Learning ≈ imposing
  (the delay is prior-dominated, §1), and both are *worse* than ignoring delays.
  Learn-both is absent (non-identifiable). So for covid, informing the model about
  the observation delay degrades short-term case forecasts.
- **cholera**: impose ≈ no-input (best, ~−0.12); learning is worse (~0.14) with
  partial CI overlap — weekly data can't inform the delay, so estimating it injects
  noise.

**Overall**: imposing literature delays is never beaten by learning them; learning
is at best equal (ebola) and at worst harmful (cholera), and for covid the
observation delay cannot be learned at all. A secondary result — for short-term
*case* forecasting, delay structure can hurt (covid: no-input best) — is consistent
across the informed conditions.

Caveats: single real outbreak per disease (no replication); covid learn-delay rests
on one origin; the GAMM `s(time)` is weak with ≤8 origins. Q1 (simulated
disease-like scenarios) GAMM still TBD.
