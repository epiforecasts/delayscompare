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

**Resim convergence** (warmup=500, one fresh-seed re-roll of transient failures):
all three diseases **36/36 cells retained**. ~7 individual forecast origins excluded
by the convergence filter (covid 6, ebola 1, cholera 0), scattered and *not*
concentrated at the gt1×inc6 corner — the simulated data is smoother than real
surveillance series, so the near-singular corner that broke the case-study/learn
arms does not bite here.

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
is tiny.

### Rt estimation (a consistency check, not a finding)
Scoring the *reproduction-number* forecasts against the synthetic true Rt
(`score_q1_rt_gamm.R`; `fig_q1_rt_sensitivity.png`; Rt is synthetic-only) shows the
expected behaviour and mainly serves to validate the pipeline. Rt-CRPS is U-shaped in
generation-time misspecification (minimum at the correct GT, e.g. ebola 0.007 →
0.17 at ×2) and near-flat in incubation misspecification.

This GT effect is **mechanical, not empirical**: at a fixed observed growth rate r the
reproduction number is a deterministic function of the generation interval
(Euler–Lotka, R = 1/M_g(−r)), so changing the assumed generation time necessarily
rescales the inferred Rt. Recovering that dependence is a sanity check that the
estimator behaves as the theory dictates — it is not a result about forecasting or
about the data. (The observation delay barely enters Rt, also unsurprising.)

The informative content of Q1 is therefore the *case-forecast* side above: skill is
insensitive to delay misspecification over the 1–2 week horizon, even though the
underlying Rt reinterpretation is (necessarily) GT-dependent.

### Medium horizons: where GT misspecification starts to matter, and why

The 1–2 week insensitivity above leaves open the operationally hard 3–4 week question.
A dedicated horizon sweep addresses it: the disease-like re-simulated trajectories, GT
axis varied (0→×2), incubation held correct, 20 replicate noise realisations each,
forecast horizon extended to 42 days and scored at 1–6 weeks with a
`te(gt, horizon, by=disease)` GAMM (`06h_scenariorun_horizon.R`,
`score_horizon_gamm.R`; `fig_horizon_sensitivity.png`). 98.5% of ~2,900 fits converge;
the non-converged tail sits at the degenerate near-zero-GT corner and is filtered out.

**A modest medium-horizon effect emerges that the 1–2 week analysis cannot see.** The
marginal log-CRPS gap between correct and worst-misspecified GT is negligible at 1–2
weeks (< 0.02) and grows monotonically to ≈0.09–0.10 by 6 weeks (ebola 0.093, covid
0.096, cholera 0.066) — a ~10–15% relative degradation, resolved at 20 replicates. The
effect scales with generation-time *length*, not shortness: ebola (longest GT,
comparable to the 42-day window) carries the most, covid less, cholera least. covid
shows a clean U-shape with its minimum at the correct GT; ebola and cholera are
monotone (shorter- or longer-than-correct GT respectively forecasts marginally better),
so there is no single universal direction.

**The mechanism is trajectory curvature, not epidemic phase.** A constant-Rt phase test
isolates the cause: replicate const-low (R = 0.8, sustained decline) and const-high
(R = 1.2, sustained growth) trajectories at matched |R − 1|, same horizon sweep
(`05e_simulatedata_const_replicates.R`, `score_horizon_phasetest.R`;
`fig_horizon_phasetest.png`). On constant Rt the case forecast is **GT-invariant at
every horizon** — flat across the full 0.25×–2× GT range even at 6 weeks (ebola
const-high 6-week log-CRPS: 0.050, 0.049, 0.046, 0.044, 0.039 for GT ×0.25→×2), for
growing and declining epidemics alike (GT slope |·| < 0.007 in every cell, versus
+0.144 in the resim). Phase per se does nothing.

This locates the effect precisely. Under constant R, log-incidence is a straight line:
the model absorbs any GT into a re-scaled constant Rt (Euler–Lotka, §1), and
extrapolating a straight line forward is GT-independent — so both estimation *and*
forecasting are GT-blind. The real disease trajectories are *curved* (waves, peaks,
turnarounds); a misspecified GT mis-propagates that curvature forward over the forecast
window, and the error compounds with horizon and with GT length. Curvature is the
second-order information that breaks the first-order degeneracy of §1.

Operationally: getting the generation interval wrong barely costs short-term case-
forecast skill, and even at 3–6 weeks costs little — and only where the growth rate is
*changing* over the forecast window. The 3–4 week horizon is hard for reasons other
than the generation interval.

### Delays matter only where R is changing — the general statement

The medium-horizon result above points at a sharper claim: delay misspecification
degrades what we care about — how well R is estimated and how good forecasts are —
**only where the reproduction number is changing**, and is essentially free during
exponential phases. Two lines of evidence establish it, and separate "R changing" from
the special case "R crosses 1".

**Resim stratification.** Across the resim forecast origins, the GT-misspecification
penalty scales with the change in the true R over the forecast window (ΔR from
`rt_<disease>.rds`), not with R crossing 1 (`investigate_turningpoints.R`;
`fig_horizon_turningpoint.png`). Mean |GT slope| at 6 weeks by ΔR tertile: 0.0015
(R≈flat) → 0.020 → 0.099 (R changing a lot). Origins where R changes substantially
*without* crossing 1 still show large penalties; cholera (true R almost constant over
any window) anchors zero.

**Controlled experiment.** Three synthetic trajectories with a single change at day 105
— `flat` (R = 1.1), `bend` (R 1.3 → 1.0, changes but never crosses 1), `turn`
(R 1.3 → 0.7, crosses 1 at a peak) — swept over *both* delay axes (generation time and
observation delay) with dense forecast origins, scoring both R-nowcast error and 6-week
forecast CRPS per origin (`05g_simulatedata_turningpoint.R`, `06h` with
`FREQ_FC`/`MAX_TP`, `score_turningpoint.R`; `fig_turningpoint.png`). The per-origin
error slope follows **flat ≈ 0 → bend → turn** in every outcome × axis panel:

| outcome · axis | flat | bend | turn |
|---|---|---|---|
| forecast · observation delay | 0.013 | 0.103 | 0.142 |
| forecast · generation time   | 0.004 | 0.028 | 0.033 |
| R nowcast · observation delay | 0.002 | 0.013 | 0.018 |
| R nowcast · generation time   | 0.019 | 0.045 | 0.059 |

`bend` exceeds `flat` in every panel, so the trigger is R *changing*, not R = 1; and
`bend` (R changes but never crosses 1) nearly matches `turn` for forecasts (0.103 vs
0.142 on the delay axis), so crossing 1 adds little beyond changing R at all — the effect
tracks how much R moves, with no threshold at R = 1.

Two refinements fall out. **The observation delay matters *only* where R changes** —
flat ≈ 0 for both R estimation and forecasts — exactly the identifiability degeneracy of
§1 made dynamic (the delay is absorbed into the level under exponential growth). **The
generation time is the same for forecasts, but for R *estimation* it carries a constant
mechanical bias everywhere** (flat = 0.019): R = 1/M_g(−r) is GT-dependent at any growth
rate, so a wrong GT mis-scales the R *level* even in pure exponential growth (the
Euler–Lotka point of §3), with the turning-point rise added on top. And at turns the two
delays fail differently: the observation delay dominates *forecast* error (mis-timing a
peak is costly), while the generation time dominates *R-estimation* error (R depends on
it directly).

## 4. Q2 — impose vs learn

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
on one origin; the GAMM `s(time)` is weak with ≤8 origins.

### Weekly GT-learn vs impose: does the posterior move, and does it matter?

The monthly comparison above has only 6-8 origins per disease, too few to tell
whether the posterior is actually moving or the CRPS difference is just noise.
Re-run at weekly origins (`freq_fc=1`, `06f_scenariorun_widepriors.R` +
`06d_scenariorun_casestudy.R`, both extended with `FREQ_FC`/`MAX_TP` overrides;
`sim_weightprior` extended to extract and save the generation-time posterior
itself, previously discarded) gives ebola 25-30, cholera 23-30 and covid 7-8
converged, origin-matched pairs of (impose literature GT) vs (estimate GT,
parametric wide prior). Scored with `score_casestudy_learn_vs_impose_weekly.R`;
`fig_casestudy_learn_vs_impose_crps.png`, `fig_casestudy_gt_posterior_overtime.png`.

**The posterior moves, substantially and consistently, not just noisily around
the prior.** Ebola's posterior sits 25-35% below the literature value (10-13 days
vs 16.2) at essentially every one of 25 origins across the full six-month
outbreak — a stable, confident departure, not sampling noise. Covid drifts from
~4.4 days (early June, short window) down to ~3.0-3.9 by late July, crossing the
literature value of 3.6 on the way — consistent with the recovery diagnostic's
asymmetry (an over-long prior corrects slowly as more outbreak-specific data
accumulates) and with the case-study narrative that the literature (earlier-wave)
generation time may be too long for Delta. Cholera drifts the other way, from
~4.7 up to a stable ~5.9-6.2 plateau from August onward, away from the pooled
literature value. None of this movement is turn-gated: it is present across
steady and changing phases alike, matching `diag_recovery_findings.md` and
correcting the turning-point framing floated earlier in this project for GT
specifically (that framing holds for the *observation delay*, §1, not for GT).

**None of that movement changes the forecast.** Across nearly every origin and
all three diseases, the impose and estimate CRPS curves are visually
indistinguishable (`fig_casestudy_learn_vs_impose_crps.png`) — including at the
ebola origins where the posterior sits furthest from the literature value. This
is the real-data confirmation of the turning-point result's `forecast · GT`
finding: the generation time barely moves case-forecast CRPS anywhere, so a
posterior that moves a lot still forecasts about the same. Estimating the GT
buys parameter movement, not forecast improvement, because there is rarely
anything to gain: the model already forecasts about as well on the wrong prior.

**Estimating can actively hurt where data is scarce.** Cholera's three earliest,
shortest-window origins (May 2017, ~22 days of data) show a clear CRPS penalty
for estimating (up to 0.87 vs 1.83 log-CRPS) — a wide prior with too little data
to constrain it adds pure noise. This is a third factor, alongside turning points
and prior direction, in when learning a delay is worth it: it needs both a
misspecified prior *and* enough data to move the posterior without just adding
variance.

**A related finding, not a result but worth recording**: the covid GT-learn
weekly array left origins from 2021-08-10 onward (tp9-24 of 24) unconverged —
genuine 24-hour SLURM timeouts, not crashes, while every origin before it
(steady pre-peak growth) converged in minutes. The window first spans the July
peak at almost exactly that point. Wide-prior GT estimation may become
computationally intractable, not just harder to interpret, once the fitting
window includes a turning point — a sampling-difficulty analog of the
identifiability difficulty already documented for the observation delay (§1).

## 5. Supplement — case-study ranking robustness

The two excluded case-study fits (covid gt1/inc5/tp5, gt1/inc6/tp5) leave those cells
averaged over one fewer forecast origin than the rest (analysis plan #7). Recomputing
the per-cell log-CRPS on the **common** origin set that converged across *all* cells
(drop tp5 uniformly) leaves the gt×inc ranking essentially unchanged:
**Spearman ρ = 0.99 for covid** (the only affected disease), **1.0 for ebola and
cholera** (no exclusions). So the per-cell-available-origins heatmaps are not an
artefact of the dropped origins (`score_casestudy_robustness.R`;
`fig_casestudy_robustness.png`).
