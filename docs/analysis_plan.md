---
bibliography: delayscompare.bib
link-citations: true
---

# Analysis plan

*Research question:* What impact does misspecifying the generation interval and
incubation period have on the quality of infectious disease forecasts?

The question separates into two linked studies:

- **Q1 — Sensitivity (synthetic data, truth known).** How much does misspecifying
  the generation interval or the incubation period degrade short-term case
  forecasts, relative to the effect on Rt? We vary each input directly here; this is the
  well-specified control.
- **Q2 — Impose vs. learn (case studies, truth unknown).** Along the continuum
  from imposing the literature value to letting the data set the delay freely,
  where is forecast skill best? That measures how much the literature value should
  constrain the fit. (Omitting the delay is the degenerate end of the same axis.)

The work relates to an interest in the value of expertise when forecasting
infectious disease dynamics.

## Background

- [Addressing delayed case reporting in infectious disease forecast modelling (PLOS Computational Biology)](https://journals.plos.org/ploscompbiol/article?id=10.1371/journal.pcbi.1010115)
  - Outline methods for correcting reporting delays, accounting for over- and
    under-reporting of cases: rescaling of real-time data; modelling real-time
    data with a mean model offset; imputation of validation case counts;
    exclusion of most recent data reports.
  - Outline different approaches for estimating reporting factors.
  - Compare model performance and prediction coverage.
  - Found that forecasting accuracy and prediction coverage always increase when
    correction methods are implemented. Most appropriate correction method
    varied by context.

- [Differences between the true reproduction number and the apparent reproduction number of an epidemic time series (ScienceDirect)](https://www.sciencedirect.com/science/article/pii/S1755436524000033)
  - Discuss the biases introduced if epidemic time series are assumed to be a
    good proxy for infection incidence.
  - Include simulated data generated using a gamma-distributed convolution
    function plus four SARS-CoV-2 time series from 2020: infection prevalence;
    symptom onset; hospitalisation; and death.
  - Relevant in that they illustrate the value of accounting for differences
    between infection incidence and epidemic time series using convolution
    functions (but in the context of estimating Rt).
  - They focus on the effect of excluding incubation period and reporting delay;
    they assume perfect knowledge of the generation time.

- [Exploring surveillance data biases when estimating the reproduction number (Royal Society)](https://royalsocietypublishing.org/doi/pdf/10.1098/rstb.2020.0283)
  - Compared SARS-CoV-2 Rt estimates from different data sources: test-positive
    cases, hospitalisations, deaths.
  - Highlighted the value in presenting concurrent Rt estimates from different
    data sources — can provide more information than just one or a pooled
    estimate.
  - Conclude that there is no inherent best choice of data source — depends on
    policy context.
  - Delays could vary spatially and temporally.
  - "Comparing each type and source of delay, we judged the benefits of using
    open data to outweigh the minor observed spatial variation of the delay from
    onset to test or admission, although at the expense of increased
    uncertainty."

## Aims and objectives

1. Generate simulated data for COVID-like, cholera-like and Ebola-like
   epidemics.
2. Run multiple forecast scenarios using EpiNow2, in which generation time and
   incubation period are correctly specified vs. deliberately
   mis-specified/missing.
3. Evaluate the impact on sharpness and calibration of 1- and 2-week forecasts.
4. With COVID-19, cholera and Ebola data, evaluate the impact of: assuming no
   incubation period delay vs. including an incubation period delay; assuming a
   generation interval of 1 day vs. a more realistic assumption.
5. With the same case-study data, evaluate whether *estimating* a delay from the
   case series (prior centred on the literature value) improves on imposing or
   omitting it.

## Design decisions

1. **The observation delay is modelled as the incubation period alone; no
   separate reporting delay.** The generation interval and incubation period are
   transferable biological quantities an expert can carry across settings; the
   reporting delay is specific to a surveillance system, so importing a literature
   value for it is a guess rather than expert transfer. Whatever observation delay
   is assumed is held identical across the compared conditions, so it cannot
   confound a within-disease ranking.
2. **In the misspecification grid (Q1), vary the mean only**; the standard
   deviation is held fixed (a noted limitation, since Rt depends on the full
   generation-interval shape). The Q2 learn condition estimates the full shape
   (see decision 5), which partly addresses this.
3. **Retrospective evaluation on final, complete datasets**, cut at each forecast
   origin; real-time right-truncation is deliberately out of scope.
4. **Score on both natural and log scales** (see Evaluation).
5. **We move from imposing to learning the delay by estimating its whole
   distribution from the data, with a single parameter setting how tightly the
   literature shape constrains the estimate.** A high value keeps the estimated
   distribution close to the literature shape (impose); a low value lets the data
   determine it freely (learn); one model then spans the whole continuum, and we
   read off the estimated *shape* as well as the mean. In practice this is a
   nonparametric delay with a Dirichlet prior whose `concentration` we sweep
   (needs a recent EpiNow2 with `Dirichlet()`). We also fit a data-driven estimate
   within a parametric family (gamma or lognormal, `weight_prior=FALSE`) as a
   robustness check: does any finding depend on the free-form flexibility? We drop
   the prior-weighted variant (`weight_prior=TRUE`), since weighting the prior by
   the amount of data keeps the estimate near the imposed value and duplicates the
   impose condition.
6. **Weak identifiability is expected, and grows as concentration falls.** Where
   the fit degenerates is itself the identifiability result, so we report
   posterior shape, contraction and convergence diagnostics rather than quietly
   filtering those fits out.
7. **Sampling: EpiNow2 (Stan) defaults**, tightening the sampler (`warmup`,
   `adapt_delta`, `max_treedepth`) when it reports divergences or saturates the
   tree depth. In the *impose* conditions we exclude from analysis any individual
   scenario forecast origin whose fit did not converge (max R-hat > 1.05 or ≥ 100
   divergent transitions); the exclusion is applied automatically at aggregation by
   this convergence criterion, and each grid cell is then scored over its remaining
   converged origins (a cell is never dropped whole unless every origin fails).
   Because this may drop a small number of origins, the supplement repeats any
   affected comparison on the common set of origins that converged across all cells,
   to confirm the ranking is unchanged. In the *learn* conditions we treat
   non-convergence as an informative outcome rather than discarding it; where
   sampler tightening (warmup, adapt_delta, stochastic re-seeding) cannot achieve
   convergence because a parameter is not identifiable from the data, re-running is
   halted and the cell recorded. The excluded / non-converged set and its provenance
   are documented in `docs/rerun-and-convergence-provenance.md`; the substantive
   findings this yields (which parameters are non-identifiable, and why) belong in
   the results document, not here.

## Methods

### Data

#### Simulated data

- Simulate case series with a renewal model (`simulate_infections()`).
- SARS-CoV-2-like, Ebola-like, cholera-like (artificial Rt trajectory scenarios
  plus case studies).
- Forecasts every four weeks.
- [Estimating the time-varying reproduction number (Wellcome Open Research)](https://wellcomeopenresearch.org/articles/5-112) — used a 12-week rolling window.

We model the observation delay as the incubation period alone.

| Parameter | Value | Distribution | Source |
|---|---|---|---|
| **SARS-CoV-2** | | | |
| Generation interval | 3.6 (s.d. 3.1) days | gamma | @Sherratt2021 |
| Incubation period | 5.2 (s.d. 1.52) days | lognormal | @Sherratt2021 |
| Case ascertainment | 0.40 | | @Colman2023: 20–40% of UK infections ascertained, upper end for the Delta wave |
| R trajectory / horizon | Jun 2020 – Dec 2021 | | |
| **Ebola** | | | |
| Generation interval | 10.9 (s.d. ~8) days | gamma | @Lau2017 |
| Incubation period | 8.5 (s.d. ~5.5) days | gamma | @Nash2024 |
| Case ascertainment | 0.83 | | @Scarpino2015 |
| R trajectory / horizon | Aug 2014 – Feb 2015 | | |
| **Cholera** | | | |
| Generation interval | 5 (s.d. 7.1) days | gamma (shape 0.5) | @Azman2016 |
| Incubation period | 1.77 (s.d. 1.36) days | lognormal | @Azman2013 |
| Case ascertainment | 0.28 | | ≈ 1/3.2 infection-to-case ratio, naive population [@Finger2023]; held fixed though it rose over the outbreak |
| R trajectory / horizon | Apr 2017 – Jul 2018 | | |

No study has estimated the cholera generation interval, so the value we impose is
an assumption. @Azman2016 took a gamma (shape 0.5, rate 0.1; mean 5 days, SD ~7)
from household-contact data [@Weil2009] that capture fast, direct within-household
transmission and mix it with co-primary infections from shared water. Those data
point to a shorter, tighter interval than the assumed distribution, and the
environmental route driving population-level cholera has no well-characterised
generation interval. We therefore treat cholera as the weakest of the three
imposed values and do not read any single number as its true generation interval.

For Ebola, the generation-time estimate (mean 10.9 days, Zaire) comes from a
single study, @Lau2017; the incubation-period value (mean 8.5 days) is a pooled
mean across nine studies in the @Nash2024 review. Neither comes with a
distribution spread, so we assume moderate gamma SDs (~8 and ~5.5 days). The
spread is not well pinned down, and since we measure sensitivity to the
generation interval, that imprecision is itself part of what we test.

#### Rt trajectory scenarios

We simulate four Rt trajectories: constant below 1 (0.8); constant above 1
(1.2); increasing (0.8 → 1.2); and decreasing (1.2 → 0.8).

Case ascertainment is held at the literature value (Table above), and forecasts
carry the nowcast reproduction number forward over the horizon. We use a negative
binomial observation model with estimated reporting overdispersion.

Case studies:

- Sierra Leone Ebola 2014–2016
- Cholera Yemen 2016–2020
- COVID-19 2020–2021

#### Parameterisation scenarios — artificial Rt trajectories

A 6×6 grid crossing the assumed generation interval and incubation period, each
at: no delay; very low (×0.25); low (×0.8); correct (×1); high (×1.25); very
high (×2).

|  | IP: no delay | IP: very low | IP: low | IP: correct | IP: high | IP: very high |
|---|---|---|---|---|---|---|
| **GT: default (1 day)** | | | | | | |
| **GT: very low (×0.25)** | | | | | | |
| **GT: low (×0.8)** | | | | | | |
| **GT: correct** | | | | | | |
| **GT: high (×1.25)** | | | | | | |
| **GT: very high (×2)** | | | | | | |

#### Parameterisation scenarios — case studies

- No incubation period vs. more realistic assumption; generation time of one day
  vs. more realistic assumption.

For each of the generation interval and the incubation period, forecast under
three conditions: **omit** (a one-day generation interval, or a zero-day
incubation period, giving the model no delay to work with), **impose** (the
literature value, held fixed), and **learn** (estimate the delay from the data).
We realise the learn condition as a nonparametric delay with a Dirichlet prior,
sweeping its `concentration` from strongly constraining to fully free
(`{1, 5, 20, 50, 200}`), so one model traces the impose→learn continuum.
Alongside it we fit a parametric estimate (`weight_prior=FALSE`) as a robustness
check, with priors on the delay parameters centred on the literature value. The
width of those priors plays the same role as the concentration and faces the same
tension (too tight and the data cannot move the estimate; too wide and the model
will not sample), so we set it from the identifiability diagnostic: the loosest
relative prior SD that still recovers a known delay and converges (expected
~10–20%, since case counts weakly identify delays).

We take weekly forecast origins so the score model (see Evaluation) has many
forecast-target rows to work with; it accounts for the temporal structure. Weekly
origins also let us stratify learn-arm behaviour by whether the fitting window
contains a turning point (delays are identifiable only when the growth rate
changes; see the identifiability diagnostic).

Depth: all conditions for the generation interval (the central object); and
impose/omit/learn for the incubation period on Ebola and COVID, each delay
estimated on its own with the other held fixed. We do not learn both delays
jointly, which is degenerate from case counts. We drop cholera IP: at ~1.4 days
on weekly data it is unidentifiable and adds nothing.

We feed the score model with log and natural CRPS by condition; the estimated
delay shape and posterior contraction against the literature value; and
convergence diagnostics (R-hat, effective sample size, divergences, tree-depth)
reported as results.

#### Identifiability diagnostic (interpretive)

This is interpretive, not a precondition: the learn arm's forecasts stand whether
or not the delay is recoverable, since a delay the data cannot identify can still
forecast well. It tells us how to read a learn result: did the data inform the
delay, did the posterior stay at the prior, or did the estimated shape absorb
other misspecification?

Identifiability requires the growth rate to change; a constant Rt gives an
exponential curve on which the delay and the reproduction number trade off
exactly. We simulate a known generation time under each Rt trajectory (constants
as negative controls that should not recover; increasing and decreasing as the
informative cases), sweep the prior concentration, and ask whether the posterior
recovers the truth, with sampling diagnostics (ESS, divergences, tree-depth). A
pilot already shows the fully-free end is pathological (tree-depth saturated, ESS
near 1, mean roughly recovered but not shape), so recovery is expected only under
stronger priors.

#### Case-study data provenance and date semantics

The date a series is stamped with determines whether "observation delay =
incubation period" is exact:

- **Ebola** — lab-confirmed cases by **date of symptom onset**, so the delay from
  infection to the recorded date is the incubation period exactly.
- **COVID-19** — UKHSA daily cases by **specimen date** (the date the positive
  sample was taken): the delay is the incubation period plus an unquantified,
  few-day onset-to-specimen gap, which we absorb into the approximation. Final
  snapshot; retrospective.
- **Cholera** — eDEWS **weekly suspected** cases by epidemiological week of
  facility reporting. The incubation period (~1.4 days) is far below the weekly
  bin, so imposed observation-delay *misspecification* is undetectable at this
  resolution; the material caveat is suspected-case ascertainment and weekly
  resolution. The learn condition is still run for cholera (the posterior simply
  reflects uninformative data).

### Evaluation

Score forecasts on both the natural and log scales (log via `log(x + 1)` on
observations and predictions, following the eval-by-method convention), at the
one- and two-week horizons, using CRPS from `scoringutils`. Rt is scored on the
synthetic data only (Q1); the case studies have no ground-truth Rt, so only case
forecasts are scored there. Decompose CRPS into over-prediction, under-prediction
and dispersion.

**Primary comparison — a score model.** Following Sherratt et al.
(eval-by-method), we model the log-scale CRPS (computed from log-transformed
forecasts and observations) with a Gaussian generalised additive mixed model
(`mgcv::bam`, identity link), modelling the log score rather than putting a log
link on the natural-scale score. The effect of interest is the **condition** (in
Q1, the misspecification level). A candidate specification:

```
# Q2 (impose vs. learn)
log_crps ~ condition * disease + s(time, by = disease) + horizon

# Q1 (sensitivity)
log_crps ~ s(level, by = disease) + trajectory + s(time, by = disease) + horizon
```

We report the condition **marginal effects with confidence intervals**; no
significance testing. Overlapping intervals are a first-class result ("no clear
difference"), which is what makes the write-up robust to inconclusive outcomes.

**Outcome mapping, agreed in advance** (so any result is writable without new
choices):

- *Learn's interval below impose in ≥2 diseases* → "learning improves forecasts."
- *Intervals overlap* → "no clear difference between imposing and learning."
- *Concentration effect flat or non-monotonic* → "forecast skill is insensitive
  to the impose–learn choice."
- *Recovery fails* (posterior stays near the prior) → we read the learn arm as
  prior-dominated: any forecast change comes from the prior, not from the data
  informing the delay.
- *Q1 CRPS flat across the grid* → the informative result: short-term case
  forecasts are robust to delay misspecification because the Gaussian process
  absorbs it, even though Rt is biased as expected [@Gostic2020].

This analysis is descriptive: no inferential null-hypothesis testing. Exploratory
observations (e.g. an implausible estimated delay shape that nonetheless
forecasts well) are labelled exploratory.

Notes:

- CRPS → both sharpness and calibration.
- Median Absolute Deviation (MAD) → sharpness.
- Interval coverage → calibration.
- Specifically comparing: slightly mis-specified (low/high) vs. very
  mis-specified (very low/very high) vs. no delay. I.e. is a slightly bad
  estimate for the incubation period still better than assuming no delay; is a
  very bad estimate for the incubation period better than assuming no delay?
  Likewise for the generation interval.

#### Turning-point evaluation without conditioning on the outcome

*Specified on 2026-10-16, before any of these scores were computed.*

Stratifying ordinary CRPS by the change in R over the forecast window conditions
the evaluation on a smoothed function of the target, which breaks propriety (the
forecaster's dilemma; Lerch et al. 2017). The turning-point question is therefore
split in two and answered with measures that do not condition on the outcome,
applied to the step trajectories (flat, bend, turn; both delay axes) and to the
disease-like replicates (generation-interval axis). No new outcome is introduced:
everything is computed from the log-scale case outcomes already scored, `log(x + 1)`
of daily cases at one to six weeks after the origin, or from weekly growth rates
derived from the same forecasts. On the log scale the score already reflects the
growth since the origin (Bosse et al. 2023), so a forecast that misses a turn is
penalised without any stratification.

*Does the delay choice change the forecast at turning points?* Primary measure: the
energy distance between the six-week log-scale forecast path under each
misspecified delay and under the correct delay at the same origin. No outcome enters
it, so it may be summarised by trajectory and by where the change sits relative to
the origin (ahead of it, behind it, neither).

*Is the correct delay better?* Primary measure: the log-scale CRPS at each weekly
horizon, reported as the paired difference (misspecified minus correct) averaged
over all origins, never over an outcome-defined subset, with 95% intervals by
disease, axis, level and horizon.

*Is it better specifically at turning points?* Secondary measures, all proper and
all reported, on the weekly growth rates `g_k = log(W_k + 1) - log(W_(k-1) + 1)` for
k = 1–6, where `W_k` is the case total in week k after the origin and `W_0` the last
observed week, so that `g_0` is the growth rate known at the origin. A turn is growth
departing from `g_0`. The measures are: the threshold-weighted CRPS of each `g_k`
with chaining `v(g) = g` if `|g - g_0| > c` and `g_0` otherwise, averaged over the
six weeks; the threshold-weighted energy score of the growth path with chaining
`v(g) = g` if `max_k |g_k - g_0| > c` and the constant path `g_0` otherwise; the
outcome-weighted energy and variogram scores of the growth path with weight
`plogis((max_k |g_k - g_0| - c) / (0.3 c))`; and the unweighted energy score of the
growth path. Thresholds c = 0.05, 0.1 and 0.2.

Outcome mapping, agreed in advance:

- *Energy distance larger once the change is behind the origin than when it is
  ahead or absent* → "the delay choice moves the forecast once a change in
  transmission is in the fitting window".
- *Energy distance similar across these strata* → "the delay choice moves the
  forecast about equally whether or not transmission is changing".
- *Primary CRPS difference above zero with intervals excluding zero in at least two
  diseases* → "correctly specified delays forecast better".
- *Intervals overlapping zero* → "no clear difference in forecast skill".
- *Threshold-weighted growth scores favouring the correct delay more strongly than
  the primary* → "the advantage of correct delays is concentrated at turning
  points"; otherwise no turning-specific skill claim is made.
- *Secondary measures disagreeing with each other* → reported as such.

*Amendment, 2026-10-21, made after a first summary had been seen.* The skill
comparisons are fitted with the score GAMM of the primary comparison above (level as
a categorical effect against the correct delay, an origin random effect, horizon as
a fixed effect for the log-scale CRPS), not as paired means with independent
intervals, which ignored the dependence between rows from the same origin. The
mapping above is applied per level, because levels on either side of the correct
value can have effects of opposite sign. The turning-emphasis comparison is made
against the unweighted energy score of the growth path, since the weighted growth
scores are not on the scale of the log-scale CRPS.

*Exploratory addition, 2026-10-21, after the results above had been seen.* The skill
GAMM is also fitted within each state of transmission known at the origin: for the
step trajectories, whether the change is already inside the fitting window; for the
disease-like replicates, the tercile of the change in true R over the 42 days before
the origin. This splits by a label available when the forecast is made, so it does
not condition on the outcome.

*Amendment, 2026-10-22.* Fitting one model per disease, axis, level and state gave
about a hundred separate intervals and a patchwork of effects in both directions.
The reported skill analysis is instead one model per trajectory family, across
diseases and both delay axes, in line with the single-model specification of the
primary comparison: for each axis, misspecification enters as the log multiplier
above 1 (too long), minus the log multiplier below 1 (too short) and an indicator of
the degenerate level, each interacting with the state known at the origin, with the
correct delay as the shared reference, with horizon as a fixed effect,
random intercepts and slopes by disease, and an origin random effect. Effects are
reported per doubling, per halving and for the degenerate level (a fixed one-day
generation interval, or no observation delay), together with the difference between
the two axes.

*Exploratory additions, 2026-10-22 to 2026-10-29, after the results above had been
seen.* The energy distance between forecasts under a misspecified and the correct
delay uses no outcome, so it is modelled against the change in true R before and
after the origin, against the days from the change to the origin on the step
trajectories, and against the change in observed growth of reported cases over the
eight weeks before the origin, which a forecaster can compute at the time. The skill
GAMM is also refitted on the step trajectories with each misspecification term given
its own coefficient per scenario and origin time, flat trajectories at the same
times giving the baseline. Time enters as a factor because the scores jump once
the change reaches the data, and a smooth spreads that jump across the change. The
time from the change to the origin is fixed by the design, and every origin is
scored, so none of these condition on the outcome.

#### Dense-origin and step-up turning-point experiment

*Specified on 2026-10-29, before any of these fits were run.*

The exploratory analyses above suggest that a misspecified delay costs nothing in
skill at origins before a change in R, even when most of the forecast window lies
after it, and costs most once the first signs of the change reach the data. That
rests on a single origin a week after the change, on step trajectories that only
decline, and on a mechanism nobody has tested, and the disease-like replicates
showed nothing comparable.

Whether the delay choice matters "at turning points" cannot be answered with proper
scores. Scores stay proper averaged over all forecasts and within subsets defined by
information the forecaster has at the origin. A turning point is not such
information: around it, the change in R is mostly in infections not yet reported,
and it drives the cases being scored. Subsetting or weighting comparisons by the
true change in R near the origin therefore rewards forecasts whose quirks suit
those origins, for instance wide forecasts when something unexpected happens. The
question this experiment answers instead is whether the delay choice matters once a
change is visible in the reported data, which is a statement about information the
forecaster has.

*Design.* Step trajectories as above (change on day 105), now in both directions:
bend (R 1.3 to 1.0), turn (1.3 to 0.7), and their mirrors bendup (1.0 to 1.3) and
turnup (0.7 to 1.3). Simulated from 100 initial infections, R = 0.7 for 105 days
leaves almost no cases by the change, so for the step-up trajectories the initial
infections are scaled to give about 1000 infections on the change day. Three
replicates each. Forecast origins every three days from 14 days before to 34 days
after the change (17 origins). The flat trajectory's existing origins between 21
days before and 49 days after the change are the baseline. Disease-like replicates
1 to 10 get weekly origins between their existing four-weekly ones, so that
together they form a weekly grid from day 35 to day 224. Both delay axes: the
generation interval at levels 1 to 6 with the correct observation delay, and the
observation delay at levels 1 to 3, 5 and 6 with the correct generation interval.
Fitting settings and the convergence filter are as for the existing turning-point
fits. This is 6732 step and 6930 disease-like fits.

*Measures.* Primary: log-scale CRPS at weekly horizons one to six. Mechanism: CRPS
of log R at the origin (the estimate on partial data) against the true log R, and
the signed error of its median; this scores an estimate, not a forecast, so it can
be broken down by anything. Sensitivity: energy distance between the forecast under
each misspecified delay and the forecast under the correct delay, as above; no
outcome enters it.

*Visible change.* Each origin is described by the observed growth change, the
absolute difference between the mean weekly growth of reported cases over the last
two weeks before the origin and that over the two weeks before those, computed from
log(weekly cases + 1) with only data up to the origin.

*Primary model, both families.* Log-scale CRPS is modelled with the over, under and
degenerate-level terms per axis of the overall skill GAMM, each interacting linearly
with the observed growth change, plus horizon, a disease random effect and an origin
random effect, fitted separately for the step trajectories (all four, plus flat) and
the disease-like replicates (all origins of replicates 1 to 10 from both grids).
Effects are reported per doubling, per halving and for the degenerate level, at the
median and 90th percentile of the observed growth change, and as the slope per 0.1
change in weekly growth.

- H1: the cost of a doubled observation delay and of the degenerate level on each
  axis rises with the observed growth change (95% interval of the slope above zero),
  in each family.

*Secondary models, step trajectories.* Origins are grouped by time from the change
into before (14 to 2 days before), early (1 to 13 days after) and later (16 to 34
days after). The same terms each get a coefficient per window, pooled over the four
trajectories, with flat origins carrying a single constant per term as the baseline.
Pooling over step-up and step-down means a bias that helps in one direction counts
against it in the other.

- H2: before the change, the excess over flat for a doubled observation delay and for
  the degenerate level is within ±0.05 on the log-scale CRPS (95% interval inside that
  band).
- H3: the excess in the early window is larger than in both the before and the later
  window (95% intervals of the differences above zero).
- H4: the excess error in R at the origin follows the same pattern as H3.

Time from the change is not known to a forecaster, so H2 and H3 describe the design
rather than a forecasting situation. Direction-specific (down, up) and size-specific
(bend, turn) coefficients, a fit with one coefficient per origin time, and the same
models on the energy distance are descriptive. So is a disease-like fit with the
true change in R over the 14 days before the origin, the 28 days before those and
the 42 days after it, reported as not available in real time. Halving and the
doubled generation interval have no directional hypothesis and are reported.

*Interpretation.* If H1 holds, the paper's turning-point result is that the delay
choice matters once a change is visible in the reported data. If it holds only for
the step trajectories, it is stated for sharp changes only. H2 and H3 then say how
this looks around a known change. Failed and non-converged fits are reported by
trajectory and origin.

### Potential extensions (future work)

- Repeat the analysis varying factors we hold fixed here:
  - Case ascertainment / under-reporting (the observation scale).
  - The Rt projection method (carry the nowcast forward vs. extrapolate it).
  - The observation family or overdispersion treatment (the main analysis uses
    negative binomial with estimated overdispersion).
  - The standard deviation or shape of the delays (we vary only the mean).
  - Truncation, i.e. real-time forecasting from incomplete recent data.
- Enrich the observation model (day-of-week term, overdispersion) and check
  whether an estimated delay returns to a plausible shape, testing whether a free
  delay is absorbing model misspecification.

## Data sources

**SARS-CoV-2**

- European Forecast Hub — baseline + ensemble models already there. Case time
  series for 32 countries.

**Ebola**

- Using linelist from: [Transmission dynamics of Ebola virus disease and intervention effectiveness in Sierra Leone (PNAS)](https://www.pnas.org/doi/suppl/10.1073/pnas.1518587113)
- Lab-confirmed database: 8358 cases from May 2014 – Sep 2015 by age, district
  and chiefdom, with date of symptom onset and date sample was tested. Also
  suspected cases: 3545. Start with national → could look at district level
  later.
- Their primary analysis used confirmed cases only.

**Cholera**

- Yemen eDEWS weekly suspected cases, aggregated to national
  ([figshare](https://figshare.com/articles/dataset/Weekly_cholera_cases_and_rainfall_by_governorate_and_district_in_Yemen_between_2016_and_2020/24231019)),
  Apr 2017 – Jul 2018. These are suspected (not confirmed) cases at weekly
  resolution, which is the material caveat for cholera, so we do not read its
  generation interval as a biological quantity (see the provenance note above).

## References

::: {#refs}
:::
