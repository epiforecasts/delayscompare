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
5. **We build the impose↔learn axis as a nonparametric Dirichlet delay with a
   swept `concentration` parameter** (needs a recent EpiNow2 that provides
   `Dirichlet()`). High concentration holds the PMF close to the
   literature shape (impose); low concentration lets the data set it freely
   (learn). With one object we cover the whole continuum and read off the
   estimated *shape* as well as the mean. We keep a parametric data-driven estimate
   (gamma/lognormal, `weight_prior=FALSE`) as a robustness check: does any finding
   depend on the free-form flexibility? We drop the prior-weighted variant
   (`weight_prior=TRUE`): weighting the prior by the data keeps the estimate close
   to the imposed value, so it is redundant with the impose condition and out of
   scope for a learn arm.
6. **Weak identifiability is expected, and grows as concentration falls.** Where
   the fit degenerates is itself the identifiability result, so we report
   posterior shape, contraction and convergence diagnostics rather than quietly
   filtering those fits out.
7. **MCMC: EpiNow2 defaults**, raising `adapt_delta` on divergences and
   `max_treedepth` on tree-depth saturation (both seen in the learn arm). In the
   *impose* conditions, fits with max Rhat > 1.05 are discarded; in the *learn*
   conditions we report non-convergence instead of discarding it.

## Methods

### Data

#### Simulated data

- Generate using `simulate_infections()`.
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
| R trajectory / horizon | Jun 2020 – Dec 2021 | | [epiforecasts.io/covid](https://epiforecasts.io/covid/) |
| **Ebola** | | | |
| Generation interval | 16.2 (s.d. 9.4) days | gamma | @Park2019 |
| Incubation period | 9.7 (s.d. 5.5) days | gamma | @WHO_Ebola2014 |
| Case ascertainment | 0.83 | | @Scarpino2015 |
| R trajectory / horizon | Aug 2014 – Feb 2015 | | @Funk2019 |
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
three conditions: **omit** (a single-day distribution for the generation
interval; a zero-day delay for the incubation period), **impose** (literature
value, fixed) and **learn** (nonparametric Dirichlet PMF centred on the
literature shape, with `concentration` swept over `{1, 5, 20, 50, 200}` — from
fully free to strongly held). By sweeping the concentration we cover the
impose→learn continuum in one model; a parametric data-driven estimate
(`weight_prior=FALSE`) is run
alongside as a robustness check, with `Normal` priors on the delay's natural
parameters centred on the literature value. The prior width plays the same role
as the concentration and faces the same tension (too tight and the
data cannot move it; too wide and it will not sample), so we set it from the
identifiability diagnostic: the loosest relative SD that recovers a known delay
and still converges (expected ~10–20%, given case counts weakly identify delays).

We take weekly forecast origins so the score model (see Evaluation) has many
forecast-target rows to work with; it accounts for the temporal structure. Weekly
origins also let us stratify learn-arm behaviour by whether the fitting window
contains a turning point (delays are identifiable only when the growth rate
changes; see the identifiability diagnostic).

Depth: all conditions for the generation interval (the central object); and
impose/omit/learn for the incubation period on Ebola and COVID, each delay
estimated on its own with the other held fixed. We do not learn both delays
jointly — that is degenerate from case counts. We drop cholera IP: at ~1.4 days
on weekly data it is unidentifiable and adds nothing.

We feed the score model with log and natural CRPS by condition; the estimated PMF
shape and posterior contraction against the literature value; and convergence
diagnostics (Rhat, ESS, divergences, tree-depth) reported as results.

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

- **Ebola** — lab-confirmed cases by **date of symptom onset** (canonical
  pipeline `scripts/02_dataprocessing.R`): observation delay = incubation period,
  exactly. (The `datacollect_casestudy.R` variant dating by sample-tested is
  stale and should be removed.)
- **COVID-19** — UKHSA `COVID-19_cases_casesByDay`, dated by **specimen date**:
  observation delay = incubation period + an unquantified onset→specimen gap,
  absorbed into the approximation. Final snapshot; retrospective.
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
(`mgcv::bam`, identity link) — modelling the log score rather than putting a log
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
observations (e.g. an implausible estimated PMF shape that nonetheless forecasts
well) are labelled exploratory.

Notes:

- CRPS → both sharpness and calibration.
- Median Absolute Deviation (MAD) → sharpness.
- Interval coverage → calibration.
- Specifically comparing: slightly mis-specified (low/high) vs. very
  mis-specified (very low/very high) vs. no delay. I.e. is a slightly bad
  estimate for the incubation period still better than assuming no delay; is a
  very bad estimate for the incubation period better than assuming no delay?
  Likewise for the generation interval.

### Potential extensions (future work)

- Repeat the analysis varying factors we hold fixed here:
  - Case ascertainment / under-reporting (the observation scale).
  - The Rt projection method (carry the nowcast forward vs. extrapolate it).
  - The observation family or overdispersion treatment (the main analysis uses
    negative binomial with estimated overdispersion).
  - The standard deviation or shape of the delays (we vary only the mean).
  - Truncation, i.e. real-time forecasting from incomplete recent data.
- Enrich the observation model (day-of-week term, overdispersion) and check
  whether an estimated delay returns to a plausible shape — a test of whether a
  free delay is absorbing model misspecification.

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
