# Analysis plan

*Research question:* What impact does misspecifying the generation interval and
incubation period have on the quality of infectious disease forecasts?

The question separates into two linked studies:

- **Q1 — Sensitivity (synthetic data, truth known).** How much does misspecifying
  the generation interval or the incubation period degrade short-term case
  forecasts, relative to the effect on Rt? A knob-turning experiment, and the
  well-specified control.
- **Q2 — Impose vs. learn (case studies, truth unknown).** Along the continuum
  from imposing the literature value to letting the data set the delay freely,
  where is forecast skill best? That measures how much the literature value should
  constrain the fit. (Omitting the delay is the degenerate end of the same axis.)

The underlying interest is the trade-off between expert-imposed ("human") and
data-driven ("machine") specification of the model's mechanistic delay
parameters.

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
   depend on the free-form flexibility? We drop the prior-weighted parametric
   variant (`weight_prior=TRUE`), because Gostic et al. (2020) show that added
   uncertainty widens the intervals without shifting the central estimate.
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

Values below are the literature central values used in
`scripts/02b_definedelays.R` (the `delays` list). We model the observation delay
as the incubation period alone.

| Parameter | Value | Distribution | Source |
|---|---|---|---|
| **SARS-CoV-2** | | | |
| Generation interval | 3.6 (s.d. 3.1) days | gamma | Sherratt et al. 2021 |
| Incubation period | 5.2 (s.d. 1.52) days | lognormal | Sherratt et al. 2021 |
| Case ascertainment | 0.40 | | Colman et al. 2023 reports ~0.30; value to confirm |
| R trajectory / horizon | Jun 2020 – Dec 2021 | | [epiforecasts.io/covid](https://epiforecasts.io/covid/) |
| **Ebola** | | | |
| Generation interval | 16.2 (s.d. 9.4) days | gamma | Park et al. 2019 |
| Incubation period | 9.7 (s.d. 5.5) days | gamma | WHO Ebola Response Team 2014 |
| Case ascertainment | 0.83 | | Scarpino et al. 2015 |
| R trajectory / horizon | Aug 2014 – Feb 2015 | | Funk 2019, Zenodo doi:10.5281/zenodo.254770 |
| **Cholera** | | | |
| Generation interval | 5 (s.d. 4) days | gamma | [Azman et al. 2018](https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(18)30230-4/fulltext) |
| Incubation period | 1.77 (s.d. 1.36) days | lognormal | Azman et al. 2013 |
| Case ascertainment | 0.28 | | basis undocumented; to confirm |
| R trajectory / horizon | Apr 2017 – Jul 2018 | | |

#### Rt trajectory scenarios

We simulate four Rt trajectories: constant below 1 (0.8); constant above 1
(1.2); increasing (0.8 → 1.2); and decreasing (1.2 → 0.8).

Then for each of those:

- No under-reporting, some under-reporting (based on literature for each
  disease).
- `rt_opts` default vs. project.

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

For each of the generation interval and the incubation period, forecast along a
three-rung ladder: **omit** (a single-day distribution for the generation
interval; a zero-day delay for the incubation period), **impose** (literature
value, fixed) and **learn** (nonparametric Dirichlet PMF centred on the
literature shape, with `concentration` swept over `{1, 5, 20, 50, 200}` — from
fully free to strongly held). By sweeping the concentration we cover the
impose→learn continuum in one model; a parametric data-driven estimate
(`weight_prior=FALSE`) is run
alongside as a robustness check, with `Normal` priors on the delay's natural
parameters centred on the literature value. The prior width is the parametric
analogue of the concentration knob and faces the same tension (too tight and the
data cannot move it; too wide and it will not sample), so we set it from the
identifiability diagnostic: the loosest relative SD that recovers a known delay
and still converges (expected ~10–20%, given case counts weakly identify delays).
In addition, impose the fixed misspecification grid on the real series to see
which fixed value forecasts best out-of-sample.

We take weekly forecast origins so the score model (see Evaluation) has many
forecast-target rows to work with; it accounts for the temporal structure. Weekly
origins also let us stratify learn-arm behaviour by whether the fitting window
contains a turning point (delays are identifiable only when the growth rate
changes; see the identifiability diagnostic).

Depth: the full ladder for the generation interval (the central object);
impose/omit/learn for the incubation period on Ebola and COVID; and the joint
generation-interval-plus-incubation-period learn as an identifiability
stress-test. We drop cholera IP: at ~1.4 days on weekly data it is unidentifiable
and adds nothing.

We feed the score model with log and natural CRPS by condition; the estimated PMF
shape and posterior contraction against the literature value; and convergence
diagnostics (Rhat, ESS, divergences, tree-depth) reported as results.

#### Identifiability diagnostic (gating step, run first)

Delay identifiability from case counts requires the growth rate to change: a
constant Rt gives an exponential curve on which a delay and the reproduction
number trade off exactly, so nothing is recoverable. Before trusting the learn
arm on real data, simulate a **known** generation time under each Rt trajectory
(the constant trajectories as negative controls that should *not* recover;
increasing/decreasing as the informative turning-point cases), sweep
`concentration`, and record for each (trajectory × concentration) whether the
posterior GT PMF contracts onto the truth, with sampling diagnostics.

Recovery metric: posterior mean GT (and PMF) vs. the true value, alongside ESS,
divergences and tree-depth saturation.

Gate (pre-registered): if the posterior recovers the truth only at near-imposed
concentration, the learn arm is reported as prior-dominated and Q2 is reframed as
bounded ("the data cannot recover the delay; expert values are irreplaceable
here"). A pilot fit already shows the fully-free end (`concentration = 1`) is
pathological — tree-depth saturated, bulk ESS ≈ 1, mean roughly recovered but
shape not — so the sweep is expected to be usable only above some concentration,
which is itself the identifiability result.

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
  resolution. The learn ladder is still run for cholera (the posterior simply
  reflects uninformative data).

### Evaluation

Score forecasts on both the natural and log scales (log via `log(x + 1)` on
observations and predictions, following the eval-by-method convention), at the
one- and two-week horizons, using CRPS from `scoringutils`. Rt is scored on the
synthetic data only (Q1); the case studies have no ground-truth Rt, so only case
forecasts are scored there. Decompose CRPS into over-prediction, under-prediction
and dispersion.

**Primary comparison — a score model.** Following Sherratt et al.
(eval-by-method), fit a generalised additive (mixed) model to the CRPS with
`mgcv::bam(family = gaussian(link = "log"))`: the **condition** (in Q1, the
misspecification level as a smooth) is the effect of interest, with disease,
forecast horizon, a time smooth by disease and (Q1) Rt trajectory as
covariates/random effects. Report the condition **marginal effects with
confidence intervals** — no significance testing. Overlapping intervals are a
first-class result ("no clear difference"), which is what makes the write-up
robust to inconclusive outcomes.

**Pre-registered outcome mapping** (so any result is writable without new
choices):

- *Learn's interval below impose in ≥2 diseases* → "learning improves forecasts."
- *Intervals overlap* → "no clear difference between imposing and learning."
- *Concentration effect flat or non-monotonic* → "forecast skill is insensitive
  to the impose–learn choice."
- *Identifiability gate fails* (recovery only near-imposed) → "the data cannot
  recover the delay; the learn arm is prior-dominated," and Q2 is reframed.
- *Q1 CRPS flat across the grid* → "short-term case forecasts are insensitive to
  delay misspecification," with the signal appearing in Rt bias while CRPS stays
  flat; we treat this as a finding.

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

- Repeat analysis with additional parameters that are varied:
  - Under-reporting ("scale").
  - Observation family and, if negative binomial, overdispersion of reporting
    ("rep_phi").
  - Truncation (would require more work/thinking).

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
