# Analysis plan

*Research question:* What impact does misspecifying the generation interval and
incubation period have on the quality of infectious disease forecasts?

The question separates into two linked studies:

- **Q1 — Sensitivity (synthetic data, truth known).** How much does misspecifying
  the generation interval or the incubation period degrade short-term case
  forecasts, relative to the effect on Rt? A knob-turning experiment, and the
  well-specified control.
- **Q2 — Impose vs. learn (case studies, truth unknown).** Along a continuum from
  imposing the literature value to learning the delay freely from the data, where
  does forecast skill peak — i.e. how much expert anchoring is optimal? (Omitting
  the delay is the degenerate end of the same axis.)

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
5. **The impose↔learn axis is realised as a nonparametric Dirichlet delay with a
   swept `concentration` parameter** (requires the EpiNow2 dev build providing
   `Dirichlet()`; pin the commit). High concentration anchors the PMF to the
   literature shape (impose); low concentration lets the data determine it freely
   (learn). One object spans the continuum and exposes the estimated *shape*, not
   just the mean. A parametric data-driven estimate (gamma/lognormal,
   `weight_prior=FALSE`) is retained as a robustness comparison — does any finding
   depend on the free-form flexibility? The prior-anchored parametric variant
   (`weight_prior=TRUE`) is dropped: theory (Gostic et al. 2020) says added
   uncertainty widens intervals without shifting the central estimate.
6. **Weak identifiability is expected and grows as concentration falls.** Where the
   fit degenerates is itself the identifiability result: posterior shape,
   contraction and convergence diagnostics are reported, not used as silent
   filters.

## Methods

### Data

#### Simulated data

- Generate using `simulate_infections()`.
- SARS-CoV-2-like, Ebola-like, cholera-like (artificial Rt trajectory scenarios
  plus case studies).
- Forecasts every four weeks.
- [Estimating the time-varying reproduction number (Wellcome Open Research)](https://wellcomeopenresearch.org/articles/5-112) — used a 12-week rolling window.

| Parameter | Value | Distribution | Source |
|---|---|---|---|
| **SARS-CoV-2** | | | |
| Generation interval | 3.6 (s.d. 3.1) days | gamma | Sherratt et al. 2021 |
| Incubation period | 5.2 (s.d. 1.52) days | lognormal | Sherratt et al. 2021 |
| Reporting delay | 4.4 (s.d. 5.6) days | lognormal | Sherratt et al. 2021 |
| Under-reporting | 30% (s.d. 3.3%) | | Colman et al. 2023 |
| R trajectory | | | [epiforecasts.io/covid](https://epiforecasts.io/covid/) |
| Time horizon | Jun 2020 – Dec 2021 | | |
| Forecast frequency | 4 weeks | | |
| **Ebola** | | | |
| Generation interval | 16.2 (s.d. 9.4) days | gamma | Park et al. 2019 |
| Incubation period | 11.4 (s.d. 8.1) days | lognormal | Aylward et al. 2014 |
| Reporting delay | 4.95 (s.d. 1.79) | lognormal | Estimated from data: Fang et al. 2016 |
| Under-reporting | 83% (Scarpino et al. 2015); 27%–68% (Dalziel et al. 2018) | | |
| R trajectory | | | Funk, S. (2019), Zenodo, doi:10.5281/zenodo.254770 |
| Time horizon | Aug 2014 – Feb 2015 | | Funk et al. 2019 |
| Forecast frequency | 4 weeks | | |
| **Cholera** | | | |
| Generation interval | 8.51 (range 7.17–9.29) days; alternative: 5 (s.d. 8) days | gamma | Mari et al. 2012; alternative [Azman et al. 2018 (Lancet Global Health)](https://www.thelancet.com/journals/langlo/article/PIIS2214-109X(18)30230-4/fulltext) |
| Incubation period | 1.4 (s.d. 1.98) days | lognormal | Azman et al. 2013 |
| Reporting delay | 4.4 days (95% CI 3.2, 5.8) | lognormal | [Nishiura et al. 2017](https://tbiomed.biomedcentral.com/articles/10.1186/s12976-017-0061-x) |
| Under-reporting | 25% positivity (RDTs) and 25.9% positivity (culture) | | [Yemen figshare dataset](https://figshare.com/articles/dataset/Weekly_cholera_cases_and_rainfall_by_governorate_and_district_in_Yemen_between_2016_and_2020/24231019) |
| R trajectory | | | |
| Time horizon | | | |
| Forecast frequency | | | |

*[Need to fill in gaps in the table above.]*

The observation delay is modelled as the incubation period alone; the
reporting-delay rows are retained for reference but are not used as a separate
distribution (see Design decisions).

#### Rt trajectory scenarios

Artificial scenarios (might need reducing):

1. Constant Rt (0.8, 1.2)
2. Increasing Rt (0.5 → 1.5)
3. Decreasing Rt (1.5 → 0.5)
4. Sinusoidal around 1 (0.5–1.5)
5. Piecewise Rt (had looked at this before to base the change on e.g. lockdown;
   school closures).

Currently implemented (four trajectories): constant 0.8; constant 1.2;
increasing 0.8 → 1.2; decreasing 1.2 → 0.8.

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
value, fixed) and **learn** (nonparametric Dirichlet PMF with `concentration`
swept over a small set of values, from strongly anchored to free-form). The
concentration sweep traces the impose→learn continuum within one model; a
parametric data-driven estimate (`weight_prior=FALSE`) is run alongside as a
robustness comparison. In addition, impose the full fixed misspecification grid
(as above) on the real series to see which fixed value forecasts best
out-of-sample.

Depth: the full ladder for the generation interval (the central object);
impose/omit/learn for the incubation period across all three diseases (for
cholera the posterior is expected to be uninformative — that is the result, not a
reason to skip it); and the joint generation-interval-plus-incubation-period
learn as an identifiability stress-test.

Outputs: log and natural CRPS as a function of concentration (where does skill
peak?); the estimated PMF shape against the literature distribution (is a
better-forecasting delay biologically plausible?); posterior contraction and
convergence diagnostics.

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

- Compare CRPS (on both natural and log scales) ranking for Rt estimates vs.
  one-week and two-week forecasts (may need more time horizons):
  - By timepoint.
  - Overall for each Rt trajectory scenario.
- Also consider over/under-prediction and dispersion separately.

Notes:

- CRPS → both sharpness and calibration.
- Median Absolute Deviation (MAD) → sharpness.
- Interval coverage → calibration.
- Specifically comparing: slightly mis-specified (low/high) vs. very
  mis-specified (very low/very high) vs. no delay. I.e. is a slightly bad
  estimate for the incubation period still better than assuming no delay; is a
  very bad estimate for the incubation period better than assuming no delay?
  Likewise for the generation interval.

### Potential extensions (not planned, noted for future reference)

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

- <https://cholera-taxonomy.middle-distance.com/observation_collections/20742>
  → contains confirmed cases: culture + RDT. Observation frequency = daily.
