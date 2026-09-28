# Delay misspecification degrades situational awareness during periods of transmission change

## Abstract

Renewal-equation models estimate the time-varying reproduction number and forecast cases using two inputs that are rarely known for the outbreak at hand: the generation interval and the delay from infection to observation. Both are usually imported from other settings and may be mismatched. We asked when that mismatch actually matters, for reproduction-number estimation and for short- to medium-term case forecasts, using controlled synthetic experiments, re-simulated disease-like trajectories, and three case studies (Sierra Leone Ebola 2014--16, England COVID-19 Delta 2021, Yemen cholera 2017--21). Misspecifying either delay by up to a factor of two had little effect while transmission was steady and near-exponential: the model absorbs the wrong delay into a rescaled reproduction number and seeding, leaving estimates and forecasts almost unchanged. The penalty appeared only where the reproduction number was changing over the relevant window, and grew with how much it changed, with no special role for the reproduction number crossing one. This held for both delays and both outcomes, with one mechanical exception: the generation interval sets the reproduction-number scale through the Euler--Lotka relation, so it biases the level of estimated R even under steady growth. At turning points the two delays failed differently. The observation delay dominated forecast error, because mis-timing a peak is costly, while the generation interval dominated reproduction-number error. Because short forecast windows are close to exponential, short-term forecasts were largely insensitive to delay choice and a detectable effect emerged only at three-to-six-week horizons. In the case studies, imposing a literature generation interval improved forecasts when it was drawn from the same outbreak (Ebola), was neutral when pooled across outbreaks (cholera), and degraded them by roughly a factor of two when taken from earlier waves (COVID-19 Delta); the Delta harm fell on the wave's turning points, and the sensitivity of the forecast to the delay choice widened at those turning points and collapsed in between. Delay assumptions matter most when the epidemic is turning: in steady phases the choice barely moves reproduction-number estimates or forecasts, while a poorly matched value is most damaging exactly when transmission is changing.

## Introduction

Infectious-disease forecasting draws on a range of modelling approaches with different information requirements. At one end are purely statistical time-series models that make no assumption about the underlying transmission process; at the other are fully mechanistic compartmental models that require detailed parameterisation of the natural history and contact structure [@Banholzer2023]. Semi-mechanistic models based on the renewal equation occupy the middle ground: they use the generation interval distribution, together with delay distributions linking infection to observation, to estimate the time-varying reproduction number Rt and project incidence forward from case data [@Cori2013; @Abbott2020; @Gostic2020].

The generation interval is the time between infection of an individual and infection of those that individual subsequently infects. Together with the time-varying reproduction number it determines how observed case counts at any given time relate to the chain of preceding infections. It is rarely observed directly because infection times are unknown; it is therefore commonly inferred from the serial interval, either by assumption or by explicit deconvolution [@Ganyani2020; @Ferretti2020; @Lehtinen2021]. The observed serial interval can in turn depend on whether it is estimated forward or backward in calendar time, and on the timing of observation within an outbreak, distinguishing the "intrinsic" from the "realised" generation interval [@Svensson2007; @Nishiura2010; @Champredon2015]. Identifying transmission pairs at all requires contact-tracing data that may be sparse or biased toward household contacts.

Previous work has shown that misspecifying the generation interval biases Rt estimates in a predictable way [@Gostic2020]. Less is known about the downstream consequences for short-term forecasts of cases, which is the quantity most often used operationally, or about the observation delay, which enters the same models but has received little attention as a source of forecast error. More fundamentally, it is unclear *when* getting these delays wrong actually matters. Practitioners often assume delays matter mainly around epidemic turning points, but this intuition has not been demonstrated. It is also unclear whether estimating the generation interval jointly with Rt, rather than imposing a literature value, improves forecasts in practice, given that the data are usually only weakly informative about the generation interval on the timescales available for nowcasting.

Here we ask when misspecifying either delay, the generation interval or the infection-to-observation delay, degrades reproduction-number estimation and case forecasting, and we show that the answer is: only when transmission is changing. We first deliberately misspecify both delays on synthetic case time series with known distributions, using trajectories designed to separate steady transmission from turning points, and score both the estimated reproduction number and forecasts out to six weeks. We then apply the framework to case study data from three outbreaks (Sierra Leone Ebola 2014--16, England COVID-19 Delta 2021 and Yemen cholera 2017--21), where the true delays are unknown: we compare forecasts under three generation interval strategies (omitting it, equivalent to a one-day distribution; imposing the literature value; and estimating it from the data with the literature value as the prior mean), and we track how sensitive the forecast is to the delay choice over the course of each outbreak. A mismatched delay is nearly free while an epidemic grows or declines steadily and becomes costly only where the reproduction number turns.

## Methods

### Synthetic data

We generated synthetic case time series for three diseases with varying generation intervals, incubation periods and reporting delays. The values chosen were based on Ebola, COVID-19 and cholera (Table 1).

**Table 1.** Parameter values used to simulate the synthetic case time series and as the literature values throughout the case study forecasting runs. Values in parentheses are the SD of the normal prior placed on each parameter (mean and SD treated independently) when the generation interval is estimated. Case ascertainment is held fixed at the value shown.

| Disease | Parameter | Mean (SD of prior) | SD (SD of prior) | Distribution | Source |
|---|---|---|---|---|---|
| Ebola-like | Generation interval | 16.2 (1.62) | 9.4 (0.94) | gamma | @Park2019 |
| Ebola-like | Incubation period | 9.7 (0.97) | 5.5 (0.55) | gamma | @WHO_Ebola2014 |
| Ebola-like | Reporting delay | – | – | – | Date of symptom onset used; no reporting delay applied |
| Ebola-like | Case ascertainment rate | 0.83 | – | – | – |
| COVID-19-like | Generation interval | 3.6 (0.70) | 3.1 (0.80) | gamma | @Sherratt2021 |
| COVID-19-like | Incubation period | 5.2 (1.10) | 1.52 (1.10) | lognormal | @Sherratt2021 |
| COVID-19-like | Reporting delay | 4.4 (0.44) | 5.6 (0.56) | lognormal | @Sherratt2021 |
| COVID-19-like | Case ascertainment rate | 0.40 | – | – | – |
| Cholera-like | Generation interval | 5.0 (0.5) | 4.0 (0.4) | gamma | @Azman2018 |
| Cholera-like | Incubation period | 1.77 (0.18) | 1.36 (0.14) | lognormal | @Azman2013 |
| Cholera-like | Reporting delay | 4.4 (0.66) | 4.4 (0.66) | gamma (exponential) | @Nishiura2017 |
| Cholera-like | Case ascertainment rate | 0.28 | – | – | – |

For each example disease, we simulated infections using the renewal equation for four idealised reproduction-number trajectories: constant below one (0.8); constant above one (1.2); increasing (from 0.8 to 1.2); and decreasing (from 1.2 to 0.8). To complement these with realistic dynamics, we also generated disease-like trajectories: we fit the renewal equation model to each pathogen's case-study series (below), took the posterior median reproduction-number trajectory, and re-simulated case data from it. These retain the peaks, plateaus and turnarounds of a real outbreak while keeping the generating delays known by construction. Each disease-like trajectory was simulated as 20 independent observation-noise replicates so that per-origin differences could be resolved against sampling noise.

To isolate the epidemic conditions under which delay misspecification matters, we generated a third family of controlled turning-point trajectories over a fixed 210-day window with a single change in the reproduction number at day 105: *flat* (reproduction number constant at 1.1), *bend* (a step from 1.3 to 1.0: transmission changes but the reproduction number never crosses one and incidence never peaks), and *turn* (a step from 1.3 to 0.7, crossing one at an incidence peak). The *bend* and *turn* trajectories separate a change in transmission from the reproduction number crossing one.

### Delay misspecification on synthetic data

For each synthetic time series, we generated forecasts using the *EpiNow2* implementation of the semi-mechanistic renewal equation model [@Abbott2020]. We misspecified each of the two delay inputs in turn. For the generation interval, we varied the assumed mean across six values: a one-day distribution (equivalent to omitting the generation interval), and 0.25, 0.8, 1, 1.25 and 2 times the correct mean, holding the observation delay at its correct value. For the observation delay (incubation period and, where applicable, reporting delay), we applied the same six multipliers while holding the generation interval correct. We refit at successive forecast origins using up to 12 weeks of preceding data, and at each origin we scored both the estimated reproduction number and the forecast case counts.

For the idealised trajectories we scored forecasts at one- and two-week horizons, as in the case studies. On the disease-like trajectories we extended the forecast horizon to six weeks and scored each week, to establish at what horizon delay misspecification becomes detectable.

### Turning-point experiment

On the controlled turning-point trajectories (*flat*, *bend*, *turn*), we placed forecast origins at roughly two-week intervals across the 210-day window and ran the same two misspecification sweeps at each. For every origin we computed the change in the true reproduction number over the relevant window (the forecast window for the forecast score, the estimation window for the reproduction-number score), so that the per-origin error could be related directly to how much transmission was changing there. We summarised each origin by the slope of its error against the logarithm of the delay multiplier (dropping the degenerate one-day level), a single number capturing how strongly delay misspecification degraded that origin.

### Case studies

#### Ebola: Sierra Leone, 2014–2016

We used laboratory-confirmed daily Ebola case counts from Sierra Leone, at a national level, over the period May 2014 to September 2015 (<https://www.pnas.org/doi/suppl/10.1073/pnas.1518587113>). We assumed a case count of 0 on days when data was missing.

#### COVID-19: UK, 2021

We used daily COVID-19 case counts in England over the period of the Delta wave (June 2021–December 2021), downloaded from the UKHSA COVID-19 in the UK dashboard (<https://ukhsa-dashboard.data.gov.uk/covid-19-archive-data-download>). The series combines cases reported via Pillar 1 and Pillar 2 testing.

#### Cholera: Yemen 2016–2020

We used weekly cholera case counts from April 2017 to June 2021, extracted from the cholera dashboard developed by the WHO-led Emergency Operations Centre (<https://figshare.com/articles/dataset/Weekly_cholera_cases_and_rainfall_by_governorate_and_district_in_Yemen_between_2016_and_2020/24231019?file=42635185>). The series reports suspected cases at governorate and district level, aggregated here to the national weekly total.

### Forecasting model and conditions

We fit a renewal equation model to each case study using the *EpiNow2* implementation [@Abbott2020]. The model takes a generation interval distribution, an incubation period distribution, a reporting delay distribution, and the case time series, and infers a smoothly varying time-dependent reproduction number with a Gaussian process prior; forecasts are produced by projecting that reproduction number forward and convolving inferred infection times forward through the delay distributions to give expected observed cases.

We ran each case study under three conditions, varying the generation interval specification (Table 2). The incubation period and the reporting delay were held at their literature values in all three conditions, so that the variation in forecast performance we measure is attributable to the choice made for the generation interval. For cholera we used a generation interval of mean 5 days and SD 4 days [@Azman2018].

**Table 2.** Three generation interval conditions, with the incubation period and the reporting delay held at their literature values throughout.

| Label | Generation interval |
|---|---|
| **Default** | not supplied (single-day distribution) |
| **Literature** | literature value, fixed at the values in Table 1 |
| **Estimated** | estimated with the literature value as the central value of the prior |

In the *Default* condition the renewal equation effectively absorbs the inferred case-to-case multiplier (the reproduction number) into a generation that lasts a single day. With the incubation period and the reporting delay fixed but the generation interval absent, the model behaves as a Gaussian-process-smoothed Rt forecast applied directly to case counts after they have been deconvolved through the delays.

Because the true delays are unknown in the case studies, we cannot measure a penalty against a correct value. Instead, to see whether the delay choice matters more at some points in an outbreak than others, we also fit the full six-by-six grid of generation-interval and observation-delay multipliers (the same grid as the synthetic experiments) at each forecast origin, and summarised each origin by the spread of forecast CRPS across that grid. A wide spread means the delay choice changes the forecast substantially at that origin; a narrow spread means it is largely irrelevant. We tracked this spread over the course of each outbreak against the case time series.

To test more directly whether estimating the generation interval helps, and whether its posterior actually moves away from the literature prior, we re-ran the *Literature* and *Estimated* conditions at weekly forecast origins (rather than the roughly monthly spacing used elsewhere), matched by origin, and additionally extracted the posterior distribution of the estimated generation-time mean at each origin.

### Evaluation

We compared performance using the continuous ranked probability score (CRPS) on log-transformed reproduction-number and case estimates, and decomposed the case CRPS into underprediction, overprediction and dispersion components. For the case studies and idealised trajectories we scored one- and two-week horizons; on the disease-like trajectories we scored weekly out to six weeks. We excluded any fit for which the Markov chains had not mixed, as identified by the standard $\hat{R}$ statistic [@Gelman1992; @Vehtari2021]; specifically we discarded any fit with $\max \hat{R} > 1.05$.

## Results

### Delay misspecification is absorbed while transmission is steady

Misspecifying the generation interval biased the inferred reproduction number towards or away from one as predicted by the underlying renewal equation geometry [@Gostic2020]: a shorter assumed mean pulled the inferred reproduction number towards one and a longer mean pushed it away (full results in the Supplementary Material). The size of the bias was determined by the magnitude of the misspecification. Misspecifying the observation delay, by contrast, left the reproduction number almost unchanged: under steady growth a wrong observation delay is absorbed into the inferred seeding and level rather than the reproduction number.

The corresponding effect on short-term case forecasts was small for both delays. Across the six generation interval values from a quarter to twice the correct mean, the spread in mean log-CRPS at the second and later forecast horizons was less than 20% of the median for any disease; misspecifying the observation delay over the same range had a smaller effect still. The exception was setting the generation interval to one day, equivalent to omitting it, which gave noticeably worse forecasts than any other value in the COVID-19-like scenarios and at the first cholera-like timepoint. Moderate delay misspecification barely moved the forecast; ignoring the generation interval altogether was the only consequential error available in this grid.

This small average, however, is taken across trajectories that are mostly steady. Underprediction dominated the case CRPS decomposition when the true reproduction number was increasing and overprediction dominated when it was decreasing, while during constant-reproduction-number phases the delay choice barely registered. Dispersion contributed most at the earliest forecast timepoints in all scenarios and shrank as the time series lengthened. These patterns were consistent across the three disease scenarios and motivate separating steady from changing transmission, which we do below.

We then ran each synthetic time series under the three generation-interval conditions of Table 2 (Table 3). With the literature generation interval by construction equal to the data-generating distribution, both the literature and the estimated conditions gave lower CRPS than the default in every disease scenario. Estimating the generation interval gave a marginal further improvement over imposing the literature value (0.05 vs 0.08 for COVID-19-like, 0.08 vs 0.16 for Ebola-like, 0.11 vs 0.12 for cholera-like).

**Table 3.** Mean log-CRPS for the synthetic data scenarios under the three conditions of Table 2, averaged across the four reproduction number trajectories. The lowest CRPS in each row is in bold. The incubation period and the reporting delay were held at their literature (correct) values in all three conditions.

| Disease | Default | Literature | Estimated |
|---|---|---|---|
| Ebola-like   | 0.18 | 0.16 | **0.08** |
| COVID-19-like | 0.12 | 0.08 | **0.05** |
| Cholera-like | 0.20 | 0.12 | **0.11** |

### Delay misspecification degrades estimates and forecasts where transmission changes

The controlled turning-point trajectories show that the small average effect above is concentrated where the reproduction number changes. On the *flat* trajectory, misspecifying either delay had almost no effect on either the estimated reproduction number or the forecast at any origin. On the *bend* and *turn* trajectories the per-origin error rose sharply (Table 4, Figure 1). Because the *bend* trajectory changes the reproduction number without ever crossing one, the effect cannot depend on the reproduction number crossing one; it tracks how much transmission is changing. The *turn* trajectory, with a larger change, gave a slightly larger effect again, and the *bend* effect was already close to it for forecasts: the penalty scales with the size of the change rather than switching on at a threshold.

**Table 4.** Mean absolute per-origin error slope (change in log-CRPS per unit change in log delay multiplier) on the turning-point trajectories, by outcome and delay axis. A value near zero means the delay choice does not affect that origin.

| Outcome and axis | flat | bend | turn |
|---|---|---|---|
| Forecast, observation delay | 0.013 | 0.103 | 0.142 |
| Forecast, generation interval | 0.004 | 0.028 | 0.033 |
| Reproduction number, observation delay | 0.002 | 0.013 | 0.018 |
| Reproduction number, generation interval | 0.019 | 0.045 | 0.059 |

The one departure from a clean zero on *flat* is the generation interval's effect on the reproduction number (0.019): because the generation interval sets the reproduction-number scale through the Euler--Lotka relation, a wrong generation interval biases the level of the estimated reproduction number even under steady growth, on top of the turning-point effect. The observation delay, which has no such mechanical role, was flat-zero for both outcomes. The two delays also failed differently at the turns: the observation delay dominated forecast error, because mis-timing a peak is costly, while the generation interval dominated reproduction-number error.

The disease-like trajectories reproduced the same pattern under realistic dynamics. Stratifying each forecast origin by the change in the true reproduction number over its forecast window, the mean absolute generation-interval effect at six weeks rose monotonically from 0.002 where the reproduction number was near-constant, through 0.020, to 0.099 where it changed most; cholera, whose fitted trajectory barely moves over any six-week window, sat at the bottom throughout. The effect also emerged only with horizon: negligible at one to two weeks, it grew to roughly 0.07--0.10 log-CRPS by six weeks, because only over a longer window does a forecast span enough curvature for the delay to matter.

We applied the same three conditions to each case study (Table 5). For Ebola in Sierra Leone, imposing the literature generation interval improved on the default forecast (CRPS 0.32 vs 0.43), and estimating the generation interval gave the same CRPS as imposing it. The data and the literature value agreed sufficiently that the estimated posterior did not move far from the prior.

For COVID-19 Delta in England, imposing the literature generation interval gave a CRPS substantially worse than the default (0.73 vs 0.39). Estimating the generation interval from the data, with the literature value as the central value of the prior, partially recovered ground (0.67) but did not reach the default. The literature SARS-CoV-2 generation interval was estimated from data collected in earlier waves of the same pathogen, and our results suggest it does not transfer to the Delta wave.

For cholera in Yemen, imposing the literature generation interval gave the best forecast (CRPS 0.16). The default condition was slightly worse (0.22), and estimating the generation interval from the data was further from the imposed value (0.27); the added posterior uncertainty about the generation interval degraded the forecasts.

**Table 5.** Mean log-CRPS for each case study under the three conditions of Table 2. The incubation period and the reporting delay were held at their literature values in all three conditions.

| Disease | Default | Literature | Estimated |
|---|---|---|---|
| Ebola, Sierra Leone 2014--16    | 0.43 | **0.32** | 0.32 |
| COVID-19, England Delta 2021    | **0.39** | 0.73 | 0.67 |
| Cholera, Yemen 2017--21         | 0.22 | **0.16** | 0.27 |

The three case studies therefore occupy three distinct positions relative to the default. For Ebola, where the literature generation interval is derived from the same outbreak as the case study data [@Park2019], imposing it improves on the default. For Yemen cholera, where the value is pooled across cholera outbreaks in other settings [@Azman2018], imposing it gives a marginal improvement. For COVID-19 Delta, where the value is taken from earlier waves of the same pathogen [@Sherratt2021], imposing it almost doubles the CRPS.

The COVID-19 result is the case study with the most transmission change over the forecast period, and the over-time analysis shows the two are linked. Tracking the spread of forecast CRPS across the full delay grid over the Delta wave (Figure 2), the delay choice barely affected the forecast while transmission was steady but mattered substantially at the wave's turning points: the spread widened at the July peak and again at the autumn rise, and collapsed in the intervening steady trough. The origin where imposing the mismatched literature generation interval did most damage coincided with the July peak. Because the true generation interval is unknown here, this is sensitivity to the delay choice rather than a penalty against a known value, but it places the case-study harm in the same regime as the synthetic result: a mismatched delay is costly exactly where transmission turns.

### Estimating the generation interval moves the posterior but not the forecast

The three-condition comparison above rests on 6-8 forecast origins per case study, too few to separate a genuine shift in the estimated generation interval from noise around the literature prior. At weekly origins, matched between the *Literature* and *Estimated* conditions (25-30 converged origins for Ebola and cholera; 7-8 for COVID-19, discussed below), the posterior generation-time mean moved substantially and consistently away from the literature value, not just noisily around it (Figure 3). For Ebola it sat 25-35% below the literature value (10-13 days against 16.2) at essentially every origin across the full six-month outbreak. For COVID-19 it fell from around 4.4 days in early June to 3.0-3.9 days by late July, crossing the literature value of 3.6 days on the way. For cholera it rose from around 4.7 days to a stable plateau of 5.9-6.2 days from August onward. None of this movement was concentrated at turning points; it was present across steady and changing phases alike, unlike the observation delay (§Delay misspecification is absorbed while transmission is steady).

Despite that movement, forecast CRPS under the *Estimated* condition was visually indistinguishable from the *Literature* condition at almost every origin (Figure 4), including the Ebola origins where the posterior sat furthest from the literature value. To put a size and an interval on that impression, we fitted a GAMM to the paired origins with condition (estimated vs literature) as a categorical fixed effect interacting with disease, and an origin-level random effect absorbing the shared difficulty at each origin, which lets the condition contrast be estimated within origin. For Ebola and COVID-19 the estimated condition effect was a genuine null, with a confidence interval symmetric around zero (Ebola: -0.0001, 95% CI -0.039 to 0.039; COVID-19: -0.008, 95% CI -0.087 to 0.071). For cholera the interval excluded zero (0.044, 95% CI 0.001 to 0.086): estimating was measurably, if modestly, worse. This is the real-data counterpart of the near-zero `Forecast, generation interval` row of Table 4: the generation interval barely moves case-forecast CRPS anywhere, so a posterior that has moved substantially still forecasts about the same, and where it does move the needle it is toward harm, not benefit. A second model related the size of the CRPS gap for cholera to how far the posterior had moved from the literature value, and found the harm was largest when the posterior sat *close* to the literature value and faded as the gap grew; because a near-zero gap coincided with the earliest, shortest-window origins (around 22 days of data), this reflects too little data to constrain the wider prior rather than a cost of learning itself.

For COVID-19, weekly origins from 10 August 2021 onward failed to converge within a 24-hour compute budget. This comparison covers only the earlier, steadier part of the Delta wave; the fitting window first spans the July peak at almost exactly that point. We report this as a finding rather than a limitation worked around: estimating the generation interval under a wide prior can become computationally intractable, not just less informative, once the fitting window includes a turning point, mirroring the identifiability difficulty already documented for the observation delay.

### Learning both delays jointly is harder than learning either alone

We extended the weekly comparison to two further conditions: estimating the observation delay alone (generation interval imposed), and estimating both delays jointly, alongside the *No input*, *Literature* and *Estimated generation interval* conditions above, giving five conditions in total scored with the same pipeline. For Ebola and cholera, all five conditions converged at close to the full weekly origin count (Ebola: 58-60 of a possible 60 across the informed conditions; cholera: 27-29 of 30, the one exception in each case being a single origin with too little preceding data for a stable fit, not a sampling failure). For COVID-19, convergence fell in a clear step with the number of quantities being estimated at once: all origins converged for *No input* and *Literature*; 7 of 24 for estimating the generation interval alone; 15 of 24 for the observation delay alone; and, after doubling the compute budget to 48 hours to rule out cells that simply needed more time rather than being genuinely unidentifiable, only 5 of 24 when both were estimated jointly. Most cells that time out at 24 hours still time out at 48: the shortfall is a sampling difficulty intrinsic to the joint estimation problem, not an artifact of an arbitrary compute limit.

This convergence gradient is itself a result, and a cleaner one than any comparison of forecast skill across it. For Ebola and cholera, where the delay-to-generation-time ratio is modest (Table 1), joint estimation is about as tractable as estimating either quantity alone. For COVID-19, where that ratio is large, adding a second estimated quantity compounds the difficulty faster than either quantity's individual identifiability problem would predict: the observation delay alone already strains the sampler (only a minority of origins converge), and asking the model to locate two confounded quantities at once, rather than one, leaves next to nothing tractable. We do not report a marginal forecast-skill comparison across the five conditions for COVID-19: with convergence this rare and this uneven across conditions, the origins that happen to converge for the harder conditions are not a fair sample of the outbreak, and a comparison built on them would reflect which origins survived rather than what estimating the delays does to the forecast.

## Discussion

Our central result is that delay misspecification degrades situational awareness, both reproduction-number estimation and case forecasting, almost only while transmission is changing. Under steady, near-exponential growth or decline, a wrong generation interval or observation delay is absorbed into a rescaled reproduction number and seeding, leaving both the estimate and the forecast largely intact. This reframes the known small average effect of moderate misspecification (reproducing @Gostic2020 for the reproduction-number bias and consistent with @ODriscoll2021 for forecasts): the effect is concentrated at the turning points that make up a minority of any trajectory rather than being uniformly small. The controlled turning-point experiment localises it: the error rises with the change in the reproduction number over the forecast window, and appears equally when the reproduction number changes without crossing one. The disease-like trajectories and case studies show the same pattern under realistic dynamics.

The two delays play distinct roles. The generation interval fixes the mapping between growth rate and reproduction number through the Euler--Lotka relation, so a wrong generation interval mis-scales the estimated reproduction number even under steady growth, and at turning points it dominates the error in the estimated reproduction number. The observation delay has no such role: under exponential growth it is confounded with the seeding and level and is effectively unidentifiable: away from turning points it is close to free; at turning points, however, it dominates forecast error, because mis-timing the observed peak is more costly for a forecast than mis-scaling its height. The effect also emerged only with horizon: short-term forecasts span too little curvature for the delay to matter, and a detectable effect appeared only at three to six weeks. This suggests that the operationally difficult three-to-four-week horizon is hard because the forecast window begins to span turning points, and locates the difficulty in the transmission dynamics rather than in the delay specification.

The case studies show that whether imposing a literature generation interval improves on omitting it depends on how well the literature value matches the outbreak. For Ebola in Sierra Leone, where the literature value is taken from a study of the same outbreak [@Park2019], imposing the literature value reduced the CRPS by 0.11 over the default. For Yemen cholera, where the literature value is pooled across cholera outbreaks in other settings [@Azman2018], imposing the literature value reduced the CRPS by 0.06. For COVID-19 Delta in England, where the literature value is taken from earlier waves of SARS-CoV-2 transmission [@Sherratt2021], imposing the literature value increased the CRPS by 0.34, almost doubling it.

Estimating the generation interval from the case time series, with the literature value as the central value of the prior, did not substantially change the picture. For Ebola the data and the prior agreed; the estimated posterior gave the same CRPS as imposing the literature value. For cholera the added posterior uncertainty modestly worsened the forecast relative to imposing the literature value (0.27 vs 0.16). For COVID-19 Delta, estimation moved the posterior away from the literature value but the recovery was partial (0.67 vs the imposed 0.73), still substantially worse than omitting the generation interval entirely (0.39).

The weekly analysis explains why: the posterior moves, but the forecast does not follow it. Across dozens of origins per outbreak, the estimated generation interval settled at a value well away from the literature prior and stayed there for the rest of the outbreak (Ebola), or drifted steadily toward or past it as more outbreak-specific data accumulated (COVID-19, cholera): real, sustained movement, not noise around the prior. Estimating the generation interval is not futile in the sense of being stuck at the prior; it is futile in the sense that the forecast rarely depends on the value enough to reward the effort, and, for cholera, it costs a small but measurable amount when the prior is too wide for the data available. For COVID-19, this comparison is necessarily partial: origins from the point the fitting window first spans the July peak onward failed to converge within a 24-hour compute budget, which we read as the sampling analogue of the identifiability difficulty documented for the observation delay, arising once the window contains a turning point.

These case study results extend a related result of @Banholzer2023, who compared short-term probabilistic COVID-19 forecasts from mechanistic renewal equation models with forecasts from statistical time series models across six US states and found the statistical models to be at least as accurate. Our finding offers a candidate explanation: the value of supplying a generation interval to a mechanistic forecast model depends on whether the literature value matches the outbreak being forecast, and a poorly matched literature value can degrade the forecast substantially relative to omitting the generation interval. The literature SARS-CoV-2 generation interval is a plausible source of mismatch in the COVID-19 case, because it was estimated in earlier waves and the Delta variant has been reported to have a shorter generation interval than earlier variants.

### Limitations

The synthetic data scenarios varied the mean of the generation interval while holding the standard deviation fixed. The reproduction number depends on the full shape of the generation interval distribution rather than its mean alone [@Wallinga2007; @Park2019]. Misspecification of the standard deviation may affect forecasts in ways our grid does not capture.

In the three-condition case-study comparison (Table 5) we varied only the generation interval, holding the observation delay at its literature value. The COVID-19 Delta harm in that comparison could in principle also reflect a mismatched observation delay. The synthetic experiments, which misspecified both delays, show the two behave the same way (each is absorbed under steady transmission and bites at turning points), so the mechanism does not hinge on which delay is mismatched. The underlying confounding is itself one of our results: attempting to estimate the generation interval, the incubation period and the reproduction-number trajectory jointly is poorly identified from case data alone, because different combinations reproduce the same observed case counts under steady growth. That degeneracy is exactly why misspecification is absorbed away from turning points, and why it re-emerges where transmission changes and the combinations stop being interchangeable. The confounding is a sampling difficulty as well as a statistical one: for COVID-19, where the delay-to-generation-time ratio is largest, estimating the observation delay alone left only a minority of weekly origins tractable, and estimating both delays at once left almost none, a compounding of difficulty consistent with asking the sampler to locate two confounded quantities rather than one.

Findings held across three case studies but we did not explore the effect of increased noise or data sparsity beyond what was naturally present in those case studies. Greater noise inflates uncertainty in reproduction number estimates [@ODriscoll2021], and correct specification of the generation interval may matter more as data quality degrades.

We used a single forecasting framework. Models with different structural assumptions, for example mechanistic compartmental models or autoregressive count models with explicit lag structure, may respond differently to generation interval misspecification.

The case study comparison rests on three outbreaks. Generalisation to other outbreaks, particularly those with sparser or noisier data, requires further empirical work. We selected the three case studies for the contrast they provide on the closeness between the literature generation interval and the outbreak being forecast rather than as a representative sample of operational forecasting contexts.

### Conclusions

Delay misspecification degrades reproduction-number estimation and case forecasting mainly while transmission is changing, and is close to free while an epidemic grows or declines steadily. This holds for both the generation interval and the observation delay, for both the estimate and the forecast, and it depends on how much the reproduction number changes over the relevant window rather than on the reproduction number crossing one. The one systematic exception is that the generation interval biases the level of the estimated reproduction number even under steady growth, through the Euler--Lotka relation. Three practical implications follow. First, the value of an imported literature delay is greatest when it is used through a turning point and slight when transmission is steady: in the case studies, a generation interval matched to the outbreak (Ebola) improved forecasts, a poorly matched one (COVID-19 Delta) degraded them by almost a factor of two, and the damage fell on the wave's turning points; an outbreak-specific value should be preferred where available, and the sensitivity of a forecast to the delay choice can be read off the recent transmission dynamics. Second, effort spent refining delay distributions is best concentrated on periods of changing transmission and on the medium-range horizons over which such changes fall within the forecast window; in steady phases and at short horizons, the delay specification barely moves either the estimate or the forecast. Third, estimating the generation interval from the case time series is not a reliable substitute for a well-matched imported value: the posterior can move substantially and consistently away from an unsuitable prior, but that movement did not translate into better forecasts in any case study, and with too little data it made forecasts worse.

---

## Notes / scratch (not for final manuscript)

### Papers to reference in intro/discussion

- **Beesley et al. 2022.** *Addressing delayed case reporting in infectious disease forecast modelling.*
  - Outline methods for correcting reporting delays, accounting for over and underreporting of cases: rescaling of real-time data; modelling real-time data with mean model offset; imputation of validation case counts; exclusion of most recent data reports.
  - Outline different approaches for estimating reporting factors.
  - Compare model performance + prediction coverage.
  - Found that forecasting accuracy + prediction coverage always increase when correction methods are implemented. Most appropriate correction method varied by context.

- **Eales and Riley et al. 2024.** *Differences between the true reproduction number and the apparent reproduction number of an epidemic time series.*
  - Discuss the biases that are introduced if epidemic time series are assumed to be a good proxy for infection incidence.
  - They include simulated data generated using a gamma-distributed convolution function + 4 SARS-CoV-2 time series from 2020: infection prevalence; symptom onset; hospitalisation and death.
  - Relevant to my project in that they illustrate the value of accounting for differences b/w infection incidence and epidemic time series using convolution functions (but in the context of estimating Rt).
  - They focus on the effect of excluding incubation period + reporting delay; they assume perfect knowledge of the generation time.

- **Sherratt et al. 2021.** *Exploring data biases when estimating the reproduction number: with insights into subpopulation transmission of COVID-19 in England.*
  - Compared SARS-CoV-2 Rt estimates from different data sources: test-positive cases, hospitalisations, deaths.
  - Highlighted the value in presenting concurrent Rt estimates from different data sources — can provide more information than just one or a pooled estimate.
  - Conclude that there is no inherent best choice of data source — depends on policy context.
  - Delays could vary spatially and temporally.
  - "Comparing each type and source of delay, we judged the benefits of using open data to outweigh the minor observed spatial variation of the delay from onset to test or admission, although at the expense of increased uncertainty."

- **Binny et al. 2022.** *Real-time estimation of the effective reproduction number of SARS-CoV-2 in Aotearoa, NZ.*

- **Wallinga and Lipsitch 2007.** *How generation intervals shape the relationship between growth rates and reproduction numbers.*
  - Limitation: the value of Rt is dependent on the shape of the generation interval distribution as well as the mean. Lower sd leads to higher values for Rt. → I *didn't vary the sd in this analysis. Does this have implications for my dispersion result?*
  - Relationship between growth rate and Rt is dependent on shape of the generation interval.

- **Gostic et al. 2020.** *Practical considerations for measuring the effective reproductive number, Rt.*
  - Types of bias to watch out for when estimating Rt: 1. Systematic over/underestimation. 2. Temporal inaccuracy. *Systematic over/underestimation highlighted by const Rt scenario in our study; temporal inaccuracy highlighted by increasing/decreasing Rt.*
  - "Misspecification of the generation interval is a large potential source of over- or under-estimation, and we find that Rt estimates are most prone to this kind of bias when the true value is substantially greater or less than 1." → *We see this in our results.*
  - They point out that therefore bigger risk of this happening at the start/end of an outbreak or after introduction of very restrictive NPIs. *Might be a point to think about in the discussion i.e. when misspecification in a certain direction is most likely to be an issue.*
  - "However, the practical implications of over/under-estimating Rt are greatest when closer to Rt=1 (i.e. the control threshold)."
  - Calculating confidence intervals: "most of the off-the-shelf tools leave it up to the user to account for these 5 sources of uncertainty when calculating confidence intervals." *What are the five sources of uncertainty?? Double check this + how I am doing it. Implications for dispersion results?*
  - Their methods: synthetic data: initially assumed all infections observed.
  - They found that Cori et al. method accurately estimated Rt, even tracking abrupt changes, when tested under idealised conditions.
  - They say that Bettencourt and Ribeiro method produces biased estimates "consistent with established theory that CoV modulates relationship b/w exp growth rate and Rt." *Apparently discussed in Wallinga et al. 2007 – maybe this will help explain my dispersion results.*
  - *Mention truncation in the discussion + implications of this → often underestimate Rt early in timeseries.*
  - Generation interval misspecification: dependent on variance — form of gt as well as mean — *I only vary mean → limitation to note in discussion.*
  - Difficulties in estimating the generation interval:
    - Generation interval in renewal eqn. describes all contacts, regardless of susceptibility. However, observed intervals are sensitive to changes in susceptibility over time, so "can be biased estimaters of the intrinsic interval and Rt."
    - Observed generation interval can be shortened by self-isolation. (*ref to truncation, for discussion? Or introduction.*)
  - Consequences of misspecifying mean generation interval: mean GI is too high → Rt values will be further from 1 than true value. Mean GI is too low → Rt closer to 1 than true value. *This is borne out in my results.*
  - Point to think about: EpiNow2 etc. allows specification of prior variance at the mean and SD. "Uncertainty around an incorrect value can widen the resulting 95% CI but will not shift the assumed central value towards the truth and will not correct bias in the central Rt estimates."
  - Adjusting for delays: temporal shifts: Cori et al. method will lag true Rt by around mean delay from infection to observation. "If the delay distribution is severely misspecified, all three approaches will incorrectly infer the timing of changes in incidence."

- **Park et al. 2019.** *A practical generation interval-based approach to inferring the strength of epidemics from their speed.*

- Previous work that has compared stats and mechanistic approaches "finding no evidence of substantial variation in predictive accuracy between these model structures." Carlson et al. 2018; Viboud et al. 2018; Reich et al. 2019.

### General notes

From Gostic et al. 2020, the difference between the instantaneous reproduction number and the case reproduction number:

1. **Instantaneous reproduction number:** number of new infections occurring at a single point in time, relative to those in the previous generation and their current level of infectiousness.
2. **Case reproduction number:** average number of new infections that an individual infected on day t<sub>i</sub> will eventually go on to cause.

Need to mention O'Driscoll et al.

### Further research

- Could mention predictability here → further research = try simulating noisier datasets and see if that increases the importance of correct specification.
