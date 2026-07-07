# Recovery diagnostic: can the "learn" arm recover a misspecified generation time?

Full grid, run on the cluster as SLURM array 6267347 (288 cells; scripts
`diag_gt_recovery_grid.R` and `diag_grid_aggregate.R`). Each cell simulates cases
from a known generation time (COVID-like, mean 3.78 days), then estimates the GT
from a prior centred at 0.5x, 1x, or 2x the true mean, and records how far the
posterior moves from the prior toward the truth. `frac` below is the share of the
prior-to-truth gap the posterior closes: 1 is full recovery, 0 is stuck on the
prior, negative is movement away from the truth.

## What the grid shows

Nonparametric (Dirichlet), averaged over the four Rt trajectories:

| misspec (prior mean) | concentration | post mean | frac gap closed |
|---|---|---|---|
| 0.5 (1.8) | 5 | 2.37 | 0.11 |
| 0.5 (1.8) | 200 | 2.20 | 0.01 |
| 2.0 (7.5) | 5 | 6.06 | -0.19 |
| 2.0 (7.5) | 200 | 5.71 | -0.01 |

Parametric (Gamma with priors on shape and rate), by prior width:

| misspec (prior mean) | width | post mean | frac gap closed |
|---|---|---|---|
| 0.5 (1.8) | 0.50 | 4.42 | 1.32 |
| 0.5 (1.8) | 0.25 | 2.70 | 0.45 |
| 0.5 (1.8) | 0.10 | 2.26 | 0.23 |
| 2.0 (7.5) | 0.50 | 7.02 | 0.05 |
| 2.0 (7.5) | 0.25 | 6.51 | 0.20 |
| 2.0 (7.5) | 0.10 | 6.67 | 0.16 |

Truth = 3.78.

## Reading

The nonparametric Dirichlet arm barely learns. The posterior GT mean stays close
to whatever prior it is given, closing only 1-11% of a misspecified-mean gap, and
neither the concentration nor the Rt trajectory changes that. The free-form corner
(concentration 1) is unstable: 20 of the 288 cells crashed during sampling, all at
the combination of a wrong prior with weak concentration, and even when the prior
is correct the free-form fit overshoots the truth (3.78 to 5.30). At this data
scale the free-form shape is not identifiable, so the estimate returns the prior.

The parametric arm does recover, tunably by prior width, but asymmetrically. A
too-short prior (1.8) is pulled up well under a loose prior. A too-long prior (7.5)
barely corrects downward: a longer generation time trades off against a lower R at
the observed growth rate, so the likelihood does not penalise it. An over-long
generation time will therefore survive in the posterior, which is a blind spot for
the learn arm.

Growth-rate change matters less than expected for the parametric mean. The
constant-Rt controls recover about as well as the increasing and decreasing
trajectories, because the growth-rate-to-R relationship constrains the mean even
without a trajectory change. A growth-rate change does matter for the free-form
shape, which never identifies regardless.

## Implication for the plan

The learn arm works parametrically with a deliberately wide prior, and does not
work as a free-form Dirichlet estimate at these data volumes. The plan's learn arm
would rest on the parametric form, with the over-long-GT blind spot reported rather
than hidden. Whether to drop the Dirichlet arm entirely or keep it as a documented
negative result is a design decision.
