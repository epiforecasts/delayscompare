# Handoff — delayscompare analysis state

Status as of 2026-07-30. Single entry point: what has been done, where everything
lives, the key findings, and any remaining work.

## 1. One-paragraph status

The study compares forecast skill under **imposed** vs **learned** generation-time /
observation-delay assumptions across three pathogens (ebola, covid, cholera). **The
analysis is essentially complete** — all four scientific results are in `results.md`,
scored with GAMMs, figured, and committed on `parallel-timepoints` (unpushed):
(1) **§1 identifiability** — covid's observation delay is non-identifiable from case
counts (posterior = prior; delay↔Rt ridge ~0.99); (2) **Q1 sensitivity** — case
forecasts (1–2 wk) are ~insensitive to *any* delay misspecification; the Rt-scoring
arm merely recovers the expected Euler–Lotka R = 1/M_g(−r) dependence at fixed growth
rate (a consistency check, not a finding); (3) **Q2 impose-vs-learn** —
learning the delays never beats imposing (equal for ebola, worse for cholera, covid
can't learn it); (4) reliability accounting + supplement robustness (case-study
ranking robust to the excluded origins, Spearman ρ ≥ 0.99). Both simulated-scenario
families (trivial const/inc/dec + disease-like resim) and the real case studies are
scored. Remaining is optional polish only (see §6).

## 2. Where the work lives

- **Worktree / branch**: `/home/eidesfun/code/delayscompare-parallel`, branch
  `parallel-timepoints` (off `analysis-plan-revision`). All code committed here;
  nothing pushed. Latest commit `b4ebfcb`.
- **Cluster**: `hpclogin:~/delayscompare` (LSHTM HPC; `ssh hpclogin`). SLURM jobs.
  `module load R` needed in non-interactive shells (`Rscript` not otherwise on PATH).
- **Do NOT touch** cluster jobs named `uk-mech` / `bvd-mh` — separate user work.

## 3. The two arms

### Impose arm — COMPLETE
- Grid: 3 diseases × 3 scenario types (constRt→const_low/high, incdecRt→inc/dec,
  casestudy) × 6 GT × 6 INC × up to 8 forecast origins = 2592 tasks
  (`slurm/02_run_scenarios.sh`, runners `scripts/06b/06c/06d`).
- **3376 / 3384 fits converged (99.76%)**. **8 excluded** by criterion (max R-hat ≥
  1.05 OR ≥ 100 divergences), all at the extreme gt1–gt2 × inc6 corner. Enumerated
  with mechanism in `docs/rerun-and-convergence-provenance.md`.
- Effective sampler lever was **`warmup`** (250 → 2000–4000), not adapt_delta/samples.
- Aggregated to `results/agg/` (75 files) by `scripts/07_resultsprocessing.R`, which
  **applies the convergence filter** (drops non-converged fits from samples/R/id,
  keeps a `converged`-flagged diagnostics record). Verified: excluded fits absent
  from aggregated samples.
- **Figures** (local `results/*.png`): `fig1_*` (Q1 GT×INC heatmaps + rank +
  marginal, per disease), `fig_casestudy_*` (per-disease + combined CRPS/rank +
  marginal), `fig_diagnostics_*`. Case-study condition scoring:
  `scripts/score_casestudy_conditions.R`.
- `fig_resim.R` produces nothing — "resim" was never in the 2592-task grid. Expected.

### Learn arm (widepriors, weight_prior=FALSE) — PARTIAL
- Runner `scripts/06f_scenariorun_widepriors.R` → `sim_weightprior()` in
  `R/scenario_loop.R`. SLURM `slurm/02_run_scenarios_widepriors_single.sh <disease>`.
  Array = (timepoint × vary), `vary ∈ {gt, inc, both}`: covid 18 (6 tp × 3),
  ebola/cholera 24 (8 tp × 3).
- Uses `delays_widepriors` (10%-relative prior SDs) and estimates the delay
  distribution: `vary=gt` → generation time (`Gamma(shape=Normal, rate=Normal)`);
  `vary=inc` → incubation (`LogNormal(meanlog=Normal, sdlog=Normal)`); `both` → both.
  Reporting delay always fixed at literature value.
- `sim_weightprior` now captures R-hat (`return_fit=TRUE` + `fit_diagnostics`, fit
  dropped **after** `get_samples`; commit `b4ebfcb` — note the ordering bug fixed:
  fit must survive until `get_samples`). `warmup` wired in.
- **Completion by disease × vary** (cluster
  `results/res_<disease>_widepriors_FALSE_<vary>_latest_diagnostics_tp<N>-<N>.rds`):

  | disease | gt | inc | both |
  |---|---|---|---|
  | covid | 6/6 | **1/6 (tp1 only)** | **0/6** |
  | ebola | 5/8 | 8/8 | 7/8 |
  | cholera | 7/8 | 8/8 | 8/8 |

- All completed learn fits converged. The missing covid `inc`/`both` cells do not
  finish — non-identifiable (see §5).
- **Learn outputs are on the cluster only**, FLAT `results/` dir (old naming:
  `res_<disease>_widepriors_FALSE_<vary>_latest_<type>_tp<N>-<N>.rds`; types =
  samples, id, R, summary, warnings, timing, diagnostics). **Not synced locally**
  (`results/widepriors/` empty here). **`07b` NOT yet run.**
- `scripts/07b_widepriors_aggregate.R` reads those flat files, applies the
  convergence filter, writes `results/widepriors/`. Then `scripts/fig_widepriors.R`.

## 4. Re-run / provenance tracking

- Impose: `results/tighter_sampler_cells.tsv`, `results/rerun_log.tsv`.
- Learn covid: `results/widepriors_rerun_log.tsv` (cluster) — rounds RESEED1–5 of a
  stochastic seed-lottery on the stuck covid cells. RESEED1–3 cleared 3 via good
  seeds; RESEED4–5 cleared **zero** (the remaining 11 are structurally stuck).
- `RESEED5` (job 6418219) was cancelled 2026-07-23 once the diagnostic confirmed the
  11 stuck covid cells are non-identifiable (not seed-unlucky). No learn-arm covid
  jobs are running now.

## 5. THE KEY FINDING — covid observation delay is non-identifiable

Confirmed by diagnostic job `6418486` (script `~/delayscompare/diag_learn.R`, output
`slurm/logs/diaglearn_*.out`), a short tree-depth-capped fit of the stuck covid
`vary=inc` tp3 cell:

- **Incubation delay unpinned**: `delay_params[1]` (incubation meanlog) posterior SD
  = 0.599 vs prior SD = 0.10 (6× wider), R-hat = 8.54.
- **Identifiability ridge, explicit**: |correlation| of the incubation delay with the
  Rt/GP/infection parameters = **max 0.99, mean 0.86, 54/60 params > 0.5**.
- **Timing undetermined**: worst R-hat params were `infections[...]`, `reports[...]`,
  `obs_reports[...]` (R-hat ~10¹³, n_eff = 2).
- **Grind signature**: 12.8% of transitions hit the tree-depth cap even at 10
  (production uses 20 → 2²⁰ leapfrog steps/iteration → runs for days).

**Mechanism.** R(t) = ∫ I(t−s) f(s) ds. During exponential growth I(t)=I₀e^{rt},
R(t) = [I₀·M_f(−r)]·e^{rt} — the delay enters only through a scalar confounded with
the seeding level, so it is non-identifiable on log-linear stretches; only the growth
rate r is recoverable. The delay is identifiable **only at trend changes (curvature)**
— a peak/trough/bend is the landmark. A flexible Rt/GP makes it worse (manufactures
curvature to explain the peak's timing instead of the delay). `vary=gt` succeeds
because the generation time is identified from the growth *rate* (present always).

**Why covid specifically**: covid observation delay (~9.6 d) is ~2.7× its generation
time (~3.6 d); ebola (~0.8×) and cholera (~1.2×) have delay ≈ GT, so their delays
stay identifiable. This is the natural-parameter version of the impose arm's
short-GT × long-delay near-singular corner (excluded 1485 cell).

**Trend-change framing (checked, refuted → finding strengthened)**: the delay is in
principle identifiable only at trend changes (curvature). But the production-settings
tp1 fit converges with its incubation posterior *exactly equal to the prior* (mean
1.607/SD 0.100 vs prior 1.608/0.10) — prior-dominated, not identified — and the
delay↔Rt correlation is ~0.99 at tp1 too. So the delay is *never* identified from
these case counts: the flexible Rt/GP absorbs whatever curvature could pin it. Final
wording is in `results.md` §1.

## 6. NEXT STEPS (ordered)

1. **Cancel the futile re-run**: DONE — `RESEED5` (6418219) cancelled 2026-07-23.
2. **Close the covid learn arm**: DONE — 11 covid `inc`/`both` cells accepted as
   non-identifiable (documented `results.md` §1). Covid learn = `gt` (6) + `inc`
   tp1; ebola full; cholera all bar `gt/tp7`.
3. **Aggregate the learn arm**: DONE — `07b_widepriors_aggregate.R` →
   `results/widepriors/` (32 files, convergence-filtered); `fig_widepriors.R` →
   `fig_widepriors_crps.png`. Retained-cell table in `results.md` §4.
4. **Sync back**: DONE — `results/widepriors/`, `results/widepriors_rerun_log.tsv`,
   `fig_widepriors_crps.png` in the local worktree. (cholera `gt/tp7` re-run job
   6467184 may still be finishing on the cluster; if it lands, re-aggregate to
   pick up that one cell.)
5. **Results go in `docs/results.md`** (already seeded — §1 is the covid
   identifiability finding). Extend it as analysis proceeds. Do NOT put results in
   `analysis_plan.md`: the plan changes only if the *methodology* changes.
   `rerun-and-convergence-provenance.md` holds the convergence/exclusion provenance.
6. **Trend-change prediction — DONE (refuted, finding strengthened).** Diagnostics
   (`diag_learn_tp1.R`, `diag_learn_tp1_prod.R`; jobs 6467755, 6467858) showed tp1 is
   prior-dominated (production posterior = prior exactly: mean 1.607/SD 0.100 vs prior
   1.608/0.10), not identified by a trend change. The delay↔Rt ridge (~0.99) is
   present at all timepoints; the flexible Rt/GP absorbs the identifying curvature.
   `results.md` §1 updated to the final wording: covid observation delay is never
   identified from case counts. No curve overlay needed.
7. **Q2 impose-vs-learn — DONE.** `scripts/score_q2_gamm.R` (samples equalised to
   2000, per-origin log-CRPS, `mgcv::bam` GAMM, condition marginal effects with CIs).
   Outputs `results/q2_perorigin_logcrps.csv`, `q2_gamm.rds`,
   `q2_condition_marginal_effects.csv`, `fig_q2_marginal_effects.png`. Result in
   `results.md` §4: learning never beats imposing.
8. **Q1 sensitivity — DONE (both outcomes).** `scripts/score_q1_gamm.R` (case CRPS)
   and `scripts/score_q1_rt_gamm.R` (Rt CRPS), over the GT×INC grid across the trivial
   *and* disease-like (resim) scenarios; GAMM `s(gt,by=disease)+s(inc,by=disease)+
   trajectory+s(time,by=disease)+horizon`; marginal effects with CIs; figures
   `fig_q1_sensitivity.png`, `fig_q1_rt_sensitivity.png`. Result in `results.md` §3:
   case forecasts insensitive to any misspecification; the Rt-vs-GT U-shape is the
   mechanical Euler–Lotka consequence (validation, not a finding).
9. **Disease-like (resim) Q1 arm — DONE.** Regenerated fresh (`03`→`05` under current
   code — needed 5 EpiNow2 1.9 API fixes to `03`), grid run at warmup=500, all three
   diseases 36/36 cells (~7 scattered origins excluded), aggregated + figured
   (`fig_resim_*`). rerun log for resim re-rolls is ad-hoc (jobs 6478881-3).
10. **Supplement robustness — DONE.** `scripts/score_casestudy_robustness.R` →
   `results.md` §5: case-study ranking robust to common-origin restriction (covid
   ρ=0.99, ebola/cholera 1.0). `fig_casestudy_robustness.png`.

**Optional remaining polish** (nothing blocks the write-up):
- Trivial vs disease-like Q1 could be reported separately (currently combined via the
  `trajectory` covariate).
- If a different sampler is ever wanted for the stuck learn cells: expose
  `max_treedepth` as an env var in `sim_weightprior` (hardcoded 20).
- **Branch review / merge**: user drives; never push or merge without instruction.

## 7. Gotchas

- **Docs routing**: `results.md` = findings; `analysis_plan.md` = plan/methods only
  (edit only if the plan changes); `rerun-and-convergence-provenance.md` =
  convergence/exclusion provenance. Don't write results into the plan.
- Non-interactive `ssh hpclogin` needs `module load R` for `Rscript`.
- zsh `noclobber` is on: use `>|` to overwrite in redirects.
- rsync remote paths: use home-relative (`hpclogin:delayscompare/...`), not `~`.
- Learn-arm files use OLD flat-`results/` naming (not `raw/`); `07b` bridges to
  `results/widepriors/`.
- `extract_diagnostics()` was removed from `funcs_data.R`; `07b` now reads the new
  per-fit diagnostics files directly.
- Commit authoring: `GIT_AUTHOR_NAME=sbfnk-bot
  GIT_AUTHOR_EMAIL=242615673+sbfnk-bot@users.noreply.github.com`, co-author line
  `Co-authored-by: sbfnk <sebastian.funk@lshtm.ac.uk>` only, single-line messages,
  no issue refs, British English.

## 8. Key files

- Model core: `R/scenario_loop.R` (`sim_scenarios`=impose, `sim_weightprior`=learn,
  `fit_diagnostics`), `R/funcs_data.R`, `R/funcs_scoring.R`
  (`filter_by_diagnostics`), `scripts/02b_definedelays.R` (`delays`,
  `delays_widepriors`).
- Runners: `06b/06c/06d` (impose), `06f` (learn). Aggregation: `07` (impose), `07b`
  (learn). Scoring/figures: `score_casestudy_conditions.R`, `fig1.R`,
  `fig_casestudy.R`, `fig_diagnostics.R`, `fig_widepriors.R`.
- Diagnostics: `diag_learn.R` (learn identifiability, cluster), `diag_1485.R`
  (impose corner), `scripts/diag_gt_recovery_grid.R` (earlier recovery diagnostic).
- Docs: `docs/analysis_plan.md`, `docs/rerun-and-convergence-provenance.md`,
  `docs/diag_recovery_findings.md`, this file.
