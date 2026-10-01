# AI use and loneliness: who is vulnerable in social-emotional AI use?

Analysis code for a two-wave (2024 → 2025) panel study with a lagged-outcome (baseline-adjusted)
specification of whether **purpose-specific generative-AI use** at 2025 is associated with
**subjective loneliness** at 2025, and **for whom** (effect modification by four social factors),
among generative-AI initiators and never-users.

This repository contains the analysis scripts and a variable/estimator codebook. **The real survey
data are not included** (restricted access — see *Data*).

---

## Design in one paragraph

In the JACSIS 2024 + 2025 two-wave panel (n = 9,411 — 2,490 generative-AI initiators + 6,921
never-users; cohort `Q37S1_2025 ∈ {1, 5, 6}`), three AI-use **purpose composites** measured at 2025 —
social/emotional (**A_SE**, focal), productivity/creative (**A_PC**), daily/information (**A_DI**),
each on a 0–4 scale with non-users coded 0 — are related to **UCLA-3 loneliness** (3–12) at 2025,
adjusting for 39 baseline-2024 covariates (including 2024 loneliness). The **primary** analysis is a
single hypothesis-driven test: the A_SE → loneliness main effect, reported per one unit of the
composite. Effect modification by four social factors (age, sex, LSNS-6 friend subscale, LSNS-6
family subscale) is **exploratory**, examined in two ways — categorical moderators and
restricted-cubic-spline moderators — in one joint three-purpose model per moderator. The F test of
each moderator's product terms is Holm-adjusted across the four moderators within each purpose, and
raw and adjusted *P* values are reported together with effect sizes, 95% CIs and stratum sizes. Six
sensitivity analyses of the primary main effect are reported: (1) categorical exposure, (2) users
only, (3) the two social/emotional items separately, (4) overlap weighting for any social/emotional
use, (5) inverse-probability-of-retention weighting, and (6) restriction to baselines completed
before 1 January 2025; E-values quantify sensitivity to unmeasured confounding. Three sensitivity
analyses of the effect-modification analyses are reported: (1) categorical exposure, (2) users only,
and (3) alternative LSNS-6 isolation thresholds.

See **`CODEBOOK.md`** for full variable definitions, recoding, and the estimator/engine.

---

## Data

The analyses use the JACSIS 2024 + 2025 two-wave panel, a **restricted-access** survey. **No data are
distributed in this repository.** To run the pipeline, place the analytic CSV at
`r_code/data/jacsis_2wave2425.csv` (or point the environment variable `JACSIS_2WAVE_2425_PATH` at a
copy kept outside the repository). Two optional inputs feed the attrition analysis and the overlap
count with the earlier study: `r_code/data/jacsis_2024_all.csv` (all valid 2024 respondents;
`JACSIS_2024_ALL_PATH`) and `r_code/data/df_ref5_list.csv` (`RI_ID_2024` of the earlier analytic
sample; `REF5_LIST_PATH`). The scripts skip those sections when the files are absent. Variable
provenance is documented in `CODEBOOK.md`.


---

## Requirements

- R ≥ 4.5 (the reported analyses used R 4.5.3)
- CRAN packages: `here`, `readr`, `dplyr`, `tidyr`, `ggplot2`, `psych`, `GPArotation`, `car`,
  `rms`, `Hmisc`, `interactionRCS`, `sandwich`, `lavaan`, `ggh4x`, `ggtext`

  ```r
  install.packages(c("here", "readr", "dplyr", "tidyr", "ggplot2", "psych", "GPArotation", "car",
                     "rms", "Hmisc", "interactionRCS", "sandwich", "lavaan", "ggh4x", "ggtext"))
  ```

Estimation is linear OLS + delta-method throughout (no bootstrap), with model-based standard errors;
the two weighted sensitivity models (overlap weights, retention weights) use robust (sandwich, HC3)
standard errors from `sandwich`. The spline section skips gracefully if `rms` / `interactionRCS`
are unavailable; the confirmatory factor analysis skips if `lavaan` is unavailable; bold figure
headers fall back to plain text if `ggtext` is unavailable. Package versions used in a run are written
to `mwinit_package_versions.csv`.

---

## Layout

```
r_code/
  moderator-wide.r       # main engine: primary main effect + moderation scans + sensitivities + E-values
  diagnosis.r            # pre-analytic diagnostics (cohort flow, missingness, alpha, VIF, EFA, CFA holdout,
                         #   exposure distribution, characteristics by use history, attrition, baseline dates)
  manuscript_tables.r    # manuscript tables + figures (pure downstream CSV reader)
  data/                  # no data shipped; place jacsis_2wave2425.csv (+ optional files) here
CODEBOOK.md              # variable definitions, framework, engine
README.md
LICENSE                  # MIT
output/                  # created at runtime (tables, figures, logs)
```

Each script uses `here::here()` for paths (run from the repository root) and fixes
`set.seed(20260524)` at the top.

---

## How to run

From the repository root:

```r
Rscript r_code/diagnosis.r          # diagnostics + EFA/CFA (Supplementary Table 1) + descriptive tables
Rscript r_code/moderator-wide.r     # primary + sensitivities + moderation scans -> mwinit_*.csv
Rscript r_code/manuscript_tables.r  # manuscript tables + figures (after both scripts above)
```

Each script runs in well under a minute on real data (all closed-form; no bootstrap).

Optional environment overrides:

```sh
JACSIS_2WAVE_2425_PATH=/path/to.csv    # two-wave CSV kept outside r_code/data/
JACSIS_2024_ALL_PATH=/path/to.csv      # all valid 2024 respondents (attrition; optional)
REF5_LIST_PATH=/path/to.csv            # RI_ID_2024 of the earlier analytic sample (overlap; optional)
```

---

## Outputs

Written under `output/ai_mod/`:

- `modwide_init/{tables,figures,logs}/` — analytic CSVs (`mwinit_*`) + forest/spline figures +
  overlap-weighting diagnostics figures. Delete a previous `output/` before a final run so that no
  stale file remains.
- `diagnosis/` — the 15 diagnostics, EFA (Pearson and polychoric) loadings / fit indices / factor
  correlations, CFA holdout, attrition comparison, baseline dates.
- `manuscript/{tables,figures}/` — Table 1–2 + Supplementary Tables 1–14 + Figures 1–2 +
  Supplementary Figures 2–6 (Supplementary Figure 1, the flow chart, is drawn separately from
  `sup_figure1_flow_counts.csv`). `manuscript/README.md` maps the files to the manuscript exhibits.

Reproducibility: every reported number traces to a logged `write_table()` call; the manuscript
packager re-estimates nothing. Do not hand-edit the published artifacts — fix the script/CSV and
regenerate.

---

## License & citation

Released under the **MIT License** (see `LICENSE`). Please cite the accompanying manuscript; the
JACSIS data are governed by their own access terms (the code is MIT; the data are not redistributed).
