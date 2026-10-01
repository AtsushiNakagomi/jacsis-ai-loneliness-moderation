# Codebook — AI use and loneliness: who is vulnerable in social-emotional AI use?

Standalone analytic spec. Script: `moderator-wide.r` (analysis) + `diagnosis.r` (pre-analytic checks) + `manuscript_tables.r` (downstream packager).

---

## One-line claim

In the JACSIS 2024+2025 panel (n=9,411 — 2,490 generative-AI initiators + 6,921 never-users; Q37S1_2025 ∈ {1,5,6}), this study estimates the association of three AI-use purpose composites (A_SE / A_PC / A_DI; 0–4 scale, non-users coded 0) at 2025 with UCLA-3 loneliness at 2025, adjusting for 39 baseline-2024 covariates (including 2024 loneliness; a lagged-outcome, baseline-adjusted specification in which exposure and outcome are measured in the same wave), and examines effect modification by four social-factor moderators (age, sex, LSNS-6 friend subscale, LSNS-6 family subscale), one moderator at a time, in one joint three-purpose model per moderator. A_SE is the focal exposure; the three-purpose joint model adjusts each purpose for the other two.

---

## 0. Pipeline overview

Two analytic scripts + one packager:

- **`moderator-wide.r`** — analytic blocks (build → primary main effect + sensitivity analyses 2 and 3 → categorical-moderator scan + LSNS threshold sweep → spline scan → users-only scan → categorical-exposure scan (sensitivity analysis 1) → sensitivity analyses 4-6 + E-values). Linear OLS + delta-method; no bootstrap; about a minute on the real data.
- **`diagnosis.r`** — 15 diagnostics (D1–D15, incl. D10 EFA → Sup Table 1, D11 CFA holdout, D13 characteristics by use history, D14 attrition); produces `flags_<ts>.md` for triage.
- **`manuscript_tables.r`** — pure downstream CSV reader; assembles publication tables/figures.

Standard errors: model-based (conventional) OLS throughout. Only the two weighted sensitivity models
(sensitivity analysis 4, overlap weights; sensitivity analysis 5, retention weights) use robust (sandwich,
HC3) errors, because their weights are estimated.

### 0.1 Inferential framework — one hypothesis-driven primary; exploratory effect modification

**PRIMARY (hypothesis-driven; the study was not preregistered):** the **A_SE → loneliness main effect** (`mwinit_overall.csv`), per one unit of the composite. One hypothesis → no multiple-testing correction. A_PC/A_DI main effects are secondary.

**EXPLORATORY (hypothesis-generating):** the effect-modification analyses (categorical scan, spline scan, and their sensitivity analyses). Moderation is reported with effect sizes + 95% CIs + stratum n, the F test of each moderator's product terms with raw and **Holm-adjusted** *p* (4 tests per purpose, one per moderator, within each analysis), and the per-category product-term γ with raw *p*; no subgroup result is claimed as confirmed.

| Component | Manuscript exhibit | Spec |
|---|---|---|
| **Spline scan** | Figure 1 (all 3 purposes), Sup Fig 4 | Continuous A_p × RCS-spline M (age, LSNS friends, LSNS family); marginal-association curves + 2-df test per moderator (+ sex 1-df) → Sup Table 10; percentile table → Sup Table 11 |
| **Categorical scan** | Figure 2 (all 3 purposes) | Continuous A_p × categorical M (age band, sex, friend isolation, family isolation); per-stratum slopes + F test per moderator + per-category product-term contrasts + forest → Sup Tables 10 and 12 |
| **Effect-modification sensitivity analysis 1** | Sup Fig 5 | Categorical A_p (0/T1/T2/T3) × categorical M — dose-response by subgroup |
| **Effect-modification sensitivity analysis 2** | Sup Table 14, Sup Fig 6 | Categorical scan restricted to users (n = 2,490) |
| **Effect-modification sensitivity analysis 3** | Sup Table 13 | Categorical scan with isolation at <3 / <6 / <9 / <12 |

*Sensitivity analyses of the PRIMARY main effect (Table 2 / Sup Table 7): 1 = categorical exposure, 2 = users only, 3 = social/emotional items separately, 4 = overlap-weighted (any social/emotional use), 5 = inverse-probability-of-retention weighting, 6 = baseline completed before 2025. The script labels them SENS1–SENS6.*

Engine: all moderator-wide sections fit **one joint model per moderator** with all three A_p × M interactions simultaneously.

### 0.2 Study-design timeline

```
   wave-2024            wave-2025          (each ● = end-of-year measurement)
      ●─────────────────────●
            (during 2024→2025: AI initiation / use; purpose frequencies @ 2025)

   A: 3 AI-use purpose composites @ 2025, NON-USERS = 0            ← exposures
   M: 4 social-factor moderators (cat + 3 continuous splines)      ← moderators
   Y: UCLA-3 loneliness sum @ 2025 (3–12)                          ← outcome
   C: 39 baseline-2024 covariates (incl. baseline_ucla3)            ← confounders
```

**Temporal design (two-wave panel, lagged-outcome specification)**: 2024 baseline → AI use/initiation 2024→2025 → 2025 loneliness. Baseline-loneliness-adjusted (lagged-outcome). Purpose-frequency and loneliness are both measured at the 2025 wave, so the design does not establish the temporal order of use and loneliness and reverse causation cannot be excluded; the initiation contrast and the baseline-loneliness adjustment are the design's anchors. Respondents whose baseline was completed on or after 1 January 2025 (`baseline_after_2025`, from the completion timestamp) are counted and excluded in SENS6.

### 0.3 Output file-naming convention

All outputs go to `output/ai_mod/modwide_init/{tables,figures,logs}/`. Prefix `mwinit_`.

The **Component** column gives the §6 home and **→** the manuscript exhibit (§8).

| File | Component | Contents → exhibit |
|---|---|---|
| `mwinit_sample_flow.csv` | build | raw → cohort{1,5,6} → CCA → initiated/never → Sup Fig 1 (flow) |
| `mwinit_overall.csv` | PRIMARY (6.1) | 3-purpose unmoderated main effects per unit, with `sd_A`, `beta_per_sd`, `pct_zero`, `r2` → **Table 2** |
| `mwinit_overall_full_coefficients.csv` + `_fit_statistics.csv` | PRIMARY (6.1) | every coefficient of the primary model; n, R², residual SD → **Sup Table 6** |
| `mwinit_cat_overall.csv` + `mwinit_cat_cutpoints.csv` | SENS1 (6.3) | categorical-A main effects: all three purposes 0/T1/T2/T3 + `ai_init`; contrasts vs never-users and vs AI users without the purpose → **Table 2** (A_SE block) + **Sup Table 7** |
| `mwinit_overall_init.csv` | SENS2 (6.1) | main effects among users only (per unit; users' SDs) → **Table 2** + **Sup Table 7** |
| `mwinit_overall_items.csv` | SENS3 (6.1) | the two S/E items separately and together → **Sup Table 7** |
| `mwinit_overall_overlap_weighted.csv` + `mwinit_ow_*.csv` + `mwinit_ow_balance.{png,pdf}` + `mwinit_ow_propensity_overlap.{png,pdf}` | SENS4 (6.6) | any S/E use vs none: unweighted, overlap-weighted, overlap-weighted + covariates; balance, weight and propensity summaries → **Sup Table 7**, **Sup Table 8**, **Sup Figs 2–3** |
| `mwinit_overall_attrition_weighted.csv` + `mwinit_ipaw_weight_summary.csv` | SENS5 (6.6) | inverse-probability-of-retention weighted primary model → **Sup Table 7** |
| `mwinit_overall_pre2025_baseline.csv` + `mwinit_baseline_timing.csv` | SENS6 (6.6) | baselines completed before 1 Jan 2025 → **Sup Table 7** |
| `mwinit_evalues.csv` | E-values (6.6) | E-values for the primary estimate and its CI limit → **Sup Table 9** |
| `mwinit_forest_<OUTCOME>.{png,pdf}` | Categorical scan (6.2) | grouped forest of per-unit β (moderators = header rows; purpose = columns) → **Figure 2** (all 3 purposes) |
| `mwinit_stratified.csv` | Categorical scan (6.2) | per-stratum slope (per unit) + delta-SE + 95% CI (+ standardized β for reference) → **Sup Table 12** (`_subgroup_primary`) |
| `mwinit_interaction.csv` | Categorical scan (6.2) | F test of the moderator's product terms per moderator × purpose with raw p, Holm p (4 per purpose) and BH q → **Sup Tables 10 and 12** (`_primary_omnibus_tests`) |
| `mwinit_interaction_contrasts.csv` | Categorical scan (6.2) | per-category effect-modification contrast γ (the A_p×M[level] **product term**) + own SE/z/raw p, with the moderator test p / Holm p attached → **Sup Table 12** (`_primary_contrasts`) |
| `mwinit_lsns_threshold_sweep.csv` | Categorical scan (6.2) | stratified slopes and interaction p at LSNS-6 thresholds <3 / <6 / <9 / <12 (effect-modification sensitivity analysis 3) → **Sup Table 13** |
| `mwinit_cat_*.csv` + `mwinit_cat_forest_<OUTCOME>.{png,pdf}` | SENS1 (6.3) | categorical-A × categorical-M dose-response (stratified / per-cell product-term contrasts; effect-modification sensitivity analysis 1) → **Sup Fig 5** (forest) |
| `mwinit_stratified_init.csv` + `mwinit_interaction_init.csv` + `mwinit_interaction_contrasts_init.csv` + `mwinit_forest_init_<OUTCOME>.{png,pdf}` | users only (6.4) | users-only scan (effect-modification sensitivity analysis 2) → **Sup Table 14**, **Sup Fig 6** (forest) |
| `mwinit_spline_AME.csv` + `_<OUTCOME>.{png,pdf}` | Spline scan (6.5) | spline AME curves (61-pt grid over the full moderator range) → **Figure 1** (all 3 purposes) |
| `mwinit_spline_moderation_tests.csv` | Spline scan (6.5) | 2-df test per continuous moderator + 1-df nonlinearity test + sex (from the categorical scan); Holm p over the 4 tests per purpose → **Sup Table 10** |
| `mwinit_spline_AME_quantiles.csv` | Spline scan (6.5) | AME at M-quantiles {p10..p90} → **Sup Table 11** |
| `mwinit_spline_predicted.csv` + `_<OUTCOME>.{png,pdf}` | Spline scan (6.5) | predicted UCLA-3 (3–12) at A_p ∈ {0,T1,T2,T3} + 95% CI → **Sup Fig 4** |
| `mwinit_package_versions.csv` | build | R and package versions → Methods |

---

## 1. Cohort & sample

| | |
|---|---|
| Filter | `Q37S1_2025 ∈ {1, 5, 6}` — never-users (1) + 2025 initiators (5,6). Pre-2025 starters (2–4) excluded. |
| `Q37S1_2025` codes | 1=never used; 2=used before, not now; 3=started Nov 2022–Dec 2023; 4=started Jan–Dec 2024; 5=started Jan–Jun 2025; 6=started Jul–Dec 2025. Codes 2/3/4 excluded for clean temporal ordering. |
| Real-data flow | 12,194 raw → 9,411 cohort = **2,490 initiated + 6,921 never** → 9,411 complete-case (no missingness on Y + 3 A + 39 C). |
| Assertion | `stopifnot(nrow(dm) >= 3000L)`; per-section guards on the number of users and non-degenerate exposure. |
| `ai_init` | `as.integer(Q37S1_2025 ∈ {5,6})` — 1 = initiated in 2025, 0 = never-user. |
| `ai_start_label`, `ai_current_user` | descriptive labels of the Q37S1_2025 codes (diagnosis.r D13; Sup Table 4) |
| `baseline_date`, `baseline_after_2025` | baseline completion date (from `回答完了日時_2024`, UTC → JST) and indicator for on/after 2025-01-01 (SENS6; Sup Table 7) |
| Optional inputs | `jacsis_2024_all.csv` (all valid 2024 respondents; `Monitor_ID` link; attrition D14 + SENS5) and `df_ref5_list.csv` (`RI_ID_2024` of the earlier analytic sample; overlap D15) |

**Recode helpers**:

```r
ucla_recode <- function(M) pmax(1, pmin(4, 5 - M))     # UCLA-3 raw 1..4 → 4..1
k6_recode   <- function(M) pmax(0, pmin(4, 5 - M))     # K6 raw 1..5 → 4..0
lsns_recode <- function(M) pmax(0, pmin(5, M - 1L))    # LSNS-6 raw 1..6 → 0..5
```

---

## 2. Exposure (A) — 3 purpose composites, non-users = 0

Item source = Q37S3 in JACSIS 2025 (purpose-of-AI-use frequency, raw 1–5). For initiators: `A_p = mean(items) − 1` (0–4 scale). For never-users (`ai_init==0`): `A_p = 0` for all three.

| Q_No (2025) | Item | Composite |
|---|---|---|
| Q37S3.8 | Casual conversation / chat | A_SE |
| Q37S3.9 | Emotional support / venting | A_SE |
| Q37S3.1 | Document drafting / writing assistance | A_PC |
| Q37S3.2 | Translation / summarization | A_PC |
| Q37S3.4 | Image / video generation | A_PC |
| Q37S3.5 | Learning support | A_PC |
| Q37S3.3 | Information search / lookup | A_DI |
| Q37S3.6 | Daily-life planning | A_DI |
| Q37S3.7 | Health advice / health information | A_DI |

```r
A_SPEC <- list(A_SE = paste0("Q37S3.", c(8,9),     "_2025"),
               A_PC = paste0("Q37S3.", c(1,2,4,5), "_2025"),
               A_DI = paste0("Q37S3.", c(3,6,7),   "_2025"))
for (a in names(A_SPEC)) {
  v <- rowMeans(df[, A_SPEC[[a]]], na.rm = FALSE) - 1
  v[df$ai_init == 0L] <- 0
  df[[paste0(a, "_continuous")]] <- v
}
# centered on the {1,5,6} analytic sample
for (a in c("A_SE","A_PC","A_DI")) dm[[paste0(a, "_c")]] <- dm[[paste0(a,"_continuous")]] - mean(dm[[paste0(a,"_continuous")]])
```

**Margin caveat**: the never-users-0 coding mixes extensive (any-use) + intensive (how-much) margins. Sensitivity analysis 2 (users only) removes never-users but still compares users of a purpose with non-users of it; sensitivity analysis 1 (categorical) decomposes the margins as AI users without any purpose vs never-users, T_k vs never-users, and T_k vs AI users without the purpose.

**Item-level exposures**: `A_SE_conv` (Q37S3.8 − 1), `A_SE_emo` (Q37S3.9 − 1), never-users 0 (sensitivity analysis 3); `A_SE_any = as.integer(A_SE > 0)` (sensitivity analysis 4, overlap weighting). Real data: 89.2% of the cohort at 0 on A_SE, 79.6% on A_PC, 75.9% on A_DI (`diagnosis/12_exposure_distribution.csv`).

### 2.1 Categorical-exposure decomposition (sensitivity analysis 1)

Each A_p recoded as a 4-level factor: **0 / T1 / T2 / T3** where T1–T3 are tertiles of the non-zero distribution among initiators. Fallback ladder if quantile boundaries collapse: 4-level → 3-level (0/low/high) → binary (0/any). Reference = "0".

In the *main-effect* model all three factors enter together **plus `ai_init`**, so that the "0" level of each purpose refers to AI users without that purpose and never-users are separated: `ai_init` = AI user with no use of any purpose vs never-user; `T_k` = T_k vs AI user without the focal purpose (other purposes fixed); `ai_init + T_k` = T_k vs never-users. (Three never/no-use/T1–T3 factors cannot be entered together: their non-reference dummies all sum to `ai_init`.) In Table 1 and Sup Tables 2–3 the "0" column is split into **Never-users** and **AI users, no use**. The categorical × moderator scan (6.3) uses the 4-level factors.

Real-data cutpoints (all `tertile_4level`):

| Purpose | n_nonzero | q33 | q67 |
|---|---|---|---|
| A_SE | 1,020 | 1.00 | 2.00 |
| A_PC | 1,923 | 0.50 | 1.25 |
| A_DI | 2,270 | 1.00 | 1.67 |

### 2.2 EFA validation of the 3-purpose structure (→ Sup Table 1)

The A_SE / A_PC / A_DI composite structure is validated by **exploratory factor analysis** of the 9 Q37S3 purpose items, run in `diagnosis.r` on the **users** (complete-case on the 9 items; n = 2,490 — never-users have no purpose items). Packaged as **Sup Table 1**.

| Aspect | Choice |
|---|---|
| Input correlation | **Polychoric** correlations (ordered five-point items; `cor = "poly"`, `correct = 0`; `10_efa_purposes_poly_*`) — the solution reported in Sup Table 1A; the identical EFA on Pearson correlations (`10_efa_purposes_*`) is shown in Sup Table 1B |
| Adequacy checks | `psych::KMO` (overall + per-item MSA) + `psych::cortest.bartlett` (sphericity) — verified **before** interpreting factors |
| Factor retention | Horn's parallel analysis (`psych::fa.parallel(fa="fa", fm="minres", n.iter=20)`); cross-checked against the theoretical 3 blocks; capped at items/3 |
| Extraction | Minimum residual / OLS (`fm="minres"`) |
| Rotation | **Oblimin (oblique)** — purposes inter-correlate; varimax fallback if GPArotation unavailable |
| Reported | per-factor: SS loadings, proportion/cumulative variance, oblique factor-correlation Φ; per-item: pattern loadings, h², u², complexity, MSA; fit: KMO, Bartlett, RMSEA (+90% CI), TLI, RMSR, BIC, model χ² |
| Software | R `psych` + `GPArotation`; seed 20260524 |

Parallel analysis indicates **3 factors**, and A_SE / A_PC / A_DI each load most strongly on their own factor in both solutions (fit statistics in Sup Table 1).

**CFA holdout (D11).** The three-factor structure is tested in the respondents who initiated use before 2025 (Q37S1 codes 3–4; excluded from every other analysis) with a confirmatory factor analysis of ordered items (lavaan, WLSMV, `std.lv = TRUE`), compared with a one-factor model (scaled χ² difference). Outputs `11_cfa_holdout_{fit,loadings,factor_cor}.csv` → Sup Table 1C.

---

## 3. Moderators (M) — 4 focal social factors, one-at-a-time

> **One moderator at a time.** Each model carries product terms for a single moderator only (joint over the 3 purposes — see §0.1 engine). Other moderators enter as main-effect covariates (confounders).

| Moderator | Levels (ref first) | Construction |
|---|---|---|
| `age_band` | `<=39` / `40-64` / `65+` | cut of `age_2024` |
| `sex_cat` | `Male` / `Female` | `sex_female` |
| `friends_iso` | `Connected` / `Isolated` | `baseline_lsns6_friends < 6` (Lubben's total-scale criterion of <12 — fewer than two contacts per item on average — applied to the three-item subscale) |
| `family_iso` | `Connected` / `Isolated` | `baseline_lsns6_family < 6` |

The stratified analysis is repeated with the subscale threshold at <3, <9 and <12 (`LSNS_ALT_CUTS`; `mwinit_lsns_threshold_sweep.csv`; effect-modification sensitivity analysis 3).

**Continuous moderator counterparts** (Spline scan; sex excluded as binary): `baseline_lsns6_friends_c`, `baseline_lsns6_family_c`, `age_2024_c` (mean-centered on the analytic sample).

**Continuous `age_2024` always stays in C** even when `age_band` is the moderator — the band moderates; the continuous form confounds. Same convention for LSNS continuous forms when their categorical (iso <6) is the focal moderator.

**Baseline loneliness is a confounder, not a moderator** (`baseline_ucla3` ∈ C; ceiling concern).

---

## 4. Outcome (Y)

`Y_ucla3` — UCLA-3 loneliness at 2025. JACSIS Q66 raw scale at 2025: `1=常にある (always)`, `2=時々ある (sometimes)`, `3=ほとんどない (hardly ever)`, `4=決してない (never)`. Recoding `ucla_recode = pmax(1, pmin(4, 5 − x))` maps raw 1 → 4, raw 4 → 1, so **recoded higher = lonelier**; the sum runs 3–12 (conventional scoring).

| Q_No (2025) | Item |
|---|---|
| Q66.1_2025 | "I lack companionship" |
| Q66.2_2025 | "I feel left out" |
| Q66.3_2025 | "I feel isolated from others" |

```r
Y_ucla3 <- rowSums(ucla_recode(df[, paste0("Q66.", 1:3, "_2025")]))   # 3–12
```

---

## 5. Covariates (C) — 39, all baseline 2024 (pre-exposure)

All at the **2024 wave** (strictly pre-exposure), so C precedes the 2024→2025 exposure and the 2025 outcome — a baseline-adjusted (lagged-outcome) design. `baseline_ucla3` is in C (baseline loneliness; ceiling concern makes it a confounder, not a moderator). All three A_p purpose composites are always in the model together (cross-purpose adjustment). A source variable is removed from C **only when keeping it would duplicate the moderator** — which depends on whether the moderator is the variable itself or a coarsening of it:

- **Categorical scan (categorical M).** `age_band` / `friends_iso` / `family_iso` are *coarsenings* of continuous variables, so their continuous source (`age_2024`, `baseline_lsns6_friends`, `baseline_lsns6_family`) **stays in C** as a finer within-stratum confounder — the band/threshold moderates while the continuous form confounds (the LSNS isolation indicators thus follow the **age convention**, §3; collinearity VIF-checked in D8). The only categorical-scan removal is `sex_female`, dropped when `sex_cat` is the moderator (a binary has no continuous counterpart → perfect collinearity).
- **Spline scan (continuous M).** Here the continuous variable *itself* is the moderator (`baseline_lsns6_friends_c`, `age_2024_c`, …), so it is removed from C — it cannot be both M and confounder.

So **LSNS continuous stays in the categorical subgroup models** (like age) and is dropped only where LSNS is itself the continuous moderator. `stopifnot(length(C_VARS) == 39L)`.

### 5.1 Demographic & socioeconomic block (15)

| Variable | Q_No (2024) | Coding | Type |
|---|---|---|---|
| `age_2024` | `AGE_2024` | numeric years | continuous |
| `sex_female` | `SEX_2024` | `as.integer(SEX_2024 == 2L)` (ref male) | binary |
| `edu_univ` | `Q21.1_2024` | `%in% 6:8` (university/college) | dummy |
| `edu_grad` | `Q21.1_2024` | `== 9L` (graduate) | dummy |
| _(reference)_ | `Q21.1_2024` | `1:5, 10, 11` | — |
| `emp_exec` | `Q5.1_2024` | `== 1` | dummy |
| `emp_self` | `Q5.1_2024` | `%in% 2:4` | dummy |
| `emp_nonreg` | `Q5.1_2024` | `%in% 7:11` | dummy |
| `emp_student` | `Q5.1_2024` | `%in% 12:13` | dummy |
| `emp_notwork` | `Q5.1_2024` | `%in% 14:16` or NA | dummy |
| _(reference)_ | `Q5.1_2024` | `5, 6` = regular employee | — |
| `income_2_6m` | `Q80.1_2024` | `%in% 5:8` | dummy |
| `income_6_10m` | `Q80.1_2024` | `%in% 9:12` | dummy |
| `income_10m_plus` | `Q80.1_2024` | `%in% 13:18` | dummy |
| `income_unknown` | `Q80.1_2024` | NA or `%in% c(19L, 20L)` | dummy |
| _(reference)_ | `Q80.1_2024` | `1:4` = <2m JPY | — |
| `married` | `Q2_2024` | `%in% 1:3` | binary |
| `living_alone` | `Q1.1_2024` | `== 1L` | binary |

### 5.2 Psychological / social-network block (8)

| Variable | Q_No (2024) | Coding | Scale |
|---|---|---|---|
| `baseline_lsns6_family` | `Q17.1–Q17.3_2024` | sum of `lsns_recode` per item | 0–15 (higher = more connected) |
| `baseline_lsns6_friends` | `Q17.4–Q17.6_2024` | sum of `lsns_recode` per item | 0–15 (higher = more connected) |
| `baseline_ucla3` | `Q66.1–Q66.3_2024` | sum of `ucla_recode` per item | 3–12 (higher = lonelier) |
| `baseline_k6` | `Q65.1–Q65.6_2024` | sum of `k6_recode` per item | 0–24 (higher = more distress) |
| `mental_physical_health` | `Q76.3_2024`, `Q76.4_2024` | `rowMeans(c(Q76.3, Q76.4))` (raw codes) | continuous |
| `ace_1`, `ace_2_3`, `ace_4plus` | `Q77.*_2024` | 4-level categorical (see below) | dummies (ref `ace_0`) |

> **Note**: `baseline_lsns6_friends`/`_family` are the 2024-wave LSNS (Q17) — these double as the *source* of the LSNS moderators (friend/family isolation); the relevant one drops from C when it is the focal moderator (§3).

**ACE recoding** (categorical because ACE is a count of distinct adversities with a non-linear dose-response, not a reflective scale; Felitti 1998):

```r
ace_cols    <- intersect(paste0("Q77.", c(1:8, 13), "_2024"), names(df))
ace_pos_sum <- rowSums(vapply(df[ace_cols], function(v) as.integer(as_num(v) == 1L), integer(nrow(df))), na.rm = TRUE)
q77_9       <- as_num(df$Q77.9_2024); ace_q9 <- as.integer(q77_9 == 2L); ace_q9[is.na(q77_9)] <- 0L
ace_score   <- ace_pos_sum + ace_q9
ace_1     <- as.integer(ace_score == 1L)
ace_2_3   <- as.integer(ace_score %in% 2:3)
ace_4plus <- as.integer(ace_score >= 4L)      # ref = ace_0 (ace_score == 0)
```

### 5.3 Lifestyle / time-use block (16 — categorical)

Four 2024-wave domains, each → 4 dummies against the reference `0–<1 h/day`.

| Raw band (`Q28.x_2024`) | Mapped category |
|---|---|
| 1–3 (none / <30 min / ~30 min) | **0–<1 h (reference)** |
| 4–5 (1 h / 2 h) | **1–2 h** (`_band_1_2`) |
| 6–7 (3 h / 4–5 h) | **3–4 h** (`_band_3_4`) |
| 8–11 (6–7 / 8–9 / 10–11 / ≥12 h) | **6+ h** (`_band_5plus`; the variable name is historical) |
| 12 (Don't know) or NA | **Unknown** (`_unknown`) |

```r
make_timeuse_dummies <- function(d, raw_col, prefix) {
  v <- as_num(d[[raw_col]])
  d[[paste0(prefix,"_band_1_2")]]   <- as.integer(v %in% c(4L,5L))
  d[[paste0(prefix,"_band_3_4")]]   <- as.integer(v %in% c(6L,7L))
  d[[paste0(prefix,"_band_5plus")]] <- as.integer(v %in% 8:11)
  d[[paste0(prefix,"_unknown")]]    <- as.integer(is.na(v) | v == 12L)
  d
}
# Smartphone Q28.13, PC/tablet Q28.14, sitting Q28.5, walking Q28.6 → 4 × 4 = 16 dummies
```

### 5.4 C_VARS — full vector + length assertion

```r
C_VARS <- c(
  # demographic & SES (15)
  "age_2024","sex_female","edu_univ","edu_grad",
  "emp_exec","emp_self","emp_nonreg","emp_student","emp_notwork",
  "income_2_6m","income_6_10m","income_10m_plus","income_unknown","married","living_alone",
  # psychological / social-network (8)
  "baseline_lsns6_family","baseline_lsns6_friends","baseline_ucla3","baseline_k6",
  "ace_1","ace_2_3","ace_4plus","mental_physical_health",
  # time-use dummies (16)
  "smartphone_band_1_2","smartphone_band_3_4","smartphone_band_5plus","smartphone_unknown",
  "pc_tablet_band_1_2","pc_tablet_band_3_4","pc_tablet_band_5plus","pc_tablet_unknown",
  "sitting_band_1_2","sitting_band_3_4","sitting_band_5plus","sitting_unknown",
  "walking_band_1_2","walking_band_3_4","walking_band_5plus","walking_unknown")
stopifnot(length(C_VARS) == 39L)
# per-moderator: a categorical M keeps its continuous source in C (coarsening; age/LSNS);
# only sex_female (binary, no continuous form) or a directly-used continuous M is removed
```

**Total: 15 + 8 + 16 = 39.**

---

## 6. Analysis framework

The study's **primary** analysis is the A_SE main effect (6.1). Everything else — the moderator-wide scans (categorical / spline) and their sensitivity analyses — is **exploratory** (see §0.1). Sensitivity analyses of the primary main effect are in 6.1 and 6.6.

### 6.0 Engine overview

All moderation components fit **one joint model per moderator** `M` (covariate set `cv` = C, minus only a source variable that would *duplicate* M — i.e. `sex_female` for `sex_cat`, or a continuous variable used directly as M in the Spline scan; a categorical M's continuous source is **retained** as a confounder, see §5):

```r
Y_ucla3 ~ A_SE_c + A_PC_c + A_DI_c + M + A_SE_c:M + A_PC_c:M + A_DI_c:M + cv   (linear OLS)
```

Each component is a variant of this form: categorical M (categorical scan, 6.2), spline M (spline scan, 6.5), or this scan re-run with categorical A (6.3) / restricted to users (6.4).

### 6.1 PRIMARY — main effects (`mwinit_overall.csv`)

The **primary analysis**: the unmoderated 3-purpose model (no interactions):

```r
Y_ucla3 ~ A_SE_c + A_PC_c + A_DI_c + C(39)   (linear OLS)
```

The **A_SE coefficient is the PRIMARY** estimate. A_PC/A_DI main effects are secondary. A single hypothesis → no multiple-testing correction. β is per one unit of the composite (one step of the frequency scale); `sd_A` (cohort SD of the composite), `beta_per_sd`, `pct_zero`, `sd_Y`, `r2` are written alongside for the Table 2 footnote. `mwinit_overall_full_coefficients.csv` holds every coefficient (→ Sup Table 6 with the VIFs from D8).

**Sensitivity analyses of the primary main effect (→ Table 2 blocks and Sup Table 7).** The A_SE main effect is probed under six variants of the *unmoderated* model:

- **Sensitivity analysis 1 (SENS1) — categorical exposure** (`mwinit_cat_overall.csv`; §2.1): all three purposes as 0/T1/T2/T3 factors + `ai_init` → contrasts vs never-users and vs AI users without the purpose. The A_SE block is also shown in Table 2.
- **Sensitivity analysis 2 (SENS2) — users only** (`mwinit_overall_init.csv`): re-fit on `ai_init==1` (n = 2,490) with exposures re-centred. Per unit (same scale as the primary; users' SDs written for reference). Also shown in Table 2.
- **Sensitivity analysis 3 (SENS3) — the two S/E items** (`mwinit_overall_items.csv`): casual conversation and emotional support entered alone and together.
- **Sensitivity analysis 4 (SENS4) — overlap weighting** (`mwinit_overall_overlap_weighted.csv`; 6.6).
- **Sensitivity analysis 5 (SENS5) — inverse-probability-of-retention weighting** (`mwinit_overall_attrition_weighted.csv`; 6.6).
- **Sensitivity analysis 6 (SENS6) — baseline before 2025** (`mwinit_overall_pre2025_baseline.csv`; 6.6).

### 6.2 Categorical scan — continuous A × categorical M

For each of the 4 categorical moderators, fit the joint model (6.0); then **per purpose** p ∈ {A_SE, A_PC, A_DI}:

1. **Stratified slopes** — for each level L of M, the p→Y slope = `β_{p_c} + β_{p_c:M=L}` via a contrast vector against `coef`/`vcov`, with delta-method SE and 95% CI. (This is the *within-group association*, NOT a moderation test.)
2. **Per-category effect-modification contrasts (the moderation test)** — for each non-reference level L, the contrast γ = `β_{p_c:M=L}` = (slope at L − slope at ref) with its own Wald SE/z/raw p. This is the per-category **product term**: it tests whether the p→Y slope at level L differs from the reference level. For a binary M there is one such contrast; for a k-level M there are k−1.

3. **Test of the moderator (the reported effect-modification test)** — the nested-F test of all product terms of purpose p with M (`anova(fit_red, fit)`; 2 df for age, 1 df otherwise), written to `mwinit_interaction.csv` with raw p, **Holm-adjusted p over the 4 moderators within the purpose** (`p_holm`) and BH q (for reference). For a binary M this p equals the single contrast's p.

`mwinit_stratified.csv` collects per-level slopes (`slope`, per unit; plus the per-SD columns for reference); `mwinit_interaction_contrasts.csv` collects the per-non-reference-level effect-modification contrasts (γ, SE, z, raw p) with the moderator-test p / Holm p attached.

#### Scale of the forest (Figure 2)

Figure 2 and Sup Table 12 report **β per one unit of the composite** (`slope`, `ci_lo`, `ci_hi`). The per-SD columns (`beta_std` = β × σ_A etc.) remain in the CSV for reference; the cohort SDs are given in the Table 2 footnote. Y stays in UCLA-3 units.

### 6.3 Categorical-exposure scan (effect-modification sensitivity analysis 1)

Mirrors the categorical scan with A_p as a 4-level factor (0/T1/T2/T3, see §2.1). Per moderator × purpose: per-level contrasts of (A_level vs 0) at each M stratum (delta-method SE; 95% CI). Dose-response forest plot (`mwinit_cat_forest_<OUTCOME>.{png,pdf}`) shows 0/T1/T2/T3 top-to-bottom within each stratum.

**Product-term (interaction) significance.** For each non-reference M-stratum L and each A-level T_k, the per-cell **product term** is the model interaction coefficient γ = `β_{A=T_k : M=L}` = (T_k-vs-0 effect at M=L) − (T_k-vs-0 effect at the reference stratum), with its own Wald SE/z/raw p. This is the categorical-A analog of the categorical-scan contrasts (§6.2 #2): it tests whether the dose effect *differs across strata* (the moderation), distinct from the within-stratum dose effect in `_cat_stratified`. γ is in UCLA-3 units (no per-SD standardization — A is categorical).

### 6.4 Users-only moderation (effect-modification sensitivity analysis 2)

Re-run the categorical scan on `ai_init==1` (n = 2,490), re-centring exposures within users. Tests whether full-cohort moderation survives when restricted to users. Outputs suffixed `_init` (F test of the moderator with Holm p; γ contrasts; stratified slopes per unit); forest figure `mwinit_forest_init_<OUTCOME>.{png,pdf}` on the same per-unit scale as Figure 2 → Sup Table 14, Sup Fig 6.

### 6.5 Spline scan — marginal associations across continuous moderators (Figure 1)

**Manuscript role:** **Figure 1** is the all-purposes spline grid (3 purposes × 3 moderators; moderators as rows, purposes as columns, coloured by purpose). The 2-df tests go to **Sup Table 10** and the percentile table to **Sup Table 11**; the predicted-loneliness figure is **Sup Fig 4**.

For each continuous moderator M ∈ {LSNS friends, LSNS family, age}, fit **one joint model**:

```r
Y_ucla3 ~ (A_SE_c + A_PC_c + A_DI_c) * rcs(M_c, 3) + Cmat   (gaussian GLM)
```

3-knot RCS; default knot placement at quantiles 0.10/0.50/0.90 of M_c. `interactionRCS::intEST(var1 = A_p_c, var2 = M_c, model = m)` extracts each purpose's AME of A_p_c across M_c at:

- **61-point grid** over the full moderator range (LSNS-6 subscales 0–15; age over the observed range) → `mwinit_spline_AME.csv` + figure.
- **5 percentiles** {p10, p25, p50, p75, p90} of M_c → `mwinit_spline_AME_quantiles.csv` (→ **Sup Table 11**).
- **Tests** — Test 1 drops both A_p × spline-basis terms (2 df; null hypothesis: the association of A_p with Y is constant across M); Test 2 drops only the nonlinear term (1 df). Sex (1-df product term from the categorical scan) is added so that each purpose has **4 tests, Holm-adjusted** (`mwinit_spline_moderation_tests.csv`, `overall_p_holm`) → **Sup Table 10**.

Knots: `rms::rcs(M_c, 3)` places the 3 knots at the 10th, 50th and 90th percentiles of M (Harrell).

**Cmat bundle**: confounders enter as a single matrix column (`d2$Cmat <- as.matrix(d2[, cv])`) to sidestep `intEST`'s "condition has length > 1" error on R 4.x with many individually-named predictors.

**Why joint**: the categorical scan includes all 3 A_p × M interactions in one model per moderator; the spline scan mirrors this for consistency. Each A_p's spline-AME is computed from the same joint model — its partial derivative ∂Y/∂A_p_c involves only A_p's main + A_p × spline-basis terms; other purposes' splines drop out of that derivative but their parameters are still in the fit (cross-purpose adjustment is more honest than focal-only + linear-others).

### 6.6 Further sensitivity analyses of the primary main effect (§5 of the script)

- **Sensitivity analysis 4 — overlap weighting.** Propensity model: logistic regression of `A_SE_any` on the 39 covariates with the six continuous covariates (age, K6, baseline UCLA-3, LSNS friends, LSNS family, health) also entered as 3-knot restricted cubic splines. Overlap weights (exposed 1 − PS, unexposed PS; Li, Morgan & Zaslavsky 2018) give exact mean balance on every covariate of the propensity model without trimming; the estimand is the association in the population with overlapping covariate profiles. Three models: unweighted covariate-adjusted, overlap-weighted, overlap-weighted + covariate-adjusted (`A_PC_c`, `A_DI_c` in all). Diagnostics: `mwinit_ow_balance.csv` (SMD before/after), `mwinit_ow_weight_summary.csv` (ESS, max |SMD|), `mwinit_ow_propensity_summary.csv` (PS distribution by group, AUC) and the two figures.
- **Sensitivity analysis 5 — inverse-probability-of-retention weighting.** Retention (two-wave vs lost) modelled by logistic regression on the 39 covariates among all valid 2024 respondents (`jacsis_2024_all.csv`, linked by `Monitor_ID`); stabilised inverse-probability-of-retention weights, trimmed at the 99th percentile, applied to the primary model (`mwinit_ipaw_weight_summary.csv`).
- **Sensitivity analysis 6 — baseline before 2025.** The primary model refitted after excluding respondents whose baseline was completed on or after 1 January 2025 (`mwinit_baseline_timing.csv` gives the counts).
- **E-values.** VanderWeele & Ding (2017) for a continuous outcome: the estimate and its CI limit are converted to a standardised difference d = β/SD(Y) and to an approximate risk ratio RR = exp(0.91 d); E = RR + √(RR(RR − 1)). Reported for the primary estimate per unit, per SD, for any-use vs none, and for T3 vs never-users.

The two weighted models use `sandwich::vcovHC(type = "HC3")` (robust) standard errors with normal-approximation CIs; all other models use model-based OLS errors and t-based CIs.

---

## 7. Diagnostics (`diagnosis.r`)

Fifteen diagnostics → `output/ai_mod/diagnosis/`. Produces `flags_<ts>.md` for triage:

| # | Diagnostic | Output |
|---|---|---|
| D1 | Cohort flow (per-code exclusion + initiated/never) | `01_cohort_flow.csv` |
| D2 | Missingness (full cohort + initiator-only) | `02_missingness.csv` |
| D3 | Variable construction sanity | `03_variable_sanity.csv` |
| D4 | Cronbach's α (3 A composites + UCLA-3 + K6 + LSNS + ACE) | `04_cronbach_alpha.csv` |
| D5 | Moderator strata × ai_init cross-tab | `05_moderator_distribution.csv` |
| D6 | A descriptive + crude A→Y (full + intensive scopes) | `06_A_descriptive.csv` |
| D7 | Cutpoint feasibility for the categorical exposure (tertile → median → binary fallback) | `07_A_cutpoints.csv` |
| D8 | VIF on three categorical-scan fits (main effects + friends_iso + income), uncentred AND Aiken & West centred | `08_vif_*.csv` |
| D9 | Straight-lining on the 9 Q37S3 A_p items among initiators | `09_straightlining.csv` |
| D10 | **EFA of the 9 Q37S3 purpose items** (users; parallel analysis + oblique minres EFA; §2.2) — polychoric and Pearson → **Sup Table 1** | `10_efa_purposes_{loadings,variance,factor_cor,fit}.csv`, `10_efa_purposes_poly_*.csv` |
| D11 | **CFA holdout** — three-factor vs one-factor, pre-2025 initiators, WLSMV → Sup Table 1C | `11_cfa_holdout_{fit,loadings,factor_cor}.csv` |
| D12 | Exposure distribution: share of zeros per composite, any use among users / all current users; distinct values | `12_exposure_distribution.csv`, `12_exposure_values_initiators.csv` |
| D13 | Baseline characteristics by AI-use history (Q37S1 codes; whole two-wave panel; the Table 1 rows) with SMD vs never-users → Sup Table 4 | `13_characteristics_by_ai_use_history.csv` |
| D14 | Attrition: retained vs lost among all valid 2024 respondents on the 39 covariates (SMD); logistic retention model (OR, AUC) → Sup Table 5 | `14_attrition_{comparison,retention_model,summary}.csv` |
| D15 | Baseline completion dates (JST) by group; overlap with the earlier analytic sample (`RI_ID_2024`) | `15_baseline_dates.csv`, `15_overlap_with_earlier_sample.csv` |

**Centred VIFs (D8c)**: product terms of binary moderator indicators with the centred continuous exposure inflate the uncentred VIFs (non-essential multicollinearity); after mean-centring the moderator dummies (Aiken & West 1991) the interaction VIFs are below 10 and the A_p main-effect VIFs below 3. The per-stratum slope contrasts are unaffected either way.

---

## 8. Manuscript packaging (`manuscript_tables.r`)

Pure downstream CSV reader. Reads from `output/ai_mod/modwide_init/tables/` and writes publication-ready outputs to `output/ai_mod/manuscript/{tables,figures,logs}/`. Does NOT re-estimate anything.

Manuscript structure: **A_SE-focused main exhibits; everything else in the supplement.** Numbering follows citation order in the manuscript.

**Main exhibits**

| Element | Source | Output |
|---|---|---|
| **Table 1** characteristics by total + **A_SE** category (never-users / AI users, no use / T1 / T2 / T3) + SMD (any use vs never-users) | raw cohort + `A_SE` 5-level factor | `table1_characteristics_A_SE.csv` |
| **Table 2** main effects of 3 purposes (continuous, per unit) + sensitivity analysis 1 five-category block (A_SE) + sensitivity analysis 2 users-only block; footnote statistics (n, R², SD, per-SD β, % zero) | `mwinit_overall.csv`, `mwinit_cat_overall.csv`, `mwinit_overall_init.csv` | `table2_main_effects.csv`, `table2_footnote_statistics.csv`, `table2_tertile_cutpoints.csv` |
| **Figure 1** spline marginal associations, **all 3 purposes × 3 moderators** — moderators rows × purposes columns | packager copies `mwinit_spline_AME_<OUTCOME>` | `figure1_spline_AME.{png,pdf}` |
| **Figure 2** moderation forest, **all 3 purposes** — moderators as header rows, purpose as columns (per unit) | packager copies `mwinit_forest_<OUTCOME>` | `figure2_primary_forest.{png,pdf}` |

**Supplement — tables**

| Element | Source | Output |
|---|---|---|
| **Sup Table 1** EFA of the 9 AI-use purpose items (users) — polychoric and Pearson pattern loadings + h² + factor-correlation Φ + fit; CFA holdout (fit, loadings, factor correlations) | `diagnosis/10_efa_purposes*_*.csv`, `11_cfa_holdout_*.csv` | `sup_table1_efa_purposes*.csv`, `sup_table1_cfa_holdout_*.csv` |
| **Sup Table 2** characteristics by total + **A_PC** category (same layout as Table 1) | raw cohort + `A_PC` factor | `sup_table2_characteristics_A_PC.csv` |
| **Sup Table 3** characteristics by total + **A_DI** category | raw cohort + `A_DI` factor | `sup_table3_characteristics_A_DI.csv` |
| **Sup Table 4** baseline characteristics by AI-use history (two-wave panel) | `diagnosis/13_*.csv` | `sup_table4_characteristics_by_ai_use_history.csv` |
| **Sup Table 5** attrition: retained vs lost; retention model | `diagnosis/14_*.csv` | `sup_table5_attrition_*.csv` |
| **Sup Table 6** full coefficients of the primary model + VIF; fit statistics | `mwinit_overall_full_coefficients.csv`, `diagnosis/08_vif_main_effects.csv` | `sup_table6_full_coefficients_primary_model.csv`, `sup_table6_fit_statistics_primary_model.csv` |
| **Sup Table 7** main effects — main model + sensitivity analyses 1–6 with n and R² | `mwinit_overall*`, `mwinit_cat_overall` | `sup_table7_main_effects_sensitivities.csv` (+ `_baseline_timing`, `_ipaw_weight_summary`) |
| **Sup Table 8** overlap weighting: balance, weight summary, propensity summary | `mwinit_ow_*.csv` | `sup_table8_overlap_*.csv` |
| **Sup Table 9** E-values | `mwinit_evalues.csv` | `sup_table9_evalues.csv` |
| **Sup Table 10** tests of effect modification (categorical and spline; Holm) | `mwinit_interaction.csv`, `mwinit_spline_moderation_tests.csv` | `sup_table10_effect_modification_tests.csv` |
| **Sup Table 11** spline marginal associations at percentiles | `mwinit_spline_AME_quantiles.csv` | `sup_table11_spline_quantiles.csv` |
| **Sup Table 12** subgroup, all purposes (A continuous) [categorical scan] — stratified slopes per unit + γ contrasts (raw p) + F test with Holm p | `mwinit_stratified.csv` + `mwinit_interaction_contrasts.csv` + `mwinit_interaction.csv` | `sup_table12_subgroup_primary.csv` + `_primary_contrasts.csv` + `_primary_omnibus_tests.csv` |
| **Sup Table 13** LSNS-6 threshold sweep (effect-modification sensitivity analysis 3) | `mwinit_lsns_threshold_sweep.csv` | `sup_table13_lsns_thresholds.csv` |
| **Sup Table 14** users-only subgroup analysis (effect-modification sensitivity analysis 2) | `mwinit_*_init.csv` | `sup_table14_*.csv` |

**Supplement — figures**

| Element | Source | Output |
|---|---|---|
| **Sup Figure 1** flow chart | `diagnosis/01_cohort_flow.csv` (→ `sup_figure1_flow_counts.csv`) | drawn separately |
| **Sup Figure 2** covariate balance before/after overlap weighting | `mwinit_ow_balance` | `sup_figure2_overlap_balance.{png,pdf}` |
| **Sup Figure 3** propensity-score overlap | `mwinit_ow_propensity_overlap` | `sup_figure3_propensity_overlap.{png,pdf}` |
| **Sup Figure 4** predicted loneliness, all purposes | `mwinit_spline_predicted_<OUTCOME>` | `sup_figure4_predicted.{png,pdf}` |
| **Sup Figure 5** dose-response forest with the categorical exposure (effect-modification sensitivity analysis 1) | `mwinit_cat_forest_<OUTCOME>` | `sup_figure5_categorical_exposure.{png,pdf}` |
| **Sup Figure 6** users-only forest (effect-modification sensitivity analysis 2) | `mwinit_forest_init_<OUTCOME>` | `sup_figure6_users_only.{png,pdf}` |

**Final exhibit set:** Table 1 · Table 2 · Figure 1 (spline grid) · Figure 2 (moderation forest); **Sup Tables 1–14**; **Sup Figures 1–6**.

The final publication tables/figures are assembled from these CSVs.
