# ============================================================
# Social and emotional use of general-purpose AI and loneliness:
# main effects, effect modification and sensitivity analyses
# ============================================================
# Cohort   : respondents who completed the 2024 and 2025 waves of JACSIS and were
#            never-users of generative AI or 2025 initiators (Q37S1_2025 in {1, 5, 6}).
# Exposure : three AI-use purpose composites measured in 2025 (A_SE social/emotional,
#            A_PC productivity/creative, A_DI daily/information), item mean minus 1
#            (0-4; one unit = one step of the frequency scale); never-users = 0.
# Outcome  : Y_ucla3, the 2025 UCLA-3 score (items 1-4 summed; 3-12, higher = lonelier).
# C        : 39 covariates measured at the 2024 baseline, including baseline UCLA-3
#            (lagged-outcome specification).
#
# Analyses (section numbers are used in the log and in the comments below):
#   Primary main effect  the A_SE coefficient of the unmoderated three-purpose model
#                        (mwinit_overall), per one unit of the composite; conventional
#                        (model-based) OLS standard errors.
#   §1  Categorical moderators  age band, sex, friend isolation, family isolation:
#       one joint model per moderator (all three purposes x moderator); stratum
#       slopes, stratum contrasts and the F test of the moderator's product terms,
#       Holm-adjusted over the four moderators within each purpose (Figure 2,
#       Supplementary Tables 10 and 12); alternative LSNS-6 thresholds <3, <9, <12
#       (effect-modification sensitivity analysis 3; Supplementary Table 13).
#   §2  Continuous moderators  age and the LSNS-6 friend and family subscales as
#       restricted cubic splines (3 knots) interacted with each purpose (rms +
#       interactionRCS): marginal associations over the full moderator range,
#       at five percentiles, predicted loneliness, and the 2-df spline tests,
#       Holm-adjusted with the sex test (Figure 1, Supplementary Figure 4,
#       Supplementary Tables 10 and 11).
#   §3  Users only (sensitivity analysis 2; effect-modification sensitivity analysis 2):
#       main effects and the §1 scan restricted to the 2,490 initiators
#       (Supplementary Figure 6, Supplementary Table 14).
#   §4  Categorical exposure (sensitivity analysis 1; effect-modification sensitivity
#       analysis 1): all three purposes as 0/T1/T2/T3 factors with an indicator of
#       generative-AI use (Table 2, Supplementary Table 7, Supplementary Figure 5).
#   §5  Further sensitivity analyses of the primary main effect: the two
#       social/emotional items (3), overlap weighting for any social/emotional use
#       with balance and positivity diagnostics (4), inverse-probability-of-retention
#       weighting (5), baseline completed before 1 January 2025 (6), and E-values
#       (Supplementary Tables 7-9, Supplementary Figures 2-3). The two weighted
#       models use robust (sandwich, HC3) standard errors because their weights
#       are estimated.
#
# Closed-form OLS with delta-method standard errors throughout; no bootstrap.
# Inputs : data/jacsis_2wave2425.csv (two-wave file) and, optionally,
#          data/jacsis_2024_all.csv (all valid 2024 respondents, for the attrition analysis);
#          both paths can be overridden with the environment variables named below.
# Outputs: output/ai_mod/modwide_init/{tables,figures,logs}/, prefix mwinit_.
# Variables and estimators are documented in CODEBOOK.md.

suppressPackageStartupMessages({ library(here); library(readr) })
set.seed(20260524)

# ---- boilerplate (paths, mode, logging) ----
find_proj_root <- function() {
  candidates <- character(0); ch <- tryCatch(here::here(), error = function(e) NA_character_)
  if (!is.na(ch)) candidates <- c(candidates, ch)
  args <- commandArgs(trailingOnly = FALSE); fa <- args[grepl("^--file=", args)]
  if (length(fa)) { sd <- tryCatch(normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/"), error = function(e) NA_character_); if (!is.na(sd)) candidates <- c(candidates, sd, dirname(sd)) }
  candidates <- c(candidates, getwd(), dirname(getwd()))
  for (cand in unique(candidates)) { if (!nzchar(cand)) next
    if (basename(cand) == "r_code" && dir.exists(file.path(cand, "data"))) return(normalizePath(cand, winslash = "/"))
    if (dir.exists(file.path(cand, "r_code", "data"))) return(normalizePath(file.path(cand, "r_code"), winslash = "/")) }
  stop("Could not find r_code/")
}
.proj_root <- find_proj_root(); .norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
DATA_PATH <- Sys.getenv("JACSIS_2WAVE_2425_PATH", unset = ""); if (!nzchar(DATA_PATH)) DATA_PATH <- file.path(.proj_root, "data", "jacsis_2wave2425.csv")
DATA_PATH <- .norm(DATA_PATH); if (!file.exists(DATA_PATH)) stop("Input CSV not found: ", DATA_PATH)
# Optional second input: ALL valid 2024 respondents (retained + lost), for the attrition
# analysis and the inverse-probability-of-retention weights. Linked to the two-wave file by Monitor_ID.
DATA_2024_ALL_PATH <- Sys.getenv("JACSIS_2024_ALL_PATH", unset = ""); if (!nzchar(DATA_2024_ALL_PATH)) DATA_2024_ALL_PATH <- file.path(.proj_root, "data", "jacsis_2024_all.csv")
DATA_2024_ALL_PATH <- .norm(DATA_2024_ALL_PATH)
ID_COL <- "Monitor_ID"                                   # links the two-wave file to the all-2024 file
BASELINE_TS_COL <- "回答完了日時_2024"   # 回答完了日時_2024: baseline completion timestamp (UTC, ISO 8601)
OUTCOME <- "Y_ucla3"; PIPELINE <- "ai_mod"
OUT_ROOT <- file.path(.proj_root, "output", PIPELINE, "modwide_init")
TABLES_DIR <- file.path(OUT_ROOT, "tables"); FIGS_DIR <- file.path(OUT_ROOT, "figures"); LOGS_DIR <- file.path(OUT_ROOT, "logs")
for (d in c(TABLES_DIR, FIGS_DIR, LOGS_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
.timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S"); LOG_FILE <- file.path(LOGS_DIR, sprintf("run_modwideinit_%s.log", .timestamp))
log_msg <- function(...) { m <- paste0(format(Sys.time(), "[%H:%M:%S] "), paste(..., collapse = " ")); cat(m, "\n", sep=""); cat(m, "\n", sep="", file=LOG_FILE, append=TRUE); invisible(m) }
write_table <- function(x, name) { fp <- file.path(TABLES_DIR, paste0(name, ".csv")); utils::write.csv(x, fp, row.names = FALSE); log_msg("wrote:", fp) }
log_msg("=== START (modwide_init) ===  input:", DATA_PATH)

# ---- build (Y + 39 C + ai_init; cohort {1,5,6}) ----
.as_num <- function(x) suppressWarnings(as.numeric(x))
.row_mean <- function(d, cols, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowMeans(vapply(d[cols], .as_num, numeric(nrow(d))), na.rm = na_rm) }
.row_sum_fn <- function(d, cols, fn = identity, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowSums(vapply(d[cols], function(v) fn(.as_num(v)), numeric(nrow(d))), na.rm = na_rm) }   # v1: default FALSE (strict) — matches .row_mean; item-missing -> NA -> dropped, fully-missing scale -> NA (not spurious 0)
.ucla_recode <- function(M) pmax(1, pmin(4, 5 - M)); .k6_recode <- function(M) pmax(0, pmin(4, 5 - M)); .lsns_recode <- function(M) pmax(0, pmin(5, M - 1L))   # UCLA-3 items scored 1-4 (sum 3-12)
.td <- function(d, raw, p) { v <- .as_num(d[[raw]]); d[[paste0(p,"_band_1_2")]] <- as.integer(v %in% c(4L,5L)); d[[paste0(p,"_band_3_4")]] <- as.integer(v %in% c(6L,7L)); d[[paste0(p,"_band_5plus")]] <- as.integer(v %in% c(8:11)); d[[paste0(p,"_unknown")]] <- as.integer(is.na(v) | v==12L); d }
df <- readr::read_csv(DATA_PATH, show_col_types = FALSE); log_msg(sprintf("Loaded: %d rows × %d cols", nrow(df), ncol(df)))
.safe <- function(cn) if (cn %in% names(df)) .as_num(df[[cn]]) else rep(NA_real_, nrow(df))
df$Y_ucla3 <- .row_sum_fn(df, paste0("Q66.",1:3,"_2025"), fn=.ucla_recode)
ai_start <- .safe("Q37S1_2025"); df$ai_init <- as.integer(ai_start %in% c(5L,6L))
# EXPOSURE (clarification): the 3 purpose frequencies, with NON-INITIATORS = 0.
# Initiators get composite mean − 1 (0–4); never-users (code 1) get 0 for all purposes.
A_SPEC <- list(A_SE=paste0("Q37S3.",c(8,9),"_2025"), A_PC=paste0("Q37S3.",c(1,2,4,5),"_2025"), A_DI=paste0("Q37S3.",c(3,6,7),"_2025"))
A_VARS <- names(A_SPEC); A_CONT_VARS <- paste0(A_VARS,"_continuous"); A_CENT_VARS <- paste0(A_VARS,"_c")
for (a in A_VARS) { v <- .row_mean(df, A_SPEC[[a]], na_rm = FALSE) - 1; v[df$ai_init == 0L] <- 0; df[[paste0(a,"_continuous")]] <- v }
# the two social/emotional items separately (same 0-4 coding; never-users 0) and the any-use indicator
df$A_SE_conv <- .safe("Q37S3.8_2025") - 1; df$A_SE_conv[df$ai_init == 0L] <- 0
df$A_SE_emo  <- .safe("Q37S3.9_2025") - 1; df$A_SE_emo[df$ai_init == 0L]  <- 0
df$A_SE_any  <- as.integer(is.finite(df$A_SE_continuous) & df$A_SE_continuous > 0)
# baseline completion date in Japan Standard Time (the file stores UTC) → flag for baselines on/after 1 Jan 2025
.ts_col <- if (BASELINE_TS_COL %in% names(df)) BASELINE_TS_COL else grep("完了日時.*2024", names(df), value = TRUE, useBytes = TRUE)[1]
# readr already parses ISO-8601 timestamps to POSIXct (UTC) on read, so the column is used as is; a character
# column is parsed with explicit formats (as.POSIXct() without a format silently truncates every value to the
# date when any value falls exactly on midnight).
.parse_ts <- function(x) { if (inherits(x, "POSIXt")) return(as.POSIXct(x)); x <- trimws(as.character(x)); ok <- !is.na(x) & nzchar(x)
  for (fmt in c("%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d")) {
    ts <- suppressWarnings(as.POSIXct(x, format = fmt, tz = "UTC")); if (any(ok) && mean(!is.na(ts[ok])) > 0.5) return(ts) }
  as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC") }
if (!is.na(.ts_col)) { .ts <- .parse_ts(df[[.ts_col]]); df$baseline_date <- as.Date(format(.ts, tz = "Asia/Tokyo", "%Y-%m-%d")) } else df$baseline_date <- as.Date(NA)
df$baseline_after_2025 <- as.integer(!is.na(df$baseline_date) & df$baseline_date >= as.Date("2025-01-01"))
log_msg(sprintf("baseline timestamp column: %s; %d of %d rows dated; %d (%.1f%%) completed on/after 2025-01-01 JST",
                if (is.na(.ts_col)) "NOT FOUND" else .ts_col, sum(!is.na(df$baseline_date)), nrow(df), sum(df$baseline_after_2025), 100 * mean(df$baseline_after_2025)))
df$baseline_ucla3 <- .row_sum_fn(df, paste0("Q66.",1:3,"_2024"), fn=.ucla_recode)
df$baseline_k6 <- .row_sum_fn(df, paste0("Q65.",1:6,"_2024"), fn=.k6_recode)
ace_cols <- intersect(paste0("Q77.",c(1:8,13),"_2024"), names(df))
if (length(ace_cols)) { ap <- vapply(df[ace_cols], function(v) as.integer(.as_num(v)==1L), integer(nrow(df))); aps <- rowSums(ap, na.rm=TRUE)   # v1: ACE intentionally na.rm=TRUE — count of endorsements (missing item = not-endorsed), NOT a scale sum like UCLA/K6/LSNS
  if ("Q77.9_2024" %in% names(df)) { q9 <- .as_num(df$Q77.9_2024); a9 <- as.integer(q9==2L); a9[is.na(q9)] <- 0L } else a9 <- 0L; df$ace_score <- aps + a9 } else df$ace_score <- NA_real_
df$ace_0 <- as.integer(df$ace_score==0L); df$ace_1 <- as.integer(df$ace_score==1L); df$ace_2_3 <- as.integer(df$ace_score %in% 2:3); df$ace_4plus <- as.integer(df$ace_score>=4L)   # ACE categories (ace_0 = reference)
df$age_2024 <- .safe("AGE_2024"); df$sex_female <- as.integer(.safe("SEX_2024")==2L)
edu24 <- .safe("Q21.1_2024"); df$edu_univ <- as.integer(edu24 %in% 6:8); df$edu_grad <- as.integer(edu24==9L)
emp24 <- .safe("Q5.1_2024"); df$emp_exec <- as.integer(emp24==1L); df$emp_self <- as.integer(emp24 %in% 2:4); df$emp_nonreg <- as.integer(emp24 %in% 7:11); df$emp_student <- as.integer(emp24 %in% 12:13); df$emp_notwork <- as.integer(emp24 %in% 14:16 | is.na(emp24))
inc <- .safe("Q80.1_2024"); df$income_2_6m <- as.integer(inc %in% 5:8); df$income_6_10m <- as.integer(inc %in% 9:12); df$income_10m_plus <- as.integer(inc %in% 13:18); df$income_unknown <- as.integer(is.na(inc) | inc %in% c(19L,20L))
df$married <- as.integer(.safe("Q2_2024") %in% 1:3); liv <- .safe("Q1.1_2024"); df$living_alone <- as.integer(!is.na(liv) & liv==1L)
df$baseline_lsns6_family <- .row_sum_fn(df, paste0("Q17.",1:3,"_2024"), fn=.lsns_recode); df$baseline_lsns6_friends <- .row_sum_fn(df, paste0("Q17.",4:6,"_2024"), fn=.lsns_recode)
df$mental_physical_health <- .row_mean(df, c("Q76.3_2024","Q76.4_2024"))
df <- .td(df,"Q28.13_2024","smartphone"); df <- .td(df,"Q28.14_2024","pc_tablet"); df <- .td(df,"Q28.5_2024","sitting"); df <- .td(df,"Q28.6_2024","walking")
C_VARS <- c("age_2024","sex_female","edu_univ","edu_grad","emp_exec","emp_self","emp_nonreg","emp_student","emp_notwork",
  "income_2_6m","income_6_10m","income_10m_plus","income_unknown","married","living_alone",
  "baseline_lsns6_family","baseline_lsns6_friends","baseline_ucla3","baseline_k6","ace_1","ace_2_3","ace_4plus","mental_physical_health",
  "smartphone_band_1_2","smartphone_band_3_4","smartphone_band_5plus","smartphone_unknown","pc_tablet_band_1_2","pc_tablet_band_3_4","pc_tablet_band_5plus","pc_tablet_unknown",
  "sitting_band_1_2","sitting_band_3_4","sitting_band_5plus","sitting_unknown","walking_band_1_2","walking_band_3_4","walking_band_5plus","walking_unknown")
stopifnot(length(C_VARS) == 39L)
ds <- df[ai_start %in% c(1L,5L,6L), , drop = FALSE]
dm <- ds[complete.cases(ds[, c("Y_ucla3", A_CONT_VARS, C_VARS), drop = FALSE]), , drop = FALSE]
log_msg(sprintf("Cohort {1,5,6} = %d; complete-case = %d  (initiated %d / never %d)", nrow(ds), nrow(dm), sum(dm$ai_init==1L), sum(dm$ai_init==0L)))
stopifnot(nrow(dm) >= 3000L)
for (a in A_CONT_VARS) dm[[sub("_continuous$","_c",a)]] <- dm[[a]] - mean(dm[[a]])   # center exposures on the {1,5,6} sample
write_table(data.frame(step=c("raw","cohort_1_5_6","complete_case","initiated","never"),
                       n=c(nrow(df), nrow(ds), nrow(dm), sum(dm$ai_init==1L), sum(dm$ai_init==0L))), "mwinit_sample_flow")

# ---- moderator factors ----
dm$age_band <- factor(ifelse(dm$age_2024<=39,"<=39", ifelse(dm$age_2024<=64,"40-64","65+")), levels=c("<=39","40-64","65+"))
dm$sex_cat  <- factor(ifelse(dm$sex_female==1L,"Female","Male"), levels=c("Male","Female"))
dm$edu_cat  <- factor(ifelse(dm$edu_grad==1L,"Graduate", ifelse(dm$edu_univ==1L,"University","Below univ")), levels=c("Below univ","University","Graduate"))
inc2 <- rep(NA_character_, nrow(dm)); inc2[dm$income_2_6m==1L] <- "2-6m"; inc2[dm$income_6_10m==1L] <- "6-10m"; inc2[dm$income_10m_plus==1L] <- "10m+"; inc2[is.na(inc2) & dm$income_unknown==0L] <- "<2m"
dm$income_cat <- factor(inc2, levels=c("<2m","2-6m","6-10m","10m+"))   # unknown → NA (dropped)
dm$friends_iso <- factor(ifelse(dm$baseline_lsns6_friends<6,"Isolated","Connected"), levels=c("Connected","Isolated"))
dm$family_iso  <- factor(ifelse(dm$baseline_lsns6_family <6,"Isolated","Connected"), levels=c("Connected","Isolated"))
# continuous LSNS centered + age band / sex, for the §2 LSNS×age/sex moderation check
SOC2 <- c("baseline_lsns6_friends","baseline_lsns6_family"); SOC2_C <- paste0(SOC2, "_c")
for (s in SOC2) dm[[paste0(s, "_c")]] <- dm[[s]] - mean(dm[[s]])
C_SOC <- setdiff(C_VARS, SOC2)   # confounders excl. the 2 LSNS (they are the §2 moderators)

# centred age for the §2 age spline
dm$age_2024_c <- dm$age_2024 - mean(dm$age_2024, na.rm = TRUE)
# alternative LSNS-6 isolation thresholds for the sensitivity sweep (the main analysis uses <6)
LSNS_ALT_CUTS <- c(3, 9, 12)

# ---- run_modwide: one moderator (factor mcol), 3 purposes × M ----
run_modwide <- function(d, mcol, cv) {
  pur <- c("A_SE","A_PC","A_DI"); pc <- paste0(pur, "_c")
  rhs_full <- paste(c(pc, mcol, paste0(pc, ":", mcol), cv), collapse = " + ")
  fit <- lm(as.formula(paste0(OUTCOME, " ~ ", rhs_full)), data = d)
  b <- coef(fit); V <- vcov(fit); nm <- names(b); levs <- levels(droplevels(d[[mcol]])); ref <- levs[1]; n_by <- table(d[[mcol]])
  inter <- list(); strat <- list(); contr <- list()
  for (p in pur) {
    Ac <- paste0(p, "_c")
    rhs_red <- paste(c(pc, mcol, paste0(setdiff(pc, Ac), ":", mcol), cv), collapse = " + ")
    fit_red <- lm(as.formula(paste0(OUTCOME, " ~ ", rhs_red)), data = d)
    aw <- anova(fit_red, fit)
    inter[[length(inter)+1L]] <- data.frame(moderator = mcol, purpose = p, df_num = aw$Df[2], df_den = aw$Res.Df[2],
      F_stat = aw$F[2], p_value = aw[["Pr(>F)"]][2], stringsAsFactors = FALSE)
    for (L in levs) {
      cvec <- rep(0, length(b)); cvec[nm == Ac] <- 1
      if (L != ref) { t <- paste0(Ac, ":", mcol, L); if (t %in% nm) cvec[nm == t] <- 1 }
      sl <- sum(cvec * b); se <- sqrt(max(0, as.numeric(t(cvec) %*% V %*% cvec))); z <- if (se>0) sl/se else NA_real_
      strat[[length(strat)+1L]] <- data.frame(moderator = mcol, purpose = p, level = L, n = as.integer(n_by[[L]]),
        slope = sl, se = se, ci_lo = sl-1.96*se, ci_hi = sl+1.96*se, z = z,
        p_value = if (is.finite(z)) 2*pnorm(-abs(z)) else NA_real_, stringsAsFactors = FALSE)
      # per-category EFFECT-MODIFICATION contrast: γ = A_p_c:M[L] interaction coef = (slope at L) − (slope at ref).
      # Decomposes the joint block-Wald F into per-non-reference-level moderation tests (own SE/z/p).
      # For a binary M the single contrast's p equals the 1-df joint F p; for k-level M there are k−1 contrasts.
      if (L != ref) { tg <- paste0(Ac, ":", mcol, L)
        if (tg %in% nm) { g <- unname(b[tg]); seg <- sqrt(max(0, as.numeric(V[tg, tg]))); zg <- if (seg>0) g/seg else NA_real_
          contr[[length(contr)+1L]] <- data.frame(moderator = mcol, purpose = p, level = L, ref_level = ref,
            n_level = as.integer(n_by[[L]]), gamma = g, se = seg, ci_lo = g-1.96*seg, ci_hi = g+1.96*seg,
            z = zg, p_value = if (is.finite(zg)) 2*pnorm(-abs(zg)) else NA_real_, stringsAsFactors = FALSE) } }
    }
  }
  list(inter = do.call(rbind, inter), strat = do.call(rbind, strat),
       contr = if (length(contr)) do.call(rbind, contr) else NULL)
}
mods <- list(   # the four moderators; Holm adjustment within purpose across the four.
  list(lab="Age band",                              mcol="age_band",        cv=C_VARS),
  list(lab="Sex",                                    mcol="sex_cat",         cv=setdiff(C_VARS,"sex_female")),
  # The continuous LSNS subscales stay in C: the coarsened <6 form moderates; the continuous
  # form adjusts. Mirrors age_band (which keeps age_2024 in C).
  # Sex stays setdiff() because a binary variable has no continuous form to retain.
  list(lab="LSNS friends (isolated <6)",             mcol="friends_iso",     cv=C_VARS),
  list(lab="LSNS family (isolated <6)",              mcol="family_iso",      cv=C_VARS))
# Dropped from focal scan: edu_cat, income_cat, emp_status, k6_serious.
# Source variables (income_*, edu_*, emp_*, baseline_k6) remain in C as confounders.

# Forest stratum order: low→high from TOP→BOTTOM (LSNS Isolated above Connected; matches spline x).
.order_strata <- function(fs) {
  lo <- list(`Age band`=c("<=39","40-64","65+"), `Sex`=c("Male","Female"),
    `LSNS friends (isolated <6)`=c("Isolated","Connected"), `LSNS family (isolated <6)`=c("Isolated","Connected"))
  mo <- vapply(mods, function(M) M$lab, character(1)); ml <- factor(as.character(fs$mod_label), levels = mo)
  k <- 100L*as.integer(ml) + vapply(seq_len(nrow(fs)), function(i){ v <- lo[[as.character(ml[i])]]; li <- match(fs$level[i], v); if (is.na(li)) 99L else as.integer(li) }, integer(1))
  fs[order(k), , drop = FALSE]
}
# Figure labels: purpose and moderator names as in the manuscript; forest moderator sub-headers.
.PUR_LAB <- c(A_SE = "Social–emotional", A_PC = "Productivity–creative", A_DI = "Daily–information")
.MOD_SHORT <- c(`Age band`="Age", `Sex`="Sex", `LSNS friends (isolated <6)`="LSNS friends",
  `LSNS family (isolated <6)`="LSNS family")
.pur_factor <- function(p) factor(unname(.PUR_LAB[as.character(p)]), levels = unname(.PUR_LAB))
# Grouped-forest prep: order low→high top→bottom, add mod_short (sub-header) + clean level label.
.forest_prep <- function(fs) {
  fs <- .order_strata(fs)
  mo <- vapply(mods, function(M) M$lab, character(1))
  fs$mod_short <- factor(unname(.MOD_SHORT[as.character(fs$mod_label)]), levels = unname(.MOD_SHORT[mo]))
  ll <- sprintf("%s (n=%s)", fs$level, fs$n); fs$lvl <- factor(ll, levels = rev(unique(ll)))
  fs$purpose_lab <- .pur_factor(fs$purpose)
  fs
}
# --- Header-row grouped forest ----------------------------------------------
# Instead of boxing each social factor in its own facet panel, lay ALL strata on
# ONE shared y-axis: every moderator name becomes a blank header row (no estimate /
# CI), with its strata indented beneath it (e.g.  **Age** / "   <=39 (n=…)" / …).
# AI-use purpose stays as facet COLUMNS. .forest_positions() returns the strata
# rows tagged with a numeric ypos plus the y break/label map (headers + strata).
.forest_positions <- function(fs, gap = 1L) {
  mo     <- vapply(mods, function(M) M$lab, character(1))
  use_md <- requireNamespace("ggtext", quietly = TRUE)
  ind    <- if (use_md) "  " else "      "               # indent for strata rows
  uk     <- unique(data.frame(mod_label = as.character(fs$mod_label),
                              level = as.character(fs$level), n = fs$n, stringsAsFactors = FALSE))
  recs <- list(); pos <- 0L; first <- TRUE
  for (m in mo) {
    ks <- uk[uk$mod_label == m, , drop = FALSE]
    if (!nrow(ks)) next
    if (!first) pos <- pos + gap                                   # blank gap between groups
    first <- FALSE
    pos <- pos + 1L                                                # header row
    recs[[length(recs) + 1L]] <- data.frame(mod_label = m, level = NA_character_,
      label = if (use_md) sprintf("**%s**", unname(.MOD_SHORT[m])) else unname(.MOD_SHORT[m]),
      pos = pos, stringsAsFactors = FALSE)
    for (i in seq_len(nrow(ks))) {
      pos <- pos + 1L                                             # one indented stratum row
      recs[[length(recs) + 1L]] <- data.frame(mod_label = m, level = ks$level[i],
        label = sprintf("%s%s (n=%s)", ind, ks$level[i], ks$n[i]), pos = pos, stringsAsFactors = FALSE)
    }
  }
  layout <- do.call(rbind, recs)
  layout$ypos <- max(layout$pos) - layout$pos + 1                  # top row → highest y
  fs$ypos <- layout$ypos[match(paste(as.character(fs$mod_label), as.character(fs$level)),
                               paste(layout$mod_label, layout$level))]
  list(df = fs, breaks = layout$ypos, labels = layout$label, use_md = use_md)
}
# Full grouped forest (shared by the §1 scan and the users-only sensitivity analysis).
# Plots the per-unit slope (slope, ci_lo, ci_hi); xlim = NULL shows every estimate and CI.
.forest_grouped <- function(fs, title, subtitle, xlim = NULL,
                            xlab = "Adjusted β per one unit of the composite (95% CI)") {
  LY <- .forest_positions(.forest_prep(fs))
  g <- ggplot2::ggplot(LY$df, ggplot2::aes(slope, ypos, colour = mod_short)) +
    ggplot2::geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
    ggplot2::geom_pointrange(ggplot2::aes(xmin = ci_lo, xmax = ci_hi), linewidth = 0.5, size = 0.35, na.rm = TRUE) +
    ggplot2::facet_grid(. ~ purpose_lab) +
    ggplot2::scale_y_continuous(breaks = LY$breaks, labels = LY$labels, expand = ggplot2::expansion(add = c(0.8, 1.4)))
  if (!is.null(xlim)) g <- g + ggplot2::coord_cartesian(xlim = xlim)
  g +
    ggplot2::labs(title = title, subtitle = subtitle, x = xlab, y = NULL) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(legend.position = "none", panel.grid.major.y = ggplot2::element_blank(),
      axis.text.y = if (LY$use_md) ggtext::element_markdown(hjust = 0) else ggplot2::element_text(hjust = 0))
}

set.seed(20260524)
.n_init <- sum(dm$ai_init == 1L)
.exp_ok <- all(vapply(A_CENT_VARS, function(a) length(unique(dm[[a]])) > 1L, logical(1)))
if (nrow(dm) < 50L || .n_init < 50L || !.exp_ok) {
  log_msg(sprintf("Scan SKIP: n=%d, initiators=%d, exposure-variance=%s.",
                  nrow(dm), .n_init, .exp_ok))
} else {
  log_msg("=== Moderator-wide scan: 3 purposes (non-users=0) → loneliness ===")
  # Overall (unmoderated) 3-purpose main effects for reference
  fit0 <- lm(as.formula(paste0(OUTCOME, " ~ ", paste(c(A_CENT_VARS, C_VARS), collapse=" + "))), data = dm)
  sm0 <- summary(fit0)$coefficients; ci0 <- confint(fit0)
  # β is per one unit of the composite (one step of the frequency scale). The exposure SD, the
  # per-SD value, the share of zeros, n and R² are written alongside for the Table 2 footnote.
  .sd_A0 <- setNames(vapply(A_CENT_VARS, function(a) sd(dm[[a]], na.rm = TRUE), numeric(1)), sub("_c$", "", A_CENT_VARS))
  .pct0  <- setNames(vapply(A_CONT_VARS, function(a) 100 * mean(dm[[a]] == 0, na.rm = TRUE), numeric(1)), sub("_continuous$", "", A_CONT_VARS))
  .fit_stats <- function(fit, nrow_d) data.frame(n = nrow_d, r2 = summary(fit)$r.squared, adj_r2 = summary(fit)$adj.r.squared, stringsAsFactors = FALSE)
  ov <- do.call(rbind, lapply(A_CENT_VARS, function(a) data.frame(purpose = sub("_c$","",a),
    beta = sm0[a,"Estimate"], se = sm0[a,"Std. Error"], ci_lo = ci0[a,1], ci_hi = ci0[a,2], p_value = sm0[a,"Pr(>|t|)"], n = nrow(dm),
    sd_A = unname(.sd_A0[sub("_c$","",a)]), beta_per_sd = sm0[a,"Estimate"] * unname(.sd_A0[sub("_c$","",a)]),
    pct_zero = unname(.pct0[sub("_c$","",a)]), sd_Y = sd(dm[[OUTCOME]]),
    r2 = summary(fit0)$r.squared, adj_r2 = summary(fit0)$adj.r.squared, stringsAsFactors = FALSE)))
  write_table(ov, "mwinit_overall")
  for (k in seq_len(nrow(ov))) log_msg(sprintf("  overall %-4s → loneliness: β=%+.4f per unit (p=%.3g)", ov$purpose[k], ov$beta[k], ov$p_value[k]))
  # every coefficient of the primary model (Supplementary Table 6)
  full0 <- data.frame(term = rownames(sm0), beta = sm0[, "Estimate"], se = sm0[, "Std. Error"],
                      ci_lo = ci0[, 1], ci_hi = ci0[, 2], t = sm0[, "t value"], p_value = sm0[, "Pr(>|t|)"], stringsAsFactors = FALSE)
  full0$term <- sub("_c$", "", full0$term); rownames(full0) <- NULL
  write_table(full0, "mwinit_overall_full_coefficients")
  write_table(data.frame(statistic = c("n", "r2", "adj_r2", "residual_sd", "sd_Y", "f_statistic", "df_model", "df_residual"),
                         value = c(nrow(dm), summary(fit0)$r.squared, summary(fit0)$adj.r.squared, summary(fit0)$sigma, sd(dm[[OUTCOME]]),
                                   unname(summary(fit0)$fstatistic[1]), unname(summary(fit0)$fstatistic[2]), unname(summary(fit0)$fstatistic[3]))),
              "mwinit_overall_fit_statistics")
  # ---- sensitivity analyses of the primary main effect (Supplementary Table 7) ----
  .ov_rows <- function(fit, nrow_d, vars = A_CENT_VARS, V = NULL) {
    # V = NULL: model-based OLS SEs and t-based CIs. A sandwich V (weighted models only)
    # gives robust SEs with normal CIs.
    sm <- summary(fit)$coefficients
    if (is.null(V)) { ci <- confint(fit); se <- sm[, "Std. Error"]; p <- sm[, "Pr(>|t|)"] } else {
      se <- sqrt(diag(V)); z <- sm[, "Estimate"] / se; p <- 2 * pnorm(-abs(z))
      ci <- cbind(sm[, "Estimate"] - qnorm(0.975) * se, sm[, "Estimate"] + qnorm(0.975) * se); rownames(ci) <- rownames(sm) }
    do.call(rbind, lapply(vars, function(a) data.frame(purpose = sub("_c$","",a),
      beta = sm[a,"Estimate"], se = unname(se[a]), ci_lo = ci[a,1], ci_hi = ci[a,2],
      p_value = unname(p[a]), n = nrow_d, r2 = summary(fit)$r.squared, adj_r2 = summary(fit)$adj.r.squared, stringsAsFactors = FALSE))) }
  # sensitivity analysis 2 — main effects among users only (exposures re-centred within users)
  di0 <- dm[dm$ai_init == 1L, , drop = FALSE]
  if (nrow(di0) >= 50L) {
    for (a in A_CENT_VARS) di0[[a]] <- di0[[sub("_c$","_continuous",a)]] - mean(di0[[sub("_c$","_continuous",a)]])
    cvI <- C_VARS[vapply(C_VARS, function(v) length(unique(di0[[v]])) > 1L, logical(1))]
    fitI <- lm(as.formula(paste0(OUTCOME, " ~ ", paste(c(A_CENT_VARS, cvI), collapse=" + "))), data = di0)
    ovI <- .ov_rows(fitI, nrow(di0))
    ovI$sd_A <- vapply(A_CENT_VARS, function(a) sd(di0[[a]]), numeric(1)); ovI$beta_per_sd <- ovI$beta * ovI$sd_A
    write_table(ovI, "mwinit_overall_init")
    log_msg(sprintf("  PRIMARY sens (initiators-only): A_SE β=%+.4f (p=%.3g), n=%d", ovI$beta[ovI$purpose=="A_SE"], ovI$p_value[ovI$purpose=="A_SE"], nrow(di0)))
  }
  # ---- sensitivity analysis 3 — the two social/emotional items separately and together ----
  .item_rows <- list()
  for (it in list(list(v = "A_SE_conv", lab = "Casual conversation item alone"), list(v = "A_SE_emo", lab = "Emotional support item alone"))) {
    f_it <- lm(as.formula(paste0(OUTCOME, " ~ ", paste(c(it$v, "A_PC_c", "A_DI_c", C_VARS), collapse = " + "))), data = dm)
    r <- .ov_rows(f_it, nrow(dm), vars = it$v); r$model <- it$lab; .item_rows[[length(.item_rows) + 1L]] <- r }
  f_both <- lm(as.formula(paste0(OUTCOME, " ~ ", paste(c("A_SE_conv", "A_SE_emo", "A_PC_c", "A_DI_c", C_VARS), collapse = " + "))), data = dm)
  r <- .ov_rows(f_both, nrow(dm), vars = c("A_SE_conv", "A_SE_emo")); r$model <- "Both items entered together"; .item_rows[[length(.item_rows) + 1L]] <- r
  ov_items <- do.call(rbind, .item_rows); ov_items$item <- ifelse(ov_items$purpose == "A_SE_conv", "Casual conversation", "Emotional support"); ov_items$purpose <- "A_SE"
  ov_items$item_correlation_initiators <- cor(dm$A_SE_conv[dm$ai_init == 1L], dm$A_SE_emo[dm$ai_init == 1L])
  write_table(ov_items, "mwinit_overall_items")
  for (k in seq_len(nrow(ov_items))) log_msg(sprintf("  SENS3 %-32s %-20s β=%+.4f (p=%.3g)", ov_items$model[k], ov_items$item[k], ov_items$beta[k], ov_items$p_value[k]))
  inter_l <- list(); strat_l <- list(); contr_l <- list()
  for (M in mods) {
    d2 <- if (isTRUE(M$drop_na)) dm[!is.na(dm[[M$mcol]]), , drop = FALSE] else dm
    cvM <- M$cv[vapply(M$cv, function(v) length(unique(d2[[v]])) > 1L, logical(1))]
    r <- tryCatch(run_modwide(d2, M$mcol, cvM), error = function(e) { log_msg(sprintf("  [%-22s] failed: %s", M$lab, conditionMessage(e))); NULL })
    if (is.null(r)) next
    r$inter$mod_label <- M$lab; r$strat$mod_label <- M$lab
    inter_l[[length(inter_l)+1L]] <- r$inter; strat_l[[length(strat_l)+1L]] <- r$strat
    if (!is.null(r$contr)) { r$contr$mod_label <- M$lab; contr_l[[length(contr_l)+1L]] <- r$contr }
    ipSE <- r$inter[r$inter$purpose == "A_SE", ]
    log_msg(sprintf("  [%-22s] A_SE effect-modification F(%d,%d)=%.2f, p=%.3g (n=%d)", M$lab, ipSE$df_num, ipSE$df_den, ipSE$F_stat, ipSE$p_value, nrow(d2)))
  }
  if (length(inter_l)) {
    mwi <- do.call(rbind, inter_l); mws <- do.call(rbind, strat_l)
    # the F test of the moderator's product terms (2 df for age, 1 df otherwise) is the reported
    # effect-modification test, Holm-adjusted across the four moderators within each purpose;
    # Benjamini-Hochberg q values are written alongside for reference.
    mwi$q_bh <- NA_real_; mwi$p_holm <- NA_real_; mwi$n_tests_in_family <- NA_integer_
    for (p in unique(mwi$purpose)) { idx <- mwi$purpose == p; mwi$q_bh[idx] <- p.adjust(mwi$p_value[idx], "BH")
      mwi$p_holm[idx] <- p.adjust(mwi$p_value[idx], "holm"); mwi$n_tests_in_family[idx] <- sum(idx) }
    # ----------------------------------------------------------------
    # §1a — standardized β (for reference; the manuscript reports β per unit)
    # Per-SD-A standardization: β_std = β_raw × σ_A_p (UCLA-3 stays in its own units).
    # Full Cohen also reported (β × σ_A / σ_Y) for users preferring effect-size-on-effect-size.
    # (standardization step inside the §1 block; no separate code block)
    # ----------------------------------------------------------------
    sd_A <- setNames(vapply(A_CENT_VARS, function(a) sd(dm[[a]], na.rm = TRUE), numeric(1)),
                     sub("_c$", "", A_CENT_VARS))
    sd_Y <- sd(dm[[OUTCOME]], na.rm = TRUE)
    mws$sd_A_purpose <- sd_A[mws$purpose]
    mws$beta_std     <- mws$slope * mws$sd_A_purpose
    mws$se_std       <- mws$se    * mws$sd_A_purpose
    mws$ci_lo_std    <- mws$ci_lo * mws$sd_A_purpose
    mws$ci_hi_std    <- mws$ci_hi * mws$sd_A_purpose
    mws$beta_cohen   <- mws$slope * mws$sd_A_purpose / sd_Y    # full standardization (β × σ_A / σ_Y)
    write_table(mwi, "mwinit_interaction"); write_table(mws, "mwinit_stratified")
    # Per-category effect-modification contrasts (γ vs reference level; own SE/z/p). Decomposes the
    # joint block-Wald F (mwinit_interaction) into per-non-reference-level moderation tests. Raw p only —
    # the joint F + its q_bh remains the BH-multiplicity unit; these contrasts are exploratory disaggregation.
    if (length(contr_l)) {
      mwc <- do.call(rbind, contr_l)
      mwc$sd_A_purpose <- sd_A[mwc$purpose]
      mwc$beta_std  <- mwc$gamma * mwc$sd_A_purpose   # per +1 SD A_p → UCLA-3 units (kept for reference)
      mwc$ci_lo_std <- mwc$ci_lo * mwc$sd_A_purpose
      mwc$ci_hi_std <- mwc$ci_hi * mwc$sd_A_purpose
      # attach the F test of the moderator's product terms and its Holm-adjusted P to each contrast
      key_c <- paste(mwc$moderator, mwc$purpose); key_i <- paste(mwi$moderator, mwi$purpose)
      mwc$omnibus_F <- mwi$F_stat[match(key_c, key_i)]; mwc$omnibus_df <- mwi$df_num[match(key_c, key_i)]
      mwc$omnibus_p <- mwi$p_value[match(key_c, key_i)]; mwc$omnibus_p_holm <- mwi$p_holm[match(key_c, key_i)]
      write_table(mwc, "mwinit_interaction_contrasts")
    }
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      suppressPackageStartupMessages(library(ggplot2))
      # Grouped forest: moderators as header rows on one shared y-axis (no per-moderator
      # boxes); AI-use purpose as columns; per-unit β, unclipped.
      g <- .forest_grouped(mws, "Effect modification of the association between AI use and loneliness, by social factor",
        "Adjusted β per one unit of the composite (95% CI), UCLA-3 units")
      for (ext in c("png","pdf")) ggsave(file.path(FIGS_DIR, sprintf("mwinit_forest_%s.%s", OUTCOME, ext)),
                                          g, width = 12, height = 6.5, dpi = 200)   # 4 moderators (was 6): 6.5 in high
      log_msg("wrote mwinit_forest figure (grouped by moderator; per-unit β; unclipped).")
    }
    sig <- mwi[is.finite(mwi$p_holm) & mwi$p_holm < 0.05, , drop = FALSE]
    log_msg(sprintf("effect-modification Holm-adjusted p<0.05: %s", if (nrow(sig)) paste(sprintf("%s×%s (p_holm=%.3f)", sig$mod_label, sig$purpose, sig$p_holm), collapse="; ") else "none"))
  }
  # ---- effect-modification sensitivity analysis 3: alternative LSNS-6 isolation thresholds ----
  # The main analysis dichotomises each subscale at <6; the same engine is re-run at <3, <9 and <12.
  set.seed(20260524)
  sweep_l <- list()
  for (S in list(list(lab = "LSNS friends", var = "baseline_lsns6_friends"), list(lab = "LSNS family", var = "baseline_lsns6_family"))) for (ct in c(LSNS_ALT_CUTS, 6)) {
    d2 <- dm; d2$iso_alt <- factor(ifelse(d2[[S$var]] < ct, "Isolated", "Connected"), levels = c("Connected", "Isolated"))
    if (min(table(d2$iso_alt)) < 100L) { log_msg(sprintf("  threshold sweep [%s < %d] SKIP: smallest stratum < 100", S$lab, ct)); next }
    cvM <- C_VARS[vapply(C_VARS, function(v) length(unique(d2[[v]])) > 1L, logical(1))]
    r <- tryCatch(run_modwide(d2, "iso_alt", cvM), error = function(e) NULL); if (is.null(r)) next
    s <- r$strat; s$moderator <- S$lab; s$mod_label <- S$lab; s$threshold <- sprintf("< %d", ct)
    s$pct_isolated <- 100 * mean(d2$iso_alt == "Isolated"); s$n_isolated <- sum(d2$iso_alt == "Isolated")
    ci_ <- r$contr; s$gamma <- ci_$gamma[match(s$purpose, ci_$purpose)]; s$gamma_ci_lo <- ci_$ci_lo[match(s$purpose, ci_$purpose)]
    s$gamma_ci_hi <- ci_$ci_hi[match(s$purpose, ci_$purpose)]; s$interaction_p <- ci_$p_value[match(s$purpose, ci_$purpose)]
    sweep_l[[length(sweep_l) + 1L]] <- s
  }
  if (length(sweep_l)) { sw <- do.call(rbind, sweep_l); write_table(sw, "mwinit_lsns_threshold_sweep")
    for (k in which(sw$purpose == "A_SE" & sw$level == "Isolated")) log_msg(sprintf("  threshold %-12s %-5s A_SE isolated β=%+.3f  interaction p=%.3g (%.0f%% isolated)", sw$moderator[k], sw$threshold[k], sw$slope[k], sw$interaction_p[k], sw$pct_isolated[k])) }
}
# ============================================================
# §2 Spline marginal associations of A_SE/A_PC/A_DI across continuous moderators
# ============================================================
# For each continuous moderator M (age, LSNS-6 friends, LSNS-6 family; sex is binary
# and stays in §1), fit ONE joint model:
#   Y_ucla3 ~ (A_SE_c + A_PC_c + A_DI_c) × rcs(M_c, 3) + Cmat
# Marginal association (AME) at 61 grid points over the full moderator range and at
# 5 percentiles {p10, p25, p50, p75, p90}. Figure facet_grid2(moderator ~ purpose).
set.seed(20260524)
if (nrow(dm) < 50L || .n_init < 50L || !.exp_ok) {
  log_msg("§2 spline AME SKIP: degenerate exposure.")
} else if (!requireNamespace("interactionRCS", quietly = TRUE) || !requireNamespace("rms", quietly = TRUE)) {
  log_msg("§2 spline AME SKIP: rms + interactionRCS not installed.")
} else {
  suppressPackageStartupMessages({ library(rms); library(interactionRCS) })
  log_msg("=== §2: Spline marginal associations of A_SE/A_PC/A_DI across continuous moderators ===")
  cont_mods <- list(   # the three continuous moderators (sex is binary and stays in §1).
    # The AME curve is evaluated over the full scale of the LSNS-6 subscales (0-15) and over the
    # observed age range.
    list(lab="LSNS friends",         var="baseline_lsns6_friends",  c_var="baseline_lsns6_friends_c", range = c(0, 15)),
    list(lab="LSNS family",          var="baseline_lsns6_family",   c_var="baseline_lsns6_family_c",  range = c(0, 15)),
    list(lab="Age (years)",          var="age_2024",                c_var="age_2024_c",               range = NULL))
  PUR_ALL <- c("A_SE","A_PC","A_DI")
  sp_rows <- list(); spq_rows <- list(); pred_rows <- list(); mtest_rows <- list()
  for (CM in cont_mods) {
    if (!CM$c_var %in% names(dm)) { log_msg(sprintf("  [%-22s] SKIP — column missing", CM$lab)); next }
    d2 <- dm[is.finite(dm[[CM$c_var]]), , drop = FALSE]
    if (nrow(d2) < 200L) { log_msg(sprintf("  [%-22s] SKIP n=%d < 200", CM$lab, nrow(d2))); next }
    cv <- C_VARS; if (CM$var %in% C_VARS) cv <- setdiff(cv, CM$var)
    cv <- cv[vapply(cv, function(v) length(unique(d2[[v]])) > 1L, logical(1))]
    d2$Cmat <- as.matrix(d2[, cv, drop = FALSE])
    meanM <- mean(d2[[CM$var]], na.rm = TRUE)
    .rng <- if (is.null(CM$range)) range(d2[[CM$var]], na.rm = TRUE) else CM$range
    wv <- seq(.rng[1] - meanM, .rng[2] - meanM, length.out = 61L)
    qprobs <- c(0.10, 0.25, 0.50, 0.75, 0.90)
    qv     <- unname(quantile(d2[[CM$c_var]], qprobs, na.rm = TRUE))
    # ONE joint model per moderator with all 3 A_p × rcs(M_c, 3) interactions.
    # Mirrors the §1 categorical engine (all 3 purposes × M in one model).
    rhs <- sprintf("(%s) * rcs(%s, 3) + Cmat",
                   paste(A_CENT_VARS, collapse = " + "), CM$c_var)
    m <- tryCatch(glm(as.formula(paste0(OUTCOME, " ~ ", rhs)), data = d2, family = gaussian()),
                  error = function(e) { log_msg(sprintf("  [%s] joint glm failed: %s", CM$lab, conditionMessage(e))); NULL })
    if (is.null(m)) next
    # ---- Formal spline-MODERATION tests (Test 1: overall 2-df; Test 2: nonlinearity 1-df) ----
    # Twin model with EXPLICIT rcs basis columns (Msp1 linear, Msp2 nonlinear) — same span as
    # rcs(M_c,3), so nested F-tests are basis-invariant and consistent with the intEST AME model.
    # Test 1 = "does M moderate A_p (any smooth shape)?" → drop BOTH A_p×basis interactions (2 df).
    # Test 2 = "is the moderation nonlinear beyond linear?" → drop ONLY A_p×nonlinear basis (1 df).
    Bsp <- tryCatch(rms::rcs(d2[[CM$c_var]], 3), error = function(e) NULL)
    if (!is.null(Bsp) && ncol(Bsp) == 2L) {
      d2$Msp1 <- as.numeric(Bsp[, 1]); d2$Msp2 <- as.numeric(Bsp[, 2])
      ints_sp    <- as.vector(t(outer(A_CENT_VARS, c("Msp1", "Msp2"), paste, sep = ":")))
      terms_full <- c(A_CENT_VARS, "Msp1", "Msp2", ints_sp, "Cmat")
      m_sp <- tryCatch(glm(as.formula(paste0(OUTCOME, " ~ ", paste(terms_full, collapse = " + "))),
                           data = d2, family = gaussian()), error = function(e) NULL)
      if (!is.null(m_sp)) for (pp in PUR_ALL) {
        Acp  <- paste0(pp, "_c")
        red1 <- setdiff(terms_full, c(paste0(Acp, ":Msp1"), paste0(Acp, ":Msp2")))   # Test 1: overall (2 df)
        red2 <- setdiff(terms_full, paste0(Acp, ":Msp2"))                            # Test 2: nonlinearity (1 df)
        f1 <- tryCatch(glm(as.formula(paste0(OUTCOME, " ~ ", paste(red1, collapse = " + "))), data = d2, family = gaussian()), error = function(e) NULL)
        f2 <- tryCatch(glm(as.formula(paste0(OUTCOME, " ~ ", paste(red2, collapse = " + "))), data = d2, family = gaussian()), error = function(e) NULL)
        a1 <- if (!is.null(f1)) anova(f1, m_sp, test = "F") else NULL
        a2 <- if (!is.null(f2)) anova(f2, m_sp, test = "F") else NULL
        mtest_rows[[length(mtest_rows) + 1L]] <- data.frame(
          moderator = CM$lab, purpose = pp, n = nrow(d2),
          overall_df = if (!is.null(a1)) a1$Df[2] else NA_integer_,
          overall_F  = if (!is.null(a1)) a1$F[2] else NA_real_,
          overall_p  = if (!is.null(a1)) a1[["Pr(>F)"]][2] else NA_real_,
          nonlin_df  = if (!is.null(a2)) a2$Df[2] else NA_integer_,
          nonlin_F   = if (!is.null(a2)) a2$F[2] else NA_real_,
          nonlin_p   = if (!is.null(a2)) a2[["Pr(>F)"]][2] else NA_real_,
          stringsAsFactors = FALSE)
      }
      d2$Msp1 <- NULL; d2$Msp2 <- NULL
    }
    # Per-purpose AME + quantile AME + predicted Y from the SAME joint model
    for (p in PUR_ALL) {
      Ac <- paste0(p, "_c"); others <- paste0(setdiff(PUR_ALL, p), "_c")
      e <- tryCatch(as.data.frame(interactionRCS::intEST(var2values = wv, model = m, data = d2,
                                                         var1 = Ac, var2 = CM$c_var,
                                                         ci = TRUE, conf = 0.95, ci.method = "delta")),
                    error = function(e) { log_msg(sprintf("  [%s × %s] intEST(curve) failed: %s",
                                                          p, CM$lab, conditionMessage(e))); NULL })
      if (!is.null(e)) sp_rows[[length(sp_rows) + 1L]] <- data.frame(
        purpose = p, moderator = CM$lab, n = nrow(d2),
        M_level = as.numeric(e[[1]]) + meanM,
        ame = as.numeric(e[[2]]), lo = as.numeric(e[[3]]), hi = as.numeric(e[[4]]),
        stringsAsFactors = FALSE)
      eq <- tryCatch(as.data.frame(interactionRCS::intEST(var2values = qv, model = m, data = d2,
                                                          var1 = Ac, var2 = CM$c_var,
                                                          ci = TRUE, conf = 0.95, ci.method = "delta")),
                     error = function(e) { log_msg(sprintf("  [%s × %s] intEST(quantiles) failed: %s",
                                                           p, CM$lab, conditionMessage(e))); NULL })
      if (!is.null(eq)) spq_rows[[length(spq_rows) + 1L]] <- data.frame(
        purpose = p, moderator = CM$lab, n = nrow(d2),
        quantile_label = c("p10","p25","p50","p75","p90")[seq_len(nrow(eq))],
        quantile_prob  = qprobs[seq_len(nrow(eq))],
        M_level = as.numeric(eq[[1]]) + meanM,
        ame = as.numeric(eq[[2]]), lo = as.numeric(eq[[3]]), hi = as.numeric(eq[[4]]),
        stringsAsFactors = FALSE)
      # predicted E[Y | A_p, M_c, others at means] at A_p ∈ {0, T1, T2, T3 tertile midpoints}.
      # Tertile midpoints = within-tertile median A_p among initiators (mirrors §4 sens1 categorical bins).
      mean_A_focal  <- mean(dm[[paste0(p, "_continuous")]], na.rm = TRUE)
      mean_A_others <- setNames(vapply(others, function(o) mean(d2[[o]], na.rm = TRUE), numeric(1)), others)
      mean_Cmat     <- colMeans(d2$Cmat, na.rm = TRUE)
      # Use [[ ]] indexing — dm is a tibble, so [, "col"] returns a 1-col tibble (not a vector),
      # which breaks is.finite(). [[col]][rows] returns a plain numeric vector.
      nz_p <- dm[[paste0(p, "_continuous")]][dm$ai_init == 1L]
      nz_p <- nz_p[is.finite(nz_p) & nz_p > 0]
      if (length(nz_p) >= 50L) {
        q33p <- unname(quantile(nz_p, 1/3, type = 7)); q67p <- unname(quantile(nz_p, 2/3, type = 7))
        mid_T1 <- median(nz_p[nz_p <= q33p])
        mid_T2 <- median(nz_p[nz_p >  q33p & nz_p <= q67p])
        mid_T3 <- median(nz_p[nz_p >  q67p])
        A_grid <- c(`0` = 0, T1 = mid_T1, T2 = mid_T2, T3 = mid_T3)
      } else A_grid <- c(`0` = 0)
      for (lab_a in names(A_grid)) {
        a_val <- unname(A_grid[lab_a]); a_c <- a_val - mean_A_focal
        nd <- data.frame(rep(a_c, length(wv))); names(nd) <- Ac
        nd[[CM$c_var]] <- wv
        for (o in others) nd[[o]] <- unname(mean_A_others[o])
        nd$Cmat <- matrix(rep(mean_Cmat, each = nrow(nd)),
                          nrow = nrow(nd), byrow = FALSE,
                          dimnames = list(NULL, names(mean_Cmat)))
        pr <- tryCatch(predict(m, newdata = nd, se.fit = TRUE),
                       error = function(e) { log_msg(sprintf("  [%s × %s] predict() failed: %s", p, CM$lab, conditionMessage(e))); NULL })
        if (!is.null(pr)) pred_rows[[length(pred_rows) + 1L]] <- data.frame(
          purpose = p, moderator = CM$lab, n = nrow(d2),
          A_level_label = lab_a, A_value = a_val,
          M_level = as.numeric(wv) + meanM,
          predicted = as.numeric(pr$fit),
          se = as.numeric(pr$se.fit),
          ci_lo = as.numeric(pr$fit - 1.96 * pr$se.fit),
          ci_hi = as.numeric(pr$fit + 1.96 * pr$se.fit),
          stringsAsFactors = FALSE)
      }
    }
    log_msg(sprintf("  [%-22s] n=%d  JOINT spline (all 3 A_p × rcs(M,3) in one model) + predicted Y (range %.2f–%.2f)",
                    CM$lab, nrow(d2), min(d2[[CM$var]], na.rm = TRUE), max(d2[[CM$var]], na.rm = TRUE)))
  }
  if (length(sp_rows))   write_table(do.call(rbind, sp_rows),   "mwinit_spline_AME")
  if (length(spq_rows))  write_table(do.call(rbind, spq_rows),  "mwinit_spline_AME_quantiles")
  if (length(pred_rows)) write_table(do.call(rbind, pred_rows), "mwinit_spline_predicted")
  if (length(mtest_rows)) {
    mt <- do.call(rbind, mtest_rows)
    # the spline effect-modification family per purpose = the 3 continuous moderators (2-df
    # spline test) + sex (the 1-df product term from the §1 categorical scan): 4 tests per
    # purpose, Holm-adjusted; Benjamini-Hochberg q values alongside for reference.
    if (exists("mwi") && is.data.frame(mwi)) {
      sx <- mwi[mwi$moderator == "sex_cat", , drop = FALSE]
      if (nrow(sx)) mt <- rbind(mt, data.frame(moderator = "Sex", purpose = sx$purpose, n = nrow(dm),
        overall_df = sx$df_num, overall_F = sx$F_stat, overall_p = sx$p_value, nonlin_df = NA_integer_, nonlin_F = NA_real_, nonlin_p = NA_real_, stringsAsFactors = FALSE))
    }
    mt$overall_q_bh <- NA_real_; mt$overall_p_holm <- NA_real_; mt$n_tests_in_family <- NA_integer_
    for (pp in unique(mt$purpose)) { ix <- mt$purpose == pp; mt$overall_q_bh[ix] <- p.adjust(mt$overall_p[ix], "BH")
      mt$overall_p_holm[ix] <- p.adjust(mt$overall_p[ix], "holm"); mt$n_tests_in_family[ix] <- sum(ix) }
    write_table(mt, "mwinit_spline_moderation_tests")
    log_msg(sprintf("§2 spline-moderation: wrote mwinit_spline_moderation_tests (overall 2-df + nonlinearity 1-df; + sex; Holm within purpose × %d tests).",
                    length(unique(mt$moderator))))
    for (k in which(mt$purpose == "A_SE")) log_msg(sprintf("  A_SE × %-14s F=%.2f p=%.3g Holm p=%.3g", mt$moderator[k], mt$overall_F[k], mt$overall_p[k], mt$overall_p_holm[k]))
  }
  if (length(sp_rows) && requireNamespace("ggplot2", quietly = TRUE)) {
    suppressPackageStartupMessages(library(ggplot2))
    sp <- do.call(rbind, sp_rows)
    # Age leftmost; SE/PC/DI order; no y-clipping.
    sp$moderator <- factor(sp$moderator, levels = c("Age (years)","LSNS friends","LSNS family"))
    sp$purpose <- .pur_factor(sp$purpose)   # academic column labels (no A_SE jargon)
    g <- ggplot(sp, aes(M_level, ame)) +
      geom_hline(yintercept = 0, linetype = "dashed", colour = "grey40") +
      geom_ribbon(aes(ymin = lo, ymax = hi, fill = purpose), alpha = 0.18, colour = NA) +
      geom_line(aes(colour = purpose), linewidth = 0.85) +
      labs(title = "Marginal association of AI use with loneliness across continuous moderators",
           subtitle = "Adjusted marginal association (95% CI) per one unit of the composite, by AI-use purpose",
           x = "Moderator value", y = "Adjusted marginal association with loneliness",
           colour = "AI-use purpose", fill = "AI-use purpose") +
      theme_bw(base_size = 10) + theme(legend.position = "bottom")
    # moderators TOP→BOTTOM (rows), purposes horizontal (cols); per-moderator x via independent scales.
    if (requireNamespace("ggh4x", quietly = TRUE)) { suppressPackageStartupMessages(library(ggh4x))
      g <- g + ggh4x::facet_grid2(moderator ~ purpose, scales = "free_x", independent = "x")
    } else g <- g + facet_grid(moderator ~ purpose, scales = "free_x")
    for (ext in c("png","pdf")) ggsave(file.path(FIGS_DIR, sprintf("mwinit_spline_AME_%s.%s", OUTCOME, ext)),
                                        g, width = 9.5, height = 8, dpi = 200)
    log_msg("§2 wrote mwinit_spline_AME figure (3 moderators × 3 purposes; moderators top→bottom; unclipped).")
  }
  # predicted UCLA-3 across moderator at A_p = 0/T1/T2/T3,
  # with 95% CI ribbons + per-facet x-scales (LSNS 0-15; age own); 3-12 scale, unclipped.
  if (length(pred_rows) && requireNamespace("ggplot2", quietly = TRUE)) {
    suppressPackageStartupMessages(library(ggplot2))
    pp <- do.call(rbind, pred_rows)
    pp$moderator     <- factor(pp$moderator, levels = c("Age (years)","LSNS friends","LSNS family"))
    pp$purpose       <- .pur_factor(pp$purpose)
    pp$A_level_label <- factor(pp$A_level_label, levels = c("0","T1","T2","T3"))
    gp <- ggplot(pp, aes(M_level, predicted, colour = A_level_label, fill = A_level_label, group = A_level_label)) +
      geom_ribbon(aes(ymin = ci_lo, ymax = ci_hi), alpha = 0.12, colour = NA) +
      geom_line(linewidth = 0.85) +
      scale_colour_brewer(palette = "RdYlBu", direction = -1, name = "AI-use level") +
      scale_fill_brewer(palette = "RdYlBu", direction = -1, name = "AI-use level") +
      labs(title = "Predicted loneliness across continuous moderators, by AI-use level",
           subtitle = "Predicted UCLA-3 (95% CI) at no use / low / medium / high; other purposes + confounders at means",
           x = "Moderator value", y = "Predicted loneliness (UCLA-3, 3–12)") +
      theme_bw(base_size = 10) + theme(legend.position = "bottom")
    # moderators TOP→BOTTOM (rows), purposes horizontal (cols); per-moderator x via independent scales.
    if (requireNamespace("ggh4x", quietly = TRUE)) { suppressPackageStartupMessages(library(ggh4x))
      gp <- gp + ggh4x::facet_grid2(moderator ~ purpose, scales = "free_x", independent = "x")
    } else gp <- gp + facet_grid(moderator ~ purpose, scales = "free_x")
    for (ext in c("png","pdf")) ggsave(file.path(FIGS_DIR, sprintf("mwinit_spline_predicted_%s.%s", OUTCOME, ext)),
                                        gp, width = 9.5, height = 8, dpi = 200)
    log_msg("§2 wrote mwinit_spline_predicted figure (moderators top→bottom; E[Y|A_p,M] at A_p=0/T1/T2/T3 + 95% CI).")
  }
}

# ============================================================
# §3 SENS2: moderator-wide scan restricted to AI users (initiators)
# ============================================================
# Re-run the §1 scan on initiators only (ai_init==1) — the initiator-restricted
# sensitivity. (Among initiators, A_p carries within-user frequency variation; the
# never-users at A=0 are removed.) Outputs suffixed _init.
set.seed(20260524)
if (.n_init < 200L) {
  log_msg("§3 users-only sensitivity SKIP: <200 initiators.")
} else {
  log_msg("=== §3 SENSITIVITY: moderator-wide scan, INITIATORS only ===")
  di <- dm[dm$ai_init == 1L, , drop = FALSE]
  for (a in A_CENT_VARS) di[[a]] <- di[[sub("_c$","_continuous",a)]] - mean(di[[sub("_c$","_continuous",a)]])  # re-center within initiators
  inter_i <- list(); strat_i <- list(); contr_i <- list()
  for (M in mods) {
    d2 <- if (isTRUE(M$drop_na)) di[!is.na(di[[M$mcol]]), , drop=FALSE] else di
    cvM <- M$cv[vapply(M$cv, function(v) length(unique(d2[[v]]))>1L, logical(1))]
    r <- tryCatch(run_modwide(d2, M$mcol, cvM), error=function(e){log_msg(sprintf("  [%-22s] failed: %s", M$lab, conditionMessage(e))); NULL})
    if (is.null(r)) next
    r$inter$mod_label <- M$lab; r$strat$mod_label <- M$lab
    inter_i[[length(inter_i)+1L]] <- r$inter; strat_i[[length(strat_i)+1L]] <- r$strat
    if (!is.null(r$contr)) { r$contr$mod_label <- M$lab; contr_i[[length(contr_i)+1L]] <- r$contr }
    ip <- r$inter[r$inter$purpose=="A_SE", ]
    log_msg(sprintf("  [%-22s] A_SE effect-mod F(%d,%d)=%.2f, p=%.3g (n=%d)", M$lab, ip$df_num, ip$df_den, ip$F_stat, ip$p_value, nrow(d2)))
  }
  if (length(inter_i)) {
    mwii <- do.call(rbind, inter_i); mwis <- do.call(rbind, strat_i)
    mwii$q_bh <- NA_real_; mwii$p_holm <- NA_real_
    for (p in unique(mwii$purpose)) { idx <- mwii$purpose==p; mwii$q_bh[idx] <- p.adjust(mwii$p_value[idx], "BH"); mwii$p_holm[idx] <- p.adjust(mwii$p_value[idx], "holm") }
    # standardized β on initiator-only SDs (re-centered exposure → recompute SD).
    sd_A_i <- setNames(vapply(A_CENT_VARS, function(a) sd(di[[a]], na.rm = TRUE), numeric(1)),
                       sub("_c$", "", A_CENT_VARS))
    sd_Y_i <- sd(di[[OUTCOME]], na.rm = TRUE)
    mwis$sd_A_purpose <- sd_A_i[mwis$purpose]
    mwis$beta_std   <- mwis$slope * mwis$sd_A_purpose
    mwis$se_std     <- mwis$se    * mwis$sd_A_purpose
    mwis$ci_lo_std  <- mwis$ci_lo * mwis$sd_A_purpose
    mwis$ci_hi_std  <- mwis$ci_hi * mwis$sd_A_purpose
    mwis$beta_cohen <- mwis$slope * mwis$sd_A_purpose / sd_Y_i
    write_table(mwii, "mwinit_interaction_init"); write_table(mwis, "mwinit_stratified_init")
    # per-category product-term contrasts (γ = A_p×M[level]) on users-only SDs — the interaction P
    # values of Supplementary Table 14; the F tests of the moderator (mwii) give its Holm-adjusted P values.
    if (length(contr_i)) {
      mwci <- do.call(rbind, contr_i)
      mwci$sd_A_purpose <- sd_A_i[mwci$purpose]
      mwci$beta_std  <- mwci$gamma * mwci$sd_A_purpose
      mwci$ci_lo_std <- mwci$ci_lo * mwci$sd_A_purpose
      mwci$ci_hi_std <- mwci$ci_hi * mwci$sd_A_purpose
      key_c <- paste(mwci$moderator, mwci$purpose); key_i <- paste(mwii$moderator, mwii$purpose)
      mwci$omnibus_p <- mwii$p_value[match(key_c, key_i)]; mwci$omnibus_p_holm <- mwii$p_holm[match(key_c, key_i)]
      write_table(mwci, "mwinit_interaction_contrasts_init")
    }
    # users-only forest — same structure as the §1 forest, restricted to initiators;
    # per-unit β (same scale as the main forest), unclipped.
    if (requireNamespace("ggplot2", quietly = TRUE)) {
      suppressPackageStartupMessages(library(ggplot2))
      fsi <- mwis
      fsi$purpose <- factor(fsi$purpose, levels = c("A_SE","A_PC","A_DI"))
      mod_order_i <- vapply(mods, function(M) M$lab, character(1))
      fsi$mod_label <- factor(fsi$mod_label, levels = mod_order_i)
      gi <- .forest_grouped(fsi, "Effect modification among AI initiators only (Sensitivity analysis 2)",
        "Adjusted β per one unit of the composite (95% CI), UCLA-3 units")
      for (ext in c("png","pdf")) ggsave(file.path(FIGS_DIR, sprintf("mwinit_forest_init_%s.%s", OUTCOME, ext)),
                                          gi, width = 12, height = 6.5, dpi = 200)
      log_msg("wrote mwinit_forest_init figure (grouped; initiators-only).")
    }
  }
}

# ============================================================
# §4 SENS1: categorical-exposure scan
# ============================================================
# Replace continuous A_p_c with a 4-level factor (0 / T1 / T2 / T3) where T1-T3
# are tertiles of the NON-ZERO distribution among initiators. Decomposes the
# extensive (T1 vs 0 = any-use jump) and intensive (T3 vs T1 = within-user
# dose) margins that the non-users-0 linear coding mixes.
# Fallback: if 1/3 and 2/3 quantiles collapse → median 3-level (0/low/high);
# if median collapses too → binary (0/any). Reference always = 0.
.make_A_factor <- function(x, idx_init) {
  nz <- x[idx_init & is.finite(x) & x > 0]
  if (length(nz) < 50L) return(list(fac = NULL, kind = "skip", q33 = NA_real_, q50 = NA_real_, q67 = NA_real_))
  q33 <- unname(quantile(nz, 1/3, type = 7)); q50 <- unname(quantile(nz, 0.5, type = 7)); q67 <- unname(quantile(nz, 2/3, type = 7))
  qmin <- min(nz)
  if (q33 < q67 - 1e-8) {
    fac <- cut(x, breaks = c(-Inf, 0, q33, q67, Inf), labels = c("0","T1","T2","T3"), include.lowest = TRUE, right = TRUE)
    kind <- "tertile_4level"
  } else if (q50 > qmin + 1e-8) {
    fac <- cut(x, breaks = c(-Inf, 0, q50, Inf), labels = c("0","low","high"), include.lowest = TRUE, right = TRUE)
    kind <- "median_3level"
  } else {
    fac <- factor(ifelse(is.finite(x) & x > 0, "any", "0"), levels = c("0","any")); kind <- "binary_2level"
  }
  fac <- factor(fac, levels = c("0", setdiff(levels(fac), "0")))
  list(fac = fac, kind = kind, q33 = q33, q50 = q50, q67 = q67)
}
# Same logic, initiators-only (drops level 0; ref = T1 or low or any)
.make_A_factor_init <- function(x_init) {
  pos <- x_init[is.finite(x_init) & x_init > 0]
  if (length(pos) < 50L) return(list(fac = NULL, kind = "skip", q33 = NA_real_, q50 = NA_real_, q67 = NA_real_, idx = NULL))
  q33 <- unname(quantile(pos, 1/3, type = 7)); q50 <- unname(quantile(pos, 0.5, type = 7)); q67 <- unname(quantile(pos, 2/3, type = 7))
  qmin <- min(pos); idx <- is.finite(x_init) & x_init > 0   # keep only positive
  if (q33 < q67 - 1e-8) {
    fac_v <- cut(x_init, breaks = c(0, q33, q67, Inf), labels = c("T1","T2","T3"), include.lowest = FALSE, right = TRUE)
    fac_v <- factor(fac_v, levels = c("T1","T2","T3")); kind <- "tertile_3level_no0"
  } else if (q50 > qmin + 1e-8) {
    fac_v <- cut(x_init, breaks = c(0, q50, Inf), labels = c("low","high"), include.lowest = FALSE, right = TRUE)
    fac_v <- factor(fac_v, levels = c("low","high")); kind <- "median_2level_no0"
  } else {
    fac_v <- factor(rep("any", length(x_init)), levels = "any"); kind <- "single_level_no0"
  }
  fac_v[!idx] <- NA   # drop zeros
  list(fac = fac_v, kind = kind, q33 = q33, q50 = q50, q67 = q67, idx = idx)
}

set.seed(20260524)
.n_init <- sum(dm$ai_init == 1L); .can_cat <- (.n_init >= 50L && .exp_ok)
if (!.can_cat) {
  log_msg(sprintf("§4 categorical scan SKIP: n_init=%d (need >=50 with non-degenerate exposure).", .n_init))
} else {
  log_msg("=== §4 SENSITIVITY: categorical-exposure scan (0 / T1 / T2 / T3) ===")
  # Build factors on full cohort; log cutpoints
  A_FAC_VARS <- character(0); A_FAC_KINDS <- list(); cut_rows <- list()
  for (a in A_VARS) {
    out <- .make_A_factor(dm[[paste0(a, "_continuous")]], dm$ai_init == 1L)
    if (out$kind == "skip") {
      log_msg(sprintf("  [%s] cutpoint SKIP (n_nonzero < 50)", a))
      cut_rows[[length(cut_rows)+1L]] <- data.frame(purpose=a, kind=out$kind,
        q33=out$q33, q50=out$q50, q67=out$q67, n_0=NA_integer_, n_T1=NA_integer_, n_T2=NA_integer_, n_T3=NA_integer_,
        stringsAsFactors = FALSE); next
    }
    dm[[paste0(a, "_cat")]] <- out$fac; A_FAC_VARS <- c(A_FAC_VARS, paste0(a, "_cat")); A_FAC_KINDS[[a]] <- out$kind
    tbl <- table(out$fac, useNA = "no")
    log_msg(sprintf("  [%s] %s  q33=%.3f q50=%.3f q67=%.3f  n by level: %s",
                    a, out$kind, out$q33, out$q50, out$q67,
                    paste(sprintf("%s=%d", names(tbl), as.integer(tbl)), collapse = ", ")))
    cut_rows[[length(cut_rows)+1L]] <- data.frame(
      purpose = a, kind = out$kind, q33 = out$q33, q50 = out$q50, q67 = out$q67,
      n_0  = as.integer(tbl["0"]),
      n_T1 = as.integer(if ("T1" %in% names(tbl)) tbl["T1"] else if ("low" %in% names(tbl)) tbl["low"] else if ("any" %in% names(tbl)) tbl["any"] else NA),
      n_T2 = as.integer(if ("T2" %in% names(tbl)) tbl["T2"] else if ("high" %in% names(tbl)) tbl["high"] else NA),
      n_T3 = as.integer(if ("T3" %in% names(tbl)) tbl["T3"] else NA),
      stringsAsFactors = FALSE)
  }
  write_table(do.call(rbind, cut_rows), "mwinit_cat_cutpoints")

  if (!length(A_FAC_VARS)) {
    log_msg("  §4 cat scan SKIP: no purpose has a usable cutpoint.")
  } else {
    # ---- §4 unmoderated main effects (SENS1): each factor level vs never-users ----
    # All three purposes enter as 0/T1/T2/T3 factors in ONE model together with the indicator
    # of generative-AI use (ai_init), so that the "0" level of each purpose refers to AI users
    # without that purpose and never-users are separated. (Three never/no-use/T1-T3 factors
    # cannot be entered together: their non-reference dummies all sum to ai_init.)
    #   ai_init coefficient        = initiator with no use of any purpose vs never-user
    #   T_k coefficient            = T_k vs initiator without the focal use (other purposes fixed)
    #   ai_init + T_k              = T_k vs never-users (reported in Table 2 / Sup Table 4)
    rhs0 <- paste(c("ai_init", A_FAC_VARS, C_VARS), collapse = " + ")
    fit0c <- lm(as.formula(paste0(OUTCOME, " ~ ", rhs0)), data = dm)
    sm0 <- summary(fit0c)$coefficients; V0 <- vcov(fit0c); b0 <- coef(fit0c); nm0 <- names(b0)
    if (any(is.na(b0))) {   # guard: an aliased term (e.g. ai_init if every initiator uses some purpose) would otherwise turn every contrast into NA
      log_msg(sprintf("  SENS1 WARNING: aliased term(s) in the categorical model, dropped from the contrasts: %s", paste(nm0[is.na(b0)], collapse = ", ")))
      .ok0 <- !is.na(b0); b0 <- b0[.ok0]; V0 <- V0[.ok0, .ok0, drop = FALSE]; nm0 <- names(b0) }
    .lin <- function(cvec, label, purpose, a_level, n_level) { est <- sum(cvec * b0); se <- sqrt(max(0, as.numeric(t(cvec) %*% V0 %*% cvec)))
      tq <- qt(0.975, df.residual(fit0c)); data.frame(purpose = purpose, a_level = a_level, contrast = label, n_level = n_level,
      beta = est, se = se, ci_lo = est - tq * se, ci_hi = est + tq * se, p_value = 2 * pt(-abs(est / se), df.residual(fit0c)),
      n = nrow(dm), r2 = summary(fit0c)$r.squared, adj_r2 = summary(fit0c)$adj.r.squared, stringsAsFactors = FALSE) }
    ov_rows <- list(); n_never <- sum(dm$ai_init == 0L)
    for (a_fac in A_FAC_VARS) {
      p <- sub("_cat$","", a_fac); lev_a <- levels(dm[[a_fac]]); tb <- table(dm[[a_fac]][dm$ai_init == 1L])
      cv0 <- rep(0, length(b0)); cv0[nm0 == "ai_init"] <- 1
      ov_rows[[length(ov_rows)+1L]] <- .lin(cv0, "initiator, no use vs never-users", p, "0 (initiator)", as.integer(tb[["0"]]))
      for (L in lev_a[-1]) { tn <- paste0(a_fac, L); if (!tn %in% nm0) next
        cvk <- cv0; cvk[nm0 == tn] <- 1
        ov_rows[[length(ov_rows)+1L]] <- .lin(cvk, sprintf("%s vs never-users", L), p, L, as.integer(tb[[L]])) }
      # T_k vs AI user without the focal use (= the T_k coefficient; other purposes at their reference).
      for (L in lev_a[-1]) { tn <- paste0(a_fac, L); if (!tn %in% nm0) next
        cvt <- rep(0, length(b0)); cvt[nm0 == tn] <- 1
        ov_rows[[length(ov_rows)+1L]] <- .lin(cvt, sprintf("%s vs initiator, no use", L), p, L, as.integer(tb[[L]])) }
    }
    ov_cat <- do.call(rbind, ov_rows); write_table(ov_cat, "mwinit_cat_overall")
    for (k in seq_len(nrow(ov_cat))) log_msg(sprintf("  SENS1 %s %-34s β=%+.4f (p=%.3g)", ov_cat$purpose[k], ov_cat$contrast[k], ov_cat$beta[k], ov_cat$p_value[k]))

    # ---- §4 moderator-wide scan engine ----
    run_modwide_cat <- function(d, mcol, cv, A_FAC_VARS) {
      pur <- sub("_cat$","", A_FAC_VARS)
      rhs_full <- paste(c(A_FAC_VARS, mcol, paste0(A_FAC_VARS, ":", mcol), cv), collapse = " + ")
      fit <- lm(as.formula(paste0(OUTCOME, " ~ ", rhs_full)), data = d)
      b <- coef(fit); V <- vcov(fit); nm <- names(b)
      mlevs <- levels(droplevels(d[[mcol]])); refM <- mlevs[1]; n_by <- table(d[[mcol]])
      inter <- list(); strat <- list(); contr <- list()
      for (k in seq_along(A_FAC_VARS)) {
        A_fac <- A_FAC_VARS[k]; p <- pur[k]
        A_levs <- levels(d[[A_fac]]); A_lev_nz <- A_levs[-1]
        rhs_red <- paste(c(A_FAC_VARS, mcol, paste0(setdiff(A_FAC_VARS, A_fac), ":", mcol), cv), collapse = " + ")
        fit_red <- lm(as.formula(paste0(OUTCOME, " ~ ", rhs_red)), data = d)
        aw <- anova(fit_red, fit)
        inter[[length(inter)+1L]] <- data.frame(moderator = mcol, purpose = p,
          df_num = aw$Df[2], df_den = aw$Res.Df[2], F_stat = aw$F[2], p_value = aw[["Pr(>F)"]][2],
          stringsAsFactors = FALSE)
        for (LM in mlevs) for (a_lv in A_lev_nz) {
          cvec <- rep(0, length(b))
          a_main_nm <- paste0(A_fac, a_lv); if (a_main_nm %in% nm) cvec[nm == a_main_nm] <- 1
          int_nm <- NA_character_
          if (LM != refM) {
            int1 <- paste0(A_fac, a_lv, ":", mcol, LM); if (int1 %in% nm) cvec[nm == int1] <- 1
            int2 <- paste0(mcol, LM, ":", A_fac, a_lv); if (int2 %in% nm) cvec[nm == int2] <- 1
            int_nm <- if (int1 %in% nm) int1 else if (int2 %in% nm) int2 else NA_character_
          }
          sl <- sum(cvec * b); se <- sqrt(max(0, as.numeric(t(cvec) %*% V %*% cvec)))
          z <- if (se > 0) sl/se else NA_real_
          strat[[length(strat)+1L]] <- data.frame(moderator = mcol, purpose = p, level = LM,
            a_level = a_lv, n = as.integer(n_by[[LM]]), contrast_beta = sl, se = se,
            ci_lo = sl - 1.96*se, ci_hi = sl + 1.96*se, z = z,
            p_value = if (is.finite(z)) 2*pnorm(-abs(z)) else NA_real_,
            stringsAsFactors = FALSE)
          # per-cell product-term (interaction) coefficient γ = A_fac[a_lv]:mcol[LM] =
          # (A_lv-vs-0 effect at M=LM) − (A_lv-vs-0 effect at M=refM): the SENS1 moderation test.
          if (LM != refM && !is.na(int_nm)) {
            g <- unname(b[int_nm]); seg <- sqrt(max(0, as.numeric(V[int_nm, int_nm]))); zg <- if (seg > 0) g/seg else NA_real_
            contr[[length(contr)+1L]] <- data.frame(moderator = mcol, purpose = p, level = LM, ref_level = refM,
              a_level = a_lv, n_level = as.integer(n_by[[LM]]), gamma = g, se = seg,
              ci_lo = g - 1.96*seg, ci_hi = g + 1.96*seg, z = zg,
              p_value = if (is.finite(zg)) 2*pnorm(-abs(zg)) else NA_real_, stringsAsFactors = FALSE)
          }
        }
      }
      list(inter = do.call(rbind, inter), strat = do.call(rbind, strat),
           contr = if (length(contr)) do.call(rbind, contr) else NULL)
    }

    inter_c <- list(); strat_c <- list(); contr_c <- list()
    for (M in mods) {
      d2 <- if (isTRUE(M$drop_na)) dm[!is.na(dm[[M$mcol]]), , drop = FALSE] else dm
      # drop factor levels with 0 observations after subset (e.g. income_cat NA)
      for (a_fac in A_FAC_VARS) d2[[a_fac]] <- droplevels(d2[[a_fac]])
      cvM <- M$cv[vapply(M$cv, function(v) length(unique(d2[[v]])) > 1L, logical(1))]
      r <- tryCatch(run_modwide_cat(d2, M$mcol, cvM, A_FAC_VARS),
                    error = function(e) { log_msg(sprintf("  [%-22s] §4 failed: %s", M$lab, conditionMessage(e))); NULL })
      if (is.null(r)) next
      r$inter$mod_label <- M$lab; r$strat$mod_label <- M$lab
      inter_c[[length(inter_c)+1L]] <- r$inter; strat_c[[length(strat_c)+1L]] <- r$strat
      if (!is.null(r$contr)) { r$contr$mod_label <- M$lab; contr_c[[length(contr_c)+1L]] <- r$contr }
      ipSE <- r$inter[r$inter$purpose == "A_SE", ]
      if (nrow(ipSE)) log_msg(sprintf("  [%-22s] A_SE cat effect-mod F(%d,%d)=%.2f, p=%.3g (n=%d)",
        M$lab, ipSE$df_num, ipSE$df_den, ipSE$F_stat, ipSE$p_value, nrow(d2)))
    }
    if (length(inter_c)) {
      mwic <- do.call(rbind, inter_c); mwsc <- do.call(rbind, strat_c)
      mwic$q_bh <- NA_real_; for (p in unique(mwic$purpose)) { idx <- mwic$purpose == p; mwic$q_bh[idx] <- p.adjust(mwic$p_value[idx], "BH") }
      write_table(mwic, "mwinit_cat_interaction"); write_table(mwsc, "mwinit_cat_stratified")
      # per-cell product-term (interaction) contrasts (γ = A_level:M_level) — the SENS1
      # moderation-significance panel (Sup Table 6, dir40); omnibus F (mwic) stays internal QC.
      if (length(contr_c)) write_table(do.call(rbind, contr_c), "mwinit_cat_interaction_contrasts")
      sig <- mwic[is.finite(mwic$q_bh) & mwic$q_bh < 0.10, , drop = FALSE]
      log_msg(sprintf("§4 cat effect-mod q_bh<0.10: %s",
                      if (nrow(sig)) paste(sprintf("%s×%s (q=%.3f)", sig$mod_label, sig$purpose, sig$q_bh), collapse = "; ") else "none"))

      # Dose-response forest: include 0 as explicit reference point (β=0)
      if (requireNamespace("ggplot2", quietly = TRUE)) {
        suppressPackageStartupMessages(library(ggplot2))
        ref_rows <- unique(mwsc[, c("mod_label","purpose","level","n")])
        ref_rows$a_level <- "0"; ref_rows$contrast_beta <- 0; ref_rows$se <- 0
        ref_rows$ci_lo <- 0; ref_rows$ci_hi <- 0; ref_rows$z <- NA_real_; ref_rows$p_value <- NA_real_
        ref_rows$moderator <- NA_character_   # not needed for plot
        plot_cols <- c("mod_label","purpose","level","n","a_level","contrast_beta","ci_lo","ci_hi")
        fs_plot <- rbind(mwsc[, plot_cols], ref_rows[, plot_cols])
        a_levels_seen <- unique(fs_plot$a_level)
        a_order <- c("0","T1","T2","T3","low","high","any"); a_order <- a_order[a_order %in% a_levels_seen]
        fs_plot$a_level <- factor(fs_plot$a_level, levels = a_order)
        fs_plot <- .order_strata(fs_plot)   # strata low→high top→bottom
        fs_plot$mod_short <- factor(unname(.MOD_SHORT[as.character(fs_plot$mod_label)]), levels = unname(.MOD_SHORT[vapply(mods, function(M) M$lab, character(1))]))
        fs_plot$purpose <- .pur_factor(fs_plot$purpose)
        # Header-row layout: moderators as header rows on one shared
        # y-axis; the dose levels (0/T1/T2/T3) are vertically dodged within each stratum.
        LYc <- .forest_positions(fs_plot)
        fs_plot <- LYc$df
        nlev <- nlevels(fs_plot$a_level)
        fs_plot$ypos_d <- fs_plot$ypos + ((nlev + 1) / 2 - as.integer(fs_plot$a_level)) * (0.72 / nlev)
        gc <- ggplot(fs_plot, aes(contrast_beta, ypos_d, colour = a_level, shape = a_level)) +
          geom_vline(xintercept = 0, linetype = "dashed", colour = "grey50") +
          geom_pointrange(aes(xmin = ci_lo, xmax = ci_hi), linewidth = 0.4, size = 0.3, na.rm = TRUE) +
          facet_grid(. ~ purpose) +
          scale_y_continuous(breaks = LYc$breaks, labels = LYc$labels, expand = ggplot2::expansion(add = c(0.8, 1.4))) +
          labs(title = "Dose–response association of AI use with loneliness, by social-factor stratum (Sensitivity analysis 1)",
               subtitle = "Adjusted β for each AI-use level vs no use (95% CI); levels = tertiles of non-zero use among initiators",
               x = "Adjusted β vs no use (95% CI), UCLA-3 units", y = NULL, colour = "AI-use level", shape = "AI-use level") +
          theme_bw(base_size = 9) +
          theme(legend.position = "bottom", panel.grid.major.y = ggplot2::element_blank(),
                axis.text.y = if (LYc$use_md) ggtext::element_markdown(hjust = 0) else ggplot2::element_text(hjust = 0))
        for (ext in c("png","pdf")) ggsave(file.path(FIGS_DIR, sprintf("mwinit_cat_forest_%s.%s", OUTCOME, ext)),
                                            gc, width = 12, height = 9, dpi = 200)
        log_msg("§4 wrote mwinit_cat_forest figure (dose-response: SE/PC/DI order; 0/T1/T2/T3 top-to-bottom).")
      }
    }
  }
}

# ============================================================
# §5 further sensitivity analyses of the PRIMARY main effect
# ============================================================
# SENS4 overlap weighting for ANY social/emotional use (balance and positivity diagnostics)
# SENS5 inverse-probability-of-retention (attrition) weighting
# SENS6 baseline completed before 1 January 2025
# E-values for the primary estimate
# The two weighted models use robust (sandwich, HC3) standard errors because the weights are
# estimated; every other model uses model-based OLS standard errors.
set.seed(20260524)
if (nrow(dm) < 50L || .n_init < 50L || !.exp_ok) {
  log_msg("§5 SKIP: degenerate exposure.")
} else {
  log_msg("=== §5 SENSITIVITY: overlap weighting, attrition weighting, baseline timing, E-values ===")
  .ess <- function(w) sum(w)^2 / sum(w^2)
  .w_mean <- function(x, w) sum(w * x) / sum(w); .w_var <- function(x, w) sum(w * (x - .w_mean(x, w))^2) / sum(w)
  .smd <- function(x, g, w = rep(1, length(x))) { m1 <- .w_mean(x[g == 1], w[g == 1]); m0 <- .w_mean(x[g == 0], w[g == 0])
    v1 <- .w_var(x[g == 1], w[g == 1]); v0 <- .w_var(x[g == 0], w[g == 0]); (m1 - m0) / sqrt((v1 + v0) / 2) }
  .sandwich_ok <- requireNamespace("sandwich", quietly = TRUE)
  .V_robust <- function(fit) if (.sandwich_ok) sandwich::vcovHC(fit, type = "HC3") else { log_msg("  sandwich not installed: model-based SEs used for the weighted model"); vcov(fit) }
  # ---- SENS4: overlap weights for any S/E use vs none ----
  # Propensity model: logistic regression of any S/E use on the 39 covariates, with the six continuous
  # covariates also entered as restricted cubic splines (3 knots). Overlap weights (exposed 1-PS,
  # unexposed PS; Li, Morgan & Zaslavsky 2018) give exact mean balance on every covariate in the
  # propensity model without trimming; balance and positivity diagnostics are written out.
  PS_CONT <- c("age_2024", "baseline_k6", "baseline_ucla3", "baseline_lsns6_friends", "baseline_lsns6_family", "mental_physical_health")
  for (v in PS_CONT) { bs <- rms::rcs(dm[[v]], 3); dm[[paste0(v, "_rcs2")]] <- as.numeric(bs[, 2]) }
  PS_VARS <- c(C_VARS, paste0(PS_CONT, "_rcs2"))
  ps_fit <- glm(as.formula(paste("A_SE_any ~", paste(PS_VARS, collapse = " + "))), data = dm, family = binomial())
  dm$ps <- fitted(ps_fit); dm$ow <- ifelse(dm$A_SE_any == 1L, 1 - dm$ps, dm$ps)
  .qs <- function(x) c(min = min(x), p5 = unname(quantile(x, .05)), p25 = unname(quantile(x, .25)), median = median(x), p75 = unname(quantile(x, .75)), p95 = unname(quantile(x, .95)), max = max(x), mean = mean(x))
  ps_e <- dm$ps[dm$A_SE_any == 1L]; ps_u <- dm$ps[dm$A_SE_any == 0L]
  ps_summary <- rbind(data.frame(group = "Any social/emotional use", n = length(ps_e), t(.qs(ps_e))), data.frame(group = "No social/emotional use", n = length(ps_u), t(.qs(ps_u))))
  ps_summary$pct_outside_other_group_range <- c(100 * mean(ps_e < min(ps_u) | ps_e > max(ps_u)), 100 * mean(ps_u < min(ps_e) | ps_u > max(ps_e)))
  ps_summary$auc <- { r <- rank(dm$ps); n1 <- sum(dm$A_SE_any == 1L); n0 <- sum(dm$A_SE_any == 0L); (sum(r[dm$A_SE_any == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
  write_table(ps_summary, "mwinit_ow_propensity_summary")
  bal <- do.call(rbind, lapply(C_VARS, function(v) data.frame(covariate = v,
    mean_exposed = mean(dm[[v]][dm$A_SE_any == 1L]), mean_unexposed = mean(dm[[v]][dm$A_SE_any == 0L]),
    smd_unweighted = .smd(dm[[v]], dm$A_SE_any), smd_weighted = .smd(dm[[v]], dm$A_SE_any, dm$ow), stringsAsFactors = FALSE)))
  write_table(bal, "mwinit_ow_balance")
  write_table(data.frame(statistic = c("n", "n_exposed", "ess_exposed", "ess_unexposed", "weight_mean_exposed", "weight_mean_unexposed",
                                       "max_abs_smd_unweighted", "max_abs_smd_weighted", "n_covariates_abs_smd_gt_0_1_unweighted", "n_covariates_abs_smd_gt_0_1_weighted"),
                         value = c(nrow(dm), sum(dm$A_SE_any), .ess(dm$ow[dm$A_SE_any == 1L]), .ess(dm$ow[dm$A_SE_any == 0L]), mean(dm$ow[dm$A_SE_any == 1L]), mean(dm$ow[dm$A_SE_any == 0L]),
                                   max(abs(bal$smd_unweighted)), max(abs(bal$smd_weighted)), sum(abs(bal$smd_unweighted) > 0.1), sum(abs(bal$smd_weighted) > 0.1))), "mwinit_ow_weight_summary")
  log_msg(sprintf("  SENS4 balance: max |SMD| unweighted %.3f; overlap-weighted %.3f", max(abs(bal$smd_unweighted)), max(abs(bal$smd_weighted))))
  fit_any_unw <- lm(as.formula(paste0(OUTCOME, " ~ ", paste(c("A_SE_any", "A_PC_c", "A_DI_c", C_VARS), collapse = " + "))), data = dm)
  fit_any_ow  <- lm(as.formula(paste0(OUTCOME, " ~ ", paste(c("A_SE_any", "A_PC_c", "A_DI_c"), collapse = " + "))), data = dm, weights = dm$ow)
  fit_any_owc <- lm(as.formula(paste0(OUTCOME, " ~ ", paste(c("A_SE_any", "A_PC_c", "A_DI_c", C_VARS), collapse = " + "))), data = dm, weights = dm$ow)
  ov_ow <- rbind(cbind(model = "Any S/E use vs none, covariate-adjusted (unweighted)", .ov_rows(fit_any_unw, nrow(dm), vars = "A_SE_any")),
                 cbind(model = "Any S/E use vs none, overlap-weighted", .ov_rows(fit_any_ow, nrow(dm), vars = "A_SE_any", V = .V_robust(fit_any_ow))),
                 cbind(model = "Any S/E use vs none, overlap-weighted + covariate-adjusted", .ov_rows(fit_any_owc, nrow(dm), vars = "A_SE_any", V = .V_robust(fit_any_owc))))
  ov_ow$purpose <- "A_SE"; write_table(ov_ow, "mwinit_overall_overlap_weighted")
  for (k in seq_len(nrow(ov_ow))) log_msg(sprintf("  SENS4 %-62s β=%+.4f (%+.3f, %+.3f) p=%.3g", ov_ow$model[k], ov_ow$beta[k], ov_ow$ci_lo[k], ov_ow$ci_hi[k], ov_ow$p_value[k]))
  if (requireNamespace("ggplot2", quietly = TRUE)) {   # balance + positivity figures
    suppressPackageStartupMessages(library(ggplot2))
    .C_LAB <- c(age_2024 = "Age (years)", sex_female = "Female", edu_univ = "Education: university", edu_grad = "Education: graduate", emp_exec = "Employment: executive", emp_self = "Employment: self-employed",
      emp_nonreg = "Employment: non-regular", emp_student = "Employment: student", emp_notwork = "Employment: not working", income_2_6m = "Income 2-6 m JPY", income_6_10m = "Income 6-10 m JPY",
      income_10m_plus = "Income 10+ m JPY", income_unknown = "Income unknown", married = "Married", living_alone = "Living alone", baseline_lsns6_family = "LSNS-6 family", baseline_lsns6_friends = "LSNS-6 friends",
      baseline_ucla3 = "Baseline UCLA-3", baseline_k6 = "Baseline K6", ace_1 = "ACE: 1", ace_2_3 = "ACE: 2-3", ace_4plus = "ACE: 4+", mental_physical_health = "Mental/physical health",
      smartphone_band_1_2 = "Smartphone 1-2 h/day", smartphone_band_3_4 = "Smartphone 3-5 h/day", smartphone_band_5plus = "Smartphone 6+ h/day", smartphone_unknown = "Smartphone: unknown",
      pc_tablet_band_1_2 = "PC/tablet 1-2 h/day", pc_tablet_band_3_4 = "PC/tablet 3-5 h/day", pc_tablet_band_5plus = "PC/tablet 6+ h/day", pc_tablet_unknown = "PC/tablet: unknown",
      sitting_band_1_2 = "Sitting 1-2 h/day", sitting_band_3_4 = "Sitting 3-5 h/day", sitting_band_5plus = "Sitting 6+ h/day", sitting_unknown = "Sitting: unknown",
      walking_band_1_2 = "Walking 1-2 h/day", walking_band_3_4 = "Walking 3-5 h/day", walking_band_5plus = "Walking 6+ h/day", walking_unknown = "Walking: unknown")
    bal$label <- ifelse(bal$covariate %in% names(.C_LAB), .C_LAB[bal$covariate], bal$covariate)
    bal_long <- rbind(data.frame(covariate = bal$label, smd = bal$smd_unweighted, sample = "Unweighted"), data.frame(covariate = bal$label, smd = bal$smd_weighted, sample = "Overlap-weighted"))
    bal_long$covariate <- factor(bal_long$covariate, levels = bal$label[order(abs(bal$smd_unweighted))])
    gb <- ggplot(bal_long, aes(smd, covariate, colour = sample, shape = sample)) + geom_vline(xintercept = c(-0.1, 0.1), linetype = "dotted") + geom_vline(xintercept = 0) + geom_point(size = 2) +
      labs(title = "Covariate balance: any social/emotional use vs none", x = "Standardized mean difference", y = NULL, colour = NULL, shape = NULL) + theme_bw(base_size = 10) + theme(legend.position = "bottom")
    for (ext in c("png","pdf")) ggsave(file.path(FIGS_DIR, sprintf("mwinit_ow_balance.%s", ext)), gb, width = 7, height = 9, dpi = 200)
    ps_df <- data.frame(ps = dm$ps, group = ifelse(dm$A_SE_any == 1L, "Any social/emotional use", "No social/emotional use"))
    gps <- ggplot(ps_df, aes(ps, fill = group)) + geom_density(alpha = 0.4, colour = NA) + labs(title = "Propensity-score overlap", x = "Estimated probability of any social/emotional use", y = "Density", fill = NULL) +
      theme_bw(base_size = 10) + theme(legend.position = "bottom")
    for (ext in c("png","pdf")) ggsave(file.path(FIGS_DIR, sprintf("mwinit_ow_propensity_overlap.%s", ext)), gps, width = 7, height = 4.5, dpi = 200)
    log_msg("wrote mwinit_ow_balance and mwinit_ow_propensity_overlap figures.")
  }
  # ---- SENS5: inverse-probability-of-retention weights from the all-2024 file ----
  # Any failure in this block (e.g. a raw 2024 column missing from the all-2024 file) is logged and skipped so that
  # SENS6, the E-values and the package table are still written.
  if (file.exists(DATA_2024_ALL_PATH)) tryCatch({
    all24 <- readr::read_csv(DATA_2024_ALL_PATH, show_col_types = FALSE, guess_max = 30000)
    .need24 <- c(ID_COL, "AGE_2024", "SEX_2024", "Q21.1_2024", "Q5.1_2024", "Q80.1_2024", "Q2_2024", "Q1.1_2024", "Q76.3_2024", "Q76.4_2024",
                 paste0("Q66.", 1:3, "_2024"), paste0("Q65.", 1:6, "_2024"), paste0("Q77.", c(1:9, 13), "_2024"), paste0("Q17.", 1:6, "_2024"), paste0("Q28.", c(5, 6, 13, 14), "_2024"))
    .miss24 <- setdiff(.need24, names(all24)); if (length(.miss24)) log_msg("  SENS5 WARNING: all-2024 file lacks column(s): ", paste(.miss24, collapse = ", "))
    .safe24 <- function(cn) if (cn %in% names(all24)) .as_num(all24[[cn]]) else rep(NA_real_, nrow(all24))
    all24$baseline_ucla3 <- .row_sum_fn(all24, paste0("Q66.",1:3,"_2024"), fn=.ucla_recode); all24$baseline_k6 <- .row_sum_fn(all24, paste0("Q65.",1:6,"_2024"), fn=.k6_recode)
    ace24 <- intersect(paste0("Q77.",c(1:8,13),"_2024"), names(all24))
    if (length(ace24)) { ap <- vapply(all24[ace24], function(v) as.integer(.as_num(v)==1L), integer(nrow(all24))); aps <- rowSums(ap, na.rm=TRUE)
      if ("Q77.9_2024" %in% names(all24)) { q9 <- .as_num(all24$Q77.9_2024); a9 <- as.integer(q9==2L); a9[is.na(q9)] <- 0L } else a9 <- 0L; all24$ace_score <- aps + a9 } else all24$ace_score <- NA_real_
    all24$ace_1 <- as.integer(all24$ace_score==1L); all24$ace_2_3 <- as.integer(all24$ace_score %in% 2:3); all24$ace_4plus <- as.integer(all24$ace_score>=4L)
    all24$age_2024 <- .safe24("AGE_2024"); all24$sex_female <- as.integer(.safe24("SEX_2024")==2L)
    e24 <- .safe24("Q21.1_2024"); all24$edu_univ <- as.integer(e24 %in% 6:8); all24$edu_grad <- as.integer(e24==9L)
    m24 <- .safe24("Q5.1_2024"); all24$emp_exec <- as.integer(m24==1L); all24$emp_self <- as.integer(m24 %in% 2:4); all24$emp_nonreg <- as.integer(m24 %in% 7:11); all24$emp_student <- as.integer(m24 %in% 12:13); all24$emp_notwork <- as.integer(m24 %in% 14:16 | is.na(m24))
    i24 <- .safe24("Q80.1_2024"); all24$income_2_6m <- as.integer(i24 %in% 5:8); all24$income_6_10m <- as.integer(i24 %in% 9:12); all24$income_10m_plus <- as.integer(i24 %in% 13:18); all24$income_unknown <- as.integer(is.na(i24) | i24 %in% c(19L,20L))
    all24$married <- as.integer(.safe24("Q2_2024") %in% 1:3); l24 <- .safe24("Q1.1_2024"); all24$living_alone <- as.integer(!is.na(l24) & l24==1L)
    all24$baseline_lsns6_family <- .row_sum_fn(all24, paste0("Q17.",1:3,"_2024"), fn=.lsns_recode); all24$baseline_lsns6_friends <- .row_sum_fn(all24, paste0("Q17.",4:6,"_2024"), fn=.lsns_recode)
    all24$mental_physical_health <- .row_mean(all24, c("Q76.3_2024","Q76.4_2024"))
    all24 <- .td(all24,"Q28.13_2024","smartphone"); all24 <- .td(all24,"Q28.14_2024","pc_tablet"); all24 <- .td(all24,"Q28.5_2024","sitting"); all24 <- .td(all24,"Q28.6_2024","walking")
    # link on a common text key, so that it does not matter whether readr read Monitor_ID as a number in one file and as text in the other
    .id_key <- function(x) { k <- if (is.numeric(x)) sprintf("%.0f", x) else trimws(as.character(x)); k[is.na(x)] <- NA_character_; k }
    all24$retained <- if (ID_COL %in% names(all24) && ID_COL %in% names(df)) { k24 <- .id_key(all24[[ID_COL]]); kdf <- .id_key(df[[ID_COL]]); as.integer(!is.na(k24) & k24 %in% kdf[!is.na(kdf)]) } else NA_integer_
    if (all(is.na(all24$retained))) { log_msg("  SENS5 SKIP: cannot link the all-2024 file to the two-wave file (no Monitor_ID)") } else {
      log_msg(sprintf("  SENS5: all-2024 file %d rows; %d linked to the two-wave file (%d rows); %d complete on the 39 covariates",
                      nrow(all24), sum(all24$retained, na.rm = TRUE), nrow(df), sum(complete.cases(all24[, C_VARS]))))
      all24 <- all24[complete.cases(all24[, C_VARS]) & !is.na(all24$retained), , drop = FALSE]
      if (nrow(all24) < 100L || length(unique(all24$retained)) < 2L) { log_msg(sprintf("  SENS5 SKIP: %d usable all-2024 rows, %d of them retained — check that the all-2024 file has the same *_2024 column names and the same Monitor_ID values as the two-wave file", nrow(all24), sum(all24$retained))) } else {
      ret_fit <- glm(as.formula(paste("retained ~", paste(C_VARS, collapse = " + "))), data = all24, family = binomial())
      p_ret_dm <- predict(ret_fit, newdata = dm[, C_VARS], type = "response")
      w_raw <- mean(all24$retained) / p_ret_dm; w99 <- unname(quantile(w_raw, 0.99)); dm$ipaw <- pmin(w_raw, w99)   # stabilised, trimmed at p99
      fit_aw <- lm(as.formula(paste0(OUTCOME, " ~ ", paste(c(A_CENT_VARS, C_VARS), collapse = " + "))), data = dm, weights = dm$ipaw)
      ov_aw <- .ov_rows(fit_aw, nrow(dm), V = .V_robust(fit_aw)); write_table(ov_aw, "mwinit_overall_attrition_weighted")
      write_table(data.frame(statistic = c("n_valid_2024", "n_retained", "retention_pct", "ipaw_mean", "ipaw_sd", "ipaw_min", "ipaw_median", "ipaw_trim_p99", "ipaw_max_after_trim", "ess"),
                             value = c(nrow(all24), sum(all24$retained), 100 * mean(all24$retained), mean(dm$ipaw), sd(dm$ipaw), min(dm$ipaw), median(dm$ipaw), w99, max(dm$ipaw), .ess(dm$ipaw))), "mwinit_ipaw_weight_summary")
      log_msg(sprintf("  SENS5 attrition-weighted: A_SE β=%+.4f (%+.3f, %+.3f) p=%.3g; retention %.1f%%", ov_aw$beta[1], ov_aw$ci_lo[1], ov_aw$ci_hi[1], ov_aw$p_value[1], 100 * mean(all24$retained)))
      }
    }
  }, error = function(e) log_msg("  SENS5 FAILED (skipped): ", conditionMessage(e))) else log_msg("  SENS5 SKIP: all-2024 file not found (", DATA_2024_ALL_PATH, ")")
  # ---- SENS6: baseline completed before 1 January 2025 ----
  if (any(!is.na(dm$baseline_date))) {
    d_pre <- dm[!is.na(dm$baseline_date) & dm$baseline_after_2025 == 0L, , drop = FALSE]   # rows without a parsed date are excluded, not treated as pre-2025
    cvP <- C_VARS[vapply(C_VARS, function(v) length(unique(d_pre[[v]])) > 1L, logical(1))]
    fit_pre <- lm(as.formula(paste0(OUTCOME, " ~ ", paste(c(A_CENT_VARS, cvP), collapse = " + "))), data = d_pre)
    ov_pre <- .ov_rows(fit_pre, nrow(d_pre)); write_table(ov_pre, "mwinit_overall_pre2025_baseline")
    write_table(data.frame(group = c("Analytic cohort", "Initiators", "Never-users"), n = c(nrow(dm), sum(dm$ai_init == 1L), sum(dm$ai_init == 0L)),
                           n_baseline_on_or_after_2025_01_01 = c(sum(dm$baseline_after_2025), sum(dm$baseline_after_2025[dm$ai_init == 1L]), sum(dm$baseline_after_2025[dm$ai_init == 0L])),
                           pct = 100 * c(mean(dm$baseline_after_2025), mean(dm$baseline_after_2025[dm$ai_init == 1L]), mean(dm$baseline_after_2025[dm$ai_init == 0L]))), "mwinit_baseline_timing")
    log_msg(sprintf("  SENS6 baseline before 2025 (n=%d): A_SE β=%+.4f (%+.3f, %+.3f) p=%.3g", nrow(d_pre), ov_pre$beta[1], ov_pre$ci_lo[1], ov_pre$ci_hi[1], ov_pre$p_value[1]))
  } else log_msg("  SENS6 SKIP: no baseline completion timestamp")
  # ---- E-values (VanderWeele & Ding 2017; continuous outcome via the standardised difference) ----
  .evalue <- function(est, se, sd_out) { d <- est / sd_out; sd_d <- se / sd_out
    rr <- exp(0.91 * d); lo <- exp(0.91 * d - 1.78 * sd_d); hi <- exp(0.91 * d + 1.78 * sd_d)
    ev <- function(r) { if (r < 1) r <- 1 / r; r + sqrt(r * (r - 1)) }
    limit <- if (lo > 1) lo else if (hi < 1) hi else 1
    c(rr_point = rr, rr_lo = lo, rr_hi = hi, evalue_point = ev(rr), evalue_ci = if (limit == 1) 1 else ev(limit)) }
  sdY <- sd(dm[[OUTCOME]]); ev_in <- list(
    data.frame(contrast = "Social/emotional use, per one unit (primary model)", estimate = ov$beta[ov$purpose == "A_SE"], se = ov$se[ov$purpose == "A_SE"]),
    data.frame(contrast = "Social/emotional use, per 1 SD of the composite (primary model)", estimate = ov$beta[ov$purpose == "A_SE"] * ov$sd_A[ov$purpose == "A_SE"], se = ov$se[ov$purpose == "A_SE"] * ov$sd_A[ov$purpose == "A_SE"]),
    data.frame(contrast = "Any social/emotional use vs none (covariate-adjusted)", estimate = ov_ow$beta[1], se = ov_ow$se[1]))
  if (exists("ov_cat")) { kT3 <- which(ov_cat$purpose == "A_SE" & ov_cat$contrast == "T3 vs never-users")
    if (length(kT3)) ev_in[[length(ev_in) + 1L]] <- data.frame(contrast = "Highest tertile of social/emotional use vs never-users", estimate = ov_cat$beta[kT3], se = ov_cat$se[kT3]) }
  ev_tab <- do.call(rbind, lapply(ev_in, function(r) cbind(r, t(.evalue(r$estimate, r$se, sdY))))); ev_tab$outcome_sd <- sdY
  write_table(ev_tab, "mwinit_evalues")
  for (k in seq_len(nrow(ev_tab))) log_msg(sprintf("  E-value %-62s point=%.2f  CI limit=%.2f", ev_tab$contrast[k], ev_tab$evalue_point[k], ev_tab$evalue_ci[k]))
}

# package versions
.pk <- c("base", "rms", "interactionRCS", "Hmisc", "sandwich", "psych", "GPArotation", "lavaan", "car", "ggplot2", "ggh4x", "ggtext", "readr", "here")
write_table(data.frame(package = .pk, version = vapply(.pk, function(p) if (p == "base") as.character(getRversion()) else if (requireNamespace(p, quietly = TRUE)) as.character(packageVersion(p)) else "not installed", character(1))), "mwinit_package_versions")
writeLines(capture.output(sessionInfo()), file.path(LOGS_DIR, sprintf("sessionInfo_modwideinit_%s.txt", .timestamp)))
log_msg("=== END (modwide_init) ===")
