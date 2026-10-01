# =============================================================================
# diagnosis.r — diagnostics and descriptive analyses for the AI-use and loneliness study
# =============================================================================
# Shares the analytic build (Y + 3 A purposes + 39 C) with moderator-wide.r.
#
# Cohort   : Q37S1_2025 in {1, 5, 6}  (never-users + 2025 initiators)
# Exposure : 3 AI-use purpose composites measured in 2025; never-users coded 0
# Outcome  : Y_ucla3 (2025 UCLA-3 sum, 3-12)
#
# Diagnostics (all analytic, no bootstrap):
#   D1 Cohort flow                — per-code Q37S1 breakdown → analytic sample (Supplementary Figure 1)
#   D2 Missingness                — Y + A items + 39 C; full cohort + users only
#   D3 Variable sanity            — range/mean/SD/n for Y, A, baselines, LSNS, age
#   D4 Cronbach's α               — 3 A composites + UCLA-3 base/outcome + K6 + LSNS + ACE
#   D5 Moderator distribution     — 4 moderator factors × ai_init cross-tab
#   D6 A descriptive + crude A→Y  — full cohort (never-users 0) AND users only
#   D7 Cutpoint feasibility       — per-purpose non-zero quantiles among users (tertiles)
#   D8 VIF                        — main-effects + friends_iso + income; uncentred + centred
#   D9 Straight-lining            — % of users giving identical values across the 9 purpose items
#   D10 EFA (Pearson + polychoric) — 9 purpose items, users (Supplementary Table 1A-B)
#   D11 CFA holdout               — three-factor vs one-factor model, pre-2025 initiators (Supplementary Table 1C)
#   D12 Exposure distribution     — share of zeros; any social/emotional use
#   D13 Characteristics by history— whole two-wave panel by Q37S1 code (Supplementary Table 4)
#   D14 Attrition                 — retained vs lost 2024 respondents; retention model (Supplementary Table 5)
#   D15 Baseline dates / overlap  — completion dates; overlap with the earlier analytic sample
#
# Produces flags_<ts>.md (rejected/warned items) for triage.
# Outputs → output/ai_mod/diagnosis/. Files: 0X_*.csv + flags_<ts>.md + diagnosis_<ts>.log.
# Conventions (= moderator-wide.r): set.seed(20260524); here::here() for paths; never setwd()/quit();
# snake_case; raw data immutable. See CODEBOOK.md for the variable definitions.
# =============================================================================

suppressPackageStartupMessages({
  library(here); library(readr); library(psych); library(car)
})
set.seed(20260524)

# -------- Paths / mode / logging (mirrors moderator-wide.r boilerplate) -----
find_proj_root <- function() {
  candidates <- character(0); ch <- tryCatch(here::here(), error = function(e) NA_character_)
  if (!is.na(ch)) candidates <- c(candidates, ch)
  args <- commandArgs(trailingOnly = FALSE); fa <- args[grepl("^--file=", args)]
  if (length(fa)) { sd <- tryCatch(normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/"),
                                   error = function(e) NA_character_)
                    if (!is.na(sd)) candidates <- c(candidates, sd, dirname(sd)) }
  candidates <- c(candidates, getwd(), dirname(getwd()))
  for (cand in unique(candidates)) {
    if (!nzchar(cand)) next
    if (basename(cand) == "r_code" && dir.exists(file.path(cand, "data"))) return(normalizePath(cand, winslash = "/"))
    if (dir.exists(file.path(cand, "r_code", "data"))) return(normalizePath(file.path(cand, "r_code"), winslash = "/"))
  }
  stop("Could not find r_code/")
}
.proj_root <- find_proj_root()
.norm <- function(p) normalizePath(p, winslash = "/", mustWork = FALSE)
DATA_PATH <- Sys.getenv("JACSIS_2WAVE_2425_PATH", unset = "")
if (!nzchar(DATA_PATH)) DATA_PATH <- file.path(.proj_root, "data", "jacsis_2wave2425.csv")
DATA_PATH <- .norm(DATA_PATH); if (!file.exists(DATA_PATH)) stop("Input CSV not found: ", DATA_PATH)
# optional inputs — all valid 2024 respondents (attrition) and the earlier analytic sample list (overlap)
DATA_2024_ALL_PATH <- Sys.getenv("JACSIS_2024_ALL_PATH", unset = ""); if (!nzchar(DATA_2024_ALL_PATH)) DATA_2024_ALL_PATH <- file.path(.proj_root, "data", "jacsis_2024_all.csv")
REF5_LIST_PATH     <- Sys.getenv("REF5_LIST_PATH", unset = "");      if (!nzchar(REF5_LIST_PATH))     REF5_LIST_PATH     <- file.path(.proj_root, "data", "df_ref5_list.csv")
DATA_2024_ALL_PATH <- .norm(DATA_2024_ALL_PATH); REF5_LIST_PATH <- .norm(REF5_LIST_PATH)
ID_COL <- "Monitor_ID"; REF5_ID_COL <- "RI_ID_2024"
BASELINE_TS_COL <- "回答完了日時_2024"   # 回答完了日時_2024 (UTC, ISO 8601)
OUTCOME <- "Y_ucla3"
OUT_BASE <- file.path(.proj_root, "output", "ai_mod", "diagnosis")
dir.create(OUT_BASE, recursive = TRUE, showWarnings = FALSE)
.timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE   <- file.path(OUT_BASE, sprintf("diagnosis_%s.log", .timestamp))
FLAGS_FILE <- file.path(OUT_BASE, sprintf("flags_%s.md",      .timestamp))

log_msg <- function(...) {
  m <- paste0(format(Sys.time(), "[%H:%M:%S] "), paste(..., collapse = " "))
  cat(m, "\n", sep = ""); cat(m, "\n", sep = "", file = LOG_FILE, append = TRUE)
  invisible(m)
}
write_csv_out <- function(x, name) {
  fp <- file.path(OUT_BASE, paste0(name, ".csv"))
  utils::write.csv(x, fp, row.names = FALSE, fileEncoding = "UTF-8")
  log_msg("wrote:", fp)
}
.flags_rejected <- list(); .flags_warned <- list()
add_flag <- function(level, item, rule, detail) {
  row <- data.frame(item = item, rule = rule, detail = detail, stringsAsFactors = FALSE)
  if (level == "reject") .flags_rejected[[length(.flags_rejected)+1L]] <<- row
  else                    .flags_warned[[length(.flags_warned)+1L]]    <<- row
}

log_msg("=== diagnosis.R START ===  input:", DATA_PATH)

# -------- Helpers (= moderator-wide.r §0) -----------------------------------
.as_num <- function(x) suppressWarnings(as.numeric(x))
.row_mean <- function(d, cols, na_rm = FALSE) {
  cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d)))
  rowMeans(vapply(d[cols], .as_num, numeric(nrow(d))), na.rm = na_rm)
}
.row_sum_fn <- function(d, cols, fn = identity, na_rm = TRUE) {
  cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d)))
  rowSums(vapply(d[cols], function(v) fn(.as_num(v)), numeric(nrow(d))), na.rm = na_rm)
}
.ucla_recode <- function(M) pmax(1, pmin(4, 5 - M))   # items scored 1-4, sum 3-12
.k6_recode   <- function(M) pmax(0, pmin(4, 5 - M))
.lsns_recode <- function(M) pmax(0, pmin(5, M - 1L))
.td <- function(d, raw, p) {
  v <- .as_num(d[[raw]])
  d[[paste0(p,"_band_1_2")]]   <- as.integer(v %in% c(4L,5L))
  d[[paste0(p,"_band_3_4")]]   <- as.integer(v %in% c(6L,7L))
  d[[paste0(p,"_band_5plus")]] <- as.integer(v %in% c(8:11))
  d[[paste0(p,"_unknown")]]    <- as.integer(is.na(v) | v == 12L); d
}
.safe <- function(df, cn) if (cn %in% names(df)) .as_num(df[[cn]]) else rep(NA_real_, nrow(df))
.alpha_for <- function(items, data) {
  items <- intersect(items, names(data)); if (length(items) < 2L) return(list(alpha = NA_real_, n_eff = 0L))
  m <- as.matrix(data[, items, drop = FALSE]); mode(m) <- "numeric"
  m <- m[complete.cases(m), , drop = FALSE]
  if (nrow(m) < 20L) return(list(alpha = NA_real_, n_eff = nrow(m)))
  a <- tryCatch(suppressWarnings(suppressMessages(
        psych::alpha(m, na.rm = TRUE, warnings = FALSE)$total$raw_alpha)),
        error = function(e) NA_real_)
  list(alpha = a, n_eff = nrow(m))
}
.vif_flag <- function(v) ifelse(v > 10, "severe", ifelse(v > 5, "mild", "ok"))
.safe_vif <- function(fit, data) tryCatch({
  aliased <- alias(fit)$Complete
  if (!is.null(aliased) && nrow(aliased) > 0L) {
    keep <- setdiff(attr(terms(fit), "term.labels"), rownames(aliased))
    lhs <- as.character(formula(fit))[2]
    fit2 <- lm(as.formula(paste0(lhs, " ~ ", paste(keep, collapse = " + "))), data = data)
    car::vif(fit2)
  } else car::vif(fit)
}, error = function(e) { log_msg("  vif() failed:", conditionMessage(e)); NULL })

# -------- A_purpose item spec ------------------------------------------------
A_SPEC <- list(
  A_SE = paste0("Q37S3.", c(8,9),     "_2025"),  # social/emotional (chat, emotional support)
  A_PC = paste0("Q37S3.", c(1,2,4,5), "_2025"),  # productivity/creative
  A_DI = paste0("Q37S3.", c(3,6,7),   "_2025")   # daily/information
)
A_VARS <- names(A_SPEC); A_CONT_VARS <- paste0(A_VARS, "_continuous")

# -------- Load + build (mirrors moderator-wide.r §0) ------------------------
df <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
log_msg(sprintf("Loaded: %d rows × %d cols", nrow(df), ncol(df)))

df$Y_ucla3 <- .row_sum_fn(df, paste0("Q66.", 1:3, "_2025"), fn = .ucla_recode)
ai_start <- .safe(df, "Q37S1_2025")
df$Q37S1_2025 <- ai_start
df$ai_init <- as.integer(ai_start %in% c(5L, 6L))
# use-history label (Q37S1_2025 codes) for the descriptive table by history
df$ai_start_label <- factor(ai_start, levels = 1:6, labels = c("Never used", "Past user, not current", "Started 2022-2023", "Started 2024", "Started Jan-Jun 2025", "Started Jul-Dec 2025"))
df$ai_current_user <- as.integer(ai_start %in% 3:6)
# baseline completion date (JST) and flag for baselines on/after 1 Jan 2025
.ts_col <- if (BASELINE_TS_COL %in% names(df)) BASELINE_TS_COL else grep("完了日時.*2024", names(df), value = TRUE, useBytes = TRUE)[1]
# readr already parses ISO-8601 timestamps to POSIXct (UTC) on read, so the column is used as is; a character
# column is parsed with explicit formats (as.POSIXct() without a format silently truncates every value to the
# date when any value falls exactly on midnight). Same helper as moderator-wide.r.
.parse_ts <- function(x) { if (inherits(x, "POSIXt")) return(as.POSIXct(x)); x <- trimws(as.character(x)); ok <- !is.na(x) & nzchar(x)
  for (fmt in c("%Y-%m-%dT%H:%M:%SZ", "%Y-%m-%dT%H:%M:%OSZ", "%Y-%m-%d %H:%M:%S", "%Y/%m/%d %H:%M:%S", "%Y-%m-%d %H:%M", "%Y/%m/%d %H:%M", "%Y-%m-%d", "%Y/%m/%d")) {
    ts <- suppressWarnings(as.POSIXct(x, format = fmt, tz = "UTC")); if (any(ok) && mean(!is.na(ts[ok])) > 0.5) return(ts) }
  as.POSIXct(rep(NA_real_, length(x)), origin = "1970-01-01", tz = "UTC") }
if (!is.na(.ts_col)) { .ts <- .parse_ts(df[[.ts_col]]); df$baseline_date <- as.Date(format(.ts, tz = "Asia/Tokyo", "%Y-%m-%d")) } else df$baseline_date <- as.Date(NA)
df$baseline_after_2025 <- as.integer(!is.na(df$baseline_date) & df$baseline_date >= as.Date("2025-01-01"))
log_msg(sprintf("baseline timestamp column: %s; %d of %d rows dated; %d (%.1f%%) completed on/after 2025-01-01 JST",
                if (is.na(.ts_col)) "NOT FOUND" else .ts_col, sum(!is.na(df$baseline_date)), nrow(df), sum(df$baseline_after_2025), 100 * mean(df$baseline_after_2025)))

# 3 A_purpose composites; non-initiators = 0 (= moderator-wide.r convention)
for (a in A_VARS) {
  v <- .row_mean(df, A_SPEC[[a]], na_rm = FALSE) - 1
  v[df$ai_init == 0L] <- 0
  df[[paste0(a, "_continuous")]] <- v
}

# 39 baseline-2024 covariates (identical set to moderator-wide.r)
df$baseline_ucla3 <- .row_sum_fn(df, paste0("Q66.", 1:3, "_2024"), fn = .ucla_recode)
df$baseline_k6    <- .row_sum_fn(df, paste0("Q65.", 1:6, "_2024"), fn = .k6_recode)
ace_cols <- intersect(paste0("Q77.", c(1:8,13), "_2024"), names(df))
if (length(ace_cols)) {
  ap <- vapply(df[ace_cols], function(v) as.integer(.as_num(v) == 1L), integer(nrow(df)))
  aps <- rowSums(ap, na.rm = TRUE)
  if ("Q77.9_2024" %in% names(df)) { q9 <- .as_num(df$Q77.9_2024); a9 <- as.integer(q9 == 2L); a9[is.na(q9)] <- 0L }
  else a9 <- 0L
  df$ace_score <- aps + a9
} else df$ace_score <- NA_real_
df$ace_0     <- as.integer(df$ace_score == 0L)   # ACE entered and reported as categories
df$ace_1     <- as.integer(df$ace_score == 1L)
df$ace_2_3   <- as.integer(df$ace_score %in% 2:3)
df$ace_4plus <- as.integer(df$ace_score >= 4L)

df$age_2024   <- .safe(df, "AGE_2024")
df$sex_female <- as.integer(.safe(df, "SEX_2024") == 2L)
edu24 <- .safe(df, "Q21.1_2024")
df$edu_univ <- as.integer(edu24 %in% 6:8); df$edu_grad <- as.integer(edu24 == 9L)
emp24 <- .safe(df, "Q5.1_2024")
df$emp_exec    <- as.integer(emp24 == 1L)
df$emp_self    <- as.integer(emp24 %in% 2:4)
df$emp_nonreg  <- as.integer(emp24 %in% 7:11)
df$emp_student <- as.integer(emp24 %in% 12:13)
df$emp_notwork <- as.integer(emp24 %in% 14:16 | is.na(emp24))
inc <- .safe(df, "Q80.1_2024")
df$income_2_6m     <- as.integer(inc %in% 5:8)
df$income_6_10m    <- as.integer(inc %in% 9:12)
df$income_10m_plus <- as.integer(inc %in% 13:18)
df$income_unknown  <- as.integer(is.na(inc) | inc %in% c(19L, 20L))
# income_continuous via Q80.1 midpoints (JPY million); codes 19-20 → NA.
.inc_midpt <- c(0.00, 0.25, 0.75, 1.50, 2.50, 3.50, 4.50, 5.50,
                 6.50, 7.50, 8.50, 9.50, 11.0, 13.0, 15.0, 17.0,
                 19.0, 22.5, NA_real_, NA_real_)
df$income_continuous <- ifelse(is.finite(inc) & inc >= 1L & inc <= 20L, .inc_midpt[as.integer(inc)], NA_real_)
df$married <- as.integer(.safe(df, "Q2_2024") %in% 1:3)
liv <- .safe(df, "Q1.1_2024"); df$living_alone <- as.integer(!is.na(liv) & liv == 1L)
df$baseline_lsns6_family  <- .row_sum_fn(df, paste0("Q17.", 1:3, "_2024"), fn = .lsns_recode)
df$baseline_lsns6_friends <- .row_sum_fn(df, paste0("Q17.", 4:6, "_2024"), fn = .lsns_recode)
df$mental_physical_health <- .row_mean(df, c("Q76.3_2024", "Q76.4_2024"))
df <- .td(df, "Q28.13_2024", "smartphone")
df <- .td(df, "Q28.14_2024", "pc_tablet")
df <- .td(df, "Q28.5_2024",  "sitting")
df <- .td(df, "Q28.6_2024",  "walking")

C_VARS <- c("age_2024","sex_female","edu_univ","edu_grad",
  "emp_exec","emp_self","emp_nonreg","emp_student","emp_notwork",
  "income_2_6m","income_6_10m","income_10m_plus","income_unknown",
  "married","living_alone",
  "baseline_lsns6_family","baseline_lsns6_friends","baseline_ucla3","baseline_k6",
  "ace_1","ace_2_3","ace_4plus","mental_physical_health",
  "smartphone_band_1_2","smartphone_band_3_4","smartphone_band_5plus","smartphone_unknown",
  "pc_tablet_band_1_2","pc_tablet_band_3_4","pc_tablet_band_5plus","pc_tablet_unknown",
  "sitting_band_1_2","sitting_band_3_4","sitting_band_5plus","sitting_unknown",
  "walking_band_1_2","walking_band_3_4","walking_band_5plus","walking_unknown")
stopifnot(length(C_VARS) == 39L)

# Cohort + analytic samples
ds <- df[ai_start %in% c(1L, 5L, 6L), , drop = FALSE]
n_cohort <- nrow(ds)
cc_mask <- complete.cases(ds[, c("Y_ucla3", A_CONT_VARS, C_VARS), drop = FALSE])
dm <- ds[cc_mask, , drop = FALSE]
log_msg(sprintf("Cohort {1,5,6} n = %d; complete-case n = %d (initiated %d / never %d)",
                n_cohort, nrow(dm), sum(dm$ai_init == 1L), sum(dm$ai_init == 0L)))

# Moderator factors (= moderator-wide.r §0; for diagnostic 5)
dm$age_band <- factor(ifelse(dm$age_2024 <= 39, "<=39",
                      ifelse(dm$age_2024 <= 64, "40-64", "65+")), levels = c("<=39","40-64","65+"))
dm$sex_cat  <- factor(ifelse(dm$sex_female == 1L, "Female", "Male"), levels = c("Male","Female"))
dm$edu_cat  <- factor(ifelse(dm$edu_grad == 1L, "Graduate",
                      ifelse(dm$edu_univ == 1L, "University", "Below univ")),
                      levels = c("Below univ","University","Graduate"))
# 4-level income (same as moderator-wide.r §0). The uncentered (c) VIF check
# below flags severe inflation on the income×A_p product terms; the centered
# (c2) check below shows it is non-essential (per-coef VIFs <10 with Aiken &
# West 1991 centering). 4-level preserves the substantive dose-response (the
# 10m+ A_PC sign-flip, the monotonic A_SE gradient across all 4 bands).
inc2 <- rep(NA_character_, nrow(dm))
inc2[dm$income_2_6m == 1L]     <- "2-6m"
inc2[dm$income_6_10m == 1L]    <- "6-10m"
inc2[dm$income_10m_plus == 1L] <- "10m+"
inc2[is.na(inc2) & dm$income_unknown == 0L] <- "<2m"
dm$income_cat  <- factor(inc2, levels = c("<2m","2-6m","6-10m","10m+"))
dm$friends_iso <- factor(ifelse(dm$baseline_lsns6_friends < 6, "Isolated", "Connected"), levels = c("Connected","Isolated"))
dm$family_iso  <- factor(ifelse(dm$baseline_lsns6_family  < 6, "Isolated", "Connected"), levels = c("Connected","Isolated"))

# ============================================================================
# Diagnostic 1 — Cohort flow
# ============================================================================
log_msg("\n[1/15] Cohort flow")
# the number of valid 2024 respondents (for the retention figure) when the all-2024 file is present
.n_valid_2024 <- if (file.exists(DATA_2024_ALL_PATH)) nrow(readr::read_csv(DATA_2024_ALL_PATH, show_col_types = FALSE, col_select = 1, guess_max = 30000)) else NA_integer_
flow_df <- data.frame(
  step = c("valid_2024_respondents", "raw_2wave_panel",
           "code1_never_user",
           "code2_pre_2024_user",
           "code3_during_2024_started",
           "code4_just_before_2025_started",
           "code5_started_2025_active",
           "code6_started_2025_inactive",
           "NA_Q37S1_2025",
           "cohort_kept_codes_1_5_6",
           "complete_case_Y_3A_39C",
           "initiated_(ai_init==1)",
           "never_user_(ai_init==0)"),
  n = c(.n_valid_2024, nrow(df),
        sum(ai_start == 1L, na.rm = TRUE),
        sum(ai_start == 2L, na.rm = TRUE),
        sum(ai_start == 3L, na.rm = TRUE),
        sum(ai_start == 4L, na.rm = TRUE),
        sum(ai_start == 5L, na.rm = TRUE),
        sum(ai_start == 6L, na.rm = TRUE),
        sum(is.na(ai_start)),
        n_cohort,
        nrow(dm),
        sum(dm$ai_init == 1L),
        sum(dm$ai_init == 0L)),
  stringsAsFactors = FALSE)
write_csv_out(flow_df, "01_cohort_flow")
for (i in seq_len(nrow(flow_df))) log_msg(sprintf("  %-30s n = %s", flow_df$step[i], ifelse(is.na(flow_df$n[i]), "NA", flow_df$n[i])))
if (nrow(dm) < 3000L) {
  add_flag("reject", "cohort CCA n", "n < 3000 (real-mode floor)",
           sprintf("only %d subjects in complete-case set", nrow(dm)))
}
if (sum(dm$ai_init == 1L) < 50L) {
  add_flag("warn", "initiators n", "n < 50",
           sprintf("only %d initiators in CCA — §1/§2/§3/§4 scans guard-skip in moderator-wide.r", sum(dm$ai_init == 1L)))
}

# ============================================================================
# Diagnostic 2 — Missingness (full cohort + initiator-only views)
# ============================================================================
log_msg("\n[2/15] Missingness (analytic vars; full cohort + initiator-only)")
miss_vars <- unique(c("Y_ucla3", A_CONT_VARS, unlist(A_SPEC, use.names = FALSE), C_VARS))
miss_vars <- intersect(miss_vars, names(ds))
miss_one <- function(d, scope) {
  do.call(rbind, lapply(miss_vars, function(v) {
    x <- d[[v]]; n_tot <- length(x); n_miss <- sum(is.na(x)); pct <- 100 * n_miss / max(n_tot, 1L)
    flag <- ifelse(pct > 10, "severe", ifelse(pct > 5, "mild", "ok"))
    data.frame(scope = scope, variable = v, n_total = n_tot, n_missing = n_miss,
               pct_missing = pct, flag = flag, stringsAsFactors = FALSE)
  }))
}
miss_full <- miss_one(ds,                              "full_cohort_{1,5,6}")
miss_init <- miss_one(ds[ds$ai_init == 1L, , drop=FALSE], "initiators_only")
miss_df   <- rbind(miss_full, miss_init)
miss_df   <- miss_df[order(miss_df$scope, -miss_df$pct_missing), , drop = FALSE]
write_csv_out(miss_df, "02_missingness")
sev_full <- miss_full[miss_full$flag == "severe", , drop = FALSE]
log_msg(sprintf("  full cohort: %d severe (>10%%), %d mild (5-10%%)",
                nrow(sev_full), sum(miss_full$flag == "mild")))
if (nrow(sev_full)) {
  for (i in seq_len(min(5, nrow(sev_full)))) {
    log_msg(sprintf("    severe: %-30s %5.2f%% missing",
                    sev_full$variable[i], sev_full$pct_missing[i]))
    add_flag("warn", sev_full$variable[i], "missingness > 10% (full cohort)",
             sprintf("%.1f%% missing", sev_full$pct_missing[i]))
  }
}

# ============================================================================
# Diagnostic 3 — Variable construction sanity (cohort CCA)
# ============================================================================
log_msg("\n[3/15] Variable construction sanity (CCA cohort)")
summ_cont <- function(x, varname) {
  n <- sum(is.finite(x))
  if (!n) return(data.frame(variable = varname, n = 0L, summary = "no values", stringsAsFactors = FALSE))
  data.frame(variable = varname, n = n,
             summary = sprintf("mean=%.3f SD=%.3f range=[%.2f, %.2f]",
                                mean(x, na.rm=TRUE), sd(x, na.rm=TRUE),
                                min(x, na.rm=TRUE), max(x, na.rm=TRUE)),
             stringsAsFactors = FALSE)
}
sanity_vars <- c("Y_ucla3", A_CONT_VARS,
                 "baseline_ucla3","baseline_k6","ace_score","mental_physical_health",
                 "baseline_lsns6_family","baseline_lsns6_friends","age_2024")
sanity_df <- do.call(rbind, lapply(sanity_vars, function(v) summ_cont(dm[[v]], v)))
write_csv_out(sanity_df, "03_variable_sanity")
for (i in seq_len(nrow(sanity_df))) log_msg(sprintf("  %-26s n=%5d  %s",
                                                    sanity_df$variable[i], sanity_df$n[i], sanity_df$summary[i]))
# Range checks
range_check <- function(x, varname, lo, hi) {
  x <- x[is.finite(x)]; if (!length(x)) return(invisible(NULL))
  if (min(x) < lo - 0.001 || max(x) > hi + 0.001) {
    add_flag("warn", varname, sprintf("out of expected [%g, %g]", lo, hi),
             sprintf("observed [%.2f, %.2f]", min(x), max(x)))
  }
}
for (a in A_CONT_VARS) range_check(dm[[a]], a, 0, 4)
range_check(dm$Y_ucla3, "Y_ucla3", 3, 12)
range_check(dm$baseline_ucla3, "baseline_ucla3", 3, 12)
range_check(dm$baseline_k6,    "baseline_k6",    0, 24)
range_check(dm$baseline_lsns6_friends, "baseline_lsns6_friends", 0, 15)
range_check(dm$baseline_lsns6_family,  "baseline_lsns6_family",  0, 15)

# ============================================================================
# Diagnostic 4 — Cronbach's α (no W subscales; this project)
# ============================================================================
log_msg("\n[4/15] Cronbach's α (internal consistency)")
# A composites: computed on ai_init==1 only (composite undefined for non-users)
alpha_rows <- list()
alpha_add <- function(label, items, data) {
  out <- .alpha_for(items, data)
  flag <- if (!is.finite(out$alpha)) "n/a"
          else if (out$alpha < 0.60) "low"
          else if (out$alpha < 0.70) "marginal"
          else                       "ok"
  alpha_rows[[length(alpha_rows) + 1L]] <<- data.frame(
    scale = label, n_items = length(items), n_subjects = out$n_eff,
    raw_alpha = out$alpha, flag = flag, stringsAsFactors = FALSE)
  if (flag == "low") add_flag("warn", label, "α < 0.60",
                              sprintf("raw α = %.3f over %d items / %d subjects",
                                      out$alpha, length(items), out$n_eff))
}
ds_init <- ds[ds$ai_init == 1L, , drop = FALSE]
alpha_add("A_SE (Q37S3.8-9, initiators)",    A_SPEC$A_SE, ds_init)
alpha_add("A_PC (Q37S3.1,2,4,5, initiators)", A_SPEC$A_PC, ds_init)
alpha_add("A_DI (Q37S3.3,6,7, initiators)",   A_SPEC$A_DI, ds_init)
alpha_add("UCLA-3 baseline 2024 (cohort)", paste0("Q66.", 1:3, "_2024"), ds)
alpha_add("UCLA-3 outcome 2025 (cohort)",  paste0("Q66.", 1:3, "_2025"), ds)
alpha_add("K6 baseline 2024 (cohort)",      paste0("Q65.", 1:6, "_2024"), ds)
alpha_add("LSNS-friends 2024 (cohort)",     paste0("Q17.", 4:6, "_2024"), ds)
alpha_add("LSNS-family 2024 (cohort)",      paste0("Q17.", 1:3, "_2024"), ds)
alpha_add("ACE 10-item 2024 (informational)",
          c(paste0("Q77.", c(1:8,13), "_2024"), "Q77.9_2024"), ds)
alpha_df <- do.call(rbind, alpha_rows)
write_csv_out(alpha_df, "04_cronbach_alpha")
for (i in seq_len(nrow(alpha_df))) {
  a <- alpha_df$raw_alpha[i]
  log_msg(sprintf("  %-44s items=%2d n=%5d α=%s (%s)",
                  alpha_df$scale[i], alpha_df$n_items[i], alpha_df$n_subjects[i],
                  if (is.finite(a)) sprintf("%.3f", a) else "  n/a", alpha_df$flag[i]))
}

# ============================================================================
# Diagnostic 5 — Moderator-level distribution (4 moderators × ai_init)
# ============================================================================
log_msg("\n[5/15] Moderator-level distribution (n by ai_init within each level)")
mod_factors <- c("age_band","sex_cat","friends_iso","family_iso")   # demographics + isolation
# dropped edu_cat, income_cat, emp_status, k6_serious from D5 focal report;
# income_cat is still distributed-described elsewhere (kept in C). Source variables stay in C.
mod_rows <- list()
for (mc in mod_factors) {
  fac <- dm[[mc]]; lv <- levels(fac); is_na <- is.na(fac)
  for (L in lv) {
    n_total <- sum(fac == L, na.rm = TRUE)
    n_init  <- sum(fac == L & dm$ai_init == 1L, na.rm = TRUE)
    n_never <- sum(fac == L & dm$ai_init == 0L, na.rm = TRUE)
    init_rate <- if (n_total > 0L) 100 * n_init / n_total else NA_real_
    flag <- if (n_total < 50L) "tiny" else if (n_total < 200L) "small" else "ok"
    if (n_total < 50L) add_flag("warn", sprintf("%s = %s", mc, L), "n < 50",
                                sprintf("stratum size %d (init=%d, never=%d)", n_total, n_init, n_never))
    mod_rows[[length(mod_rows)+1L]] <- data.frame(
      moderator = mc, level = L, n_total = n_total, n_initiated = n_init, n_never = n_never,
      init_rate_pct = init_rate, flag = flag, stringsAsFactors = FALSE)
  }
  if (any(is_na)) mod_rows[[length(mod_rows)+1L]] <- data.frame(
      moderator = mc, level = "<NA>", n_total = sum(is_na),
      n_initiated = sum(is_na & dm$ai_init == 1L),
      n_never = sum(is_na & dm$ai_init == 0L),
      init_rate_pct = NA_real_, flag = "dropped_in_scan", stringsAsFactors = FALSE)
}
mod_df <- do.call(rbind, mod_rows)
write_csv_out(mod_df, "05_moderator_distribution")
for (i in seq_len(nrow(mod_df))) {
  log_msg(sprintf("  %-12s %-12s n=%5d (init=%4d, never=%4d)  init_rate=%s  [%s]",
                  mod_df$moderator[i], mod_df$level[i], mod_df$n_total[i],
                  mod_df$n_initiated[i], mod_df$n_never[i],
                  if (is.finite(mod_df$init_rate_pct[i])) sprintf("%5.1f%%", mod_df$init_rate_pct[i]) else "   n/a",
                  mod_df$flag[i]))
}

# ============================================================================
# Diagnostic 6 — A descriptive + crude A→Y (full cohort + initiators-only)
# ============================================================================
log_msg("\n[6/15] A descriptive + crude A→Y (two scopes)")
a_desc_rows <- list()
do_scope <- function(d, scope) {
  log_msg(sprintf("  -- scope: %s (n=%d) --", scope, nrow(d)))
  # exposures may need re-centering for initiators-only (intensive margin)
  d <- d
  for (a in A_CONT_VARS) d[[paste0(sub("_continuous$","_c",a))]] <- d[[a]] - mean(d[[a]], na.rm = TRUE)
  # Inter-A correlations
  A_mat <- as.matrix(d[, A_CONT_VARS, drop = FALSE]); mode(A_mat) <- "numeric"
  A_cor <- suppressWarnings(cor(A_mat, use = "pairwise.complete.obs"))
  for (i in seq_along(A_CONT_VARS)) for (j in seq_along(A_CONT_VARS)) if (i < j) {
    a_desc_rows[[length(a_desc_rows)+1L]] <<- data.frame(
      scope = scope, type = "inter_A_correlation",
      var1 = A_CONT_VARS[i], var2 = A_CONT_VARS[j],
      value = A_cor[i, j], se = NA_real_, p = NA_real_,
      stringsAsFactors = FALSE)
  }
  if (any(A_cor[upper.tri(A_cor)] > 0.90, na.rm = TRUE)) {
    add_flag("warn", sprintf("A triple [%s]", scope), "inter-A r > 0.90",
             sprintf("max r = %.3f", max(A_cor[upper.tri(A_cor)], na.rm = TRUE)))
  }
  # Crude per-A→Y
  for (a in A_CONT_VARS) {
    sub <- d[is.finite(d[[a]]) & is.finite(d$Y_ucla3), c(a, "Y_ucla3"), drop = FALSE]
    if (nrow(sub) < 20L || length(unique(sub[[a]])) < 2L) {
      log_msg(sprintf("  crude %s → Y SKIP (n=%d, levels=%d)",
                      a, nrow(sub), length(unique(sub[[a]]))))
      next
    }
    fit <- lm(as.formula(paste0("Y_ucla3 ~ ", a)), data = sub)
    b <- unname(coef(fit)[2]); s <- sqrt(diag(vcov(fit)))[2]; p <- 2 * pnorm(-abs(b/s))
    a_desc_rows[[length(a_desc_rows)+1L]] <<- data.frame(
      scope = scope, type = "crude_A_to_Y",
      var1 = a, var2 = "Y_ucla3", value = b, se = s, p = p,
      stringsAsFactors = FALSE)
    log_msg(sprintf("  crude %s → Y: β=%+.4f (SE=%.4f, p=%.3g, n=%d)  %s",
                    a, b, s, p, nrow(sub), if (b > 0) "lonelier" else "less lonely"))
  }
}
do_scope(dm,                                "full_cohort_non_users_0")
n_init <- sum(dm$ai_init == 1L)
if (n_init >= 50L) {
  do_scope(dm[dm$ai_init == 1L, , drop = FALSE], "initiators_only_intensive")
} else {
  log_msg(sprintf("  users-only scope SKIP (n=%d < 50).", n_init))
}
a_desc_df <- do.call(rbind, a_desc_rows)
write_csv_out(a_desc_df, "06_A_descriptive")

# ============================================================================
# Diagnostic 7 — A_p non-zero distribution + §4 cutpoint feasibility
# ============================================================================
# prep. For each purpose, examine the non-zero distribution among
# initiators and decide the §4 categorical coding:
#   tertile_4level (0 / T1 / T2 / T3) — if 1/3 and 2/3 quantiles are well separated
#   median_3level  (0 / low / high)   — if tertiles collapse but median splits
#   binary_2level  (0 / any)          — if even median collapses
# Logged + saved so moderator-wide.r §4 produces matching cutpoints.
log_msg("\n[7/15] A_p non-zero distribution + §4 cutpoint feasibility")
cut_rows <- list()
decide_cut <- function(a, dm_init) {
  x <- dm_init[[a]]; nz <- x[is.finite(x) & x > 0]
  n_nz <- length(nz)
  if (n_nz < 50L) {
    log_msg(sprintf("  [%s] n_nonzero=%d < 50 — §4 SKIP for this purpose", a, n_nz))
    return(data.frame(purpose = sub("_continuous$","", a), n_nonzero = n_nz,
                      q33 = NA_real_, q50 = NA_real_, q67 = NA_real_,
                      decision = "skip_too_few_nonzero", n_zero = NA_integer_,
                      n_T1 = NA_integer_, n_T2 = NA_integer_, n_T3 = NA_integer_,
                      stringsAsFactors = FALSE))
  }
  q33 <- unname(quantile(nz, 1/3, type = 7))
  q50 <- unname(quantile(nz, 0.5, type = 7))
  q67 <- unname(quantile(nz, 2/3, type = 7))
  q0  <- min(nz); qM <- max(nz)
  log_msg(sprintf("  [%s] n_nonzero=%d  range [%.3f, %.3f]  q33=%.3f q50=%.3f q67=%.3f",
                  a, n_nz, q0, qM, q33, q50, q67))
  decision <- if (q33 < q67 - 1e-8) "tertile_4level"
              else if (q50 > q0 + 1e-8) "median_3level"
              else "binary_2level"
  # Counts that the chosen coding would yield on the full {1,5,6} cohort
  x_full <- dm[[a]]; n_zero <- sum(x_full == 0, na.rm = TRUE)
  if (decision == "tertile_4level") {
    n_T1 <- sum(x_full > 0 & x_full <= q33, na.rm = TRUE)
    n_T2 <- sum(x_full > q33 & x_full <= q67, na.rm = TRUE)
    n_T3 <- sum(x_full > q67, na.rm = TRUE)
  } else if (decision == "median_3level") {
    n_T1 <- sum(x_full > 0 & x_full <= q50, na.rm = TRUE)
    n_T2 <- sum(x_full > q50, na.rm = TRUE); n_T3 <- NA_integer_
    log_msg(sprintf("    -> tertile collapse (q33==q67=%.3f); fall back to median 3-level", q33))
    add_flag("warn", sprintf("§4 %s", sub("_continuous$","",a)),
             "tertile collapse",
             sprintf("q33=q67=%.3f → median 3-level fallback (0/low/high)", q33))
  } else {
    n_T1 <- sum(x_full > 0, na.rm = TRUE); n_T2 <- NA_integer_; n_T3 <- NA_integer_
    log_msg(sprintf("    -> median collapse too; fall back to binary 0/any"))
    add_flag("warn", sprintf("§4 %s", sub("_continuous$","",a)),
             "binary fallback",
             "median == min(nonzero); §4 reduces to 0/any contrast for this purpose")
  }
  data.frame(purpose = sub("_continuous$","", a), n_nonzero = n_nz,
             q33 = q33, q50 = q50, q67 = q67, decision = decision,
             n_zero = n_zero, n_T1 = n_T1, n_T2 = n_T2, n_T3 = n_T3,
             stringsAsFactors = FALSE)
}
dm_init <- dm[dm$ai_init == 1L, , drop = FALSE]
if (nrow(dm_init) < 50L) {
  log_msg(sprintf("  initiator subset n=%d < 50 — cutpoint diagnostic SKIP", nrow(dm_init)))
} else {
  for (a in A_CONT_VARS) cut_rows[[length(cut_rows)+1L]] <- decide_cut(a, dm_init)
}
if (length(cut_rows)) {
  cut_df <- do.call(rbind, cut_rows); write_csv_out(cut_df, "07_A_cutpoints")
}

# ============================================================================
# Diagnostic 8 — VIF on three targets (main-effects, friends_iso, income)
# ============================================================================
log_msg("\n[8/15] VIF on three §1 fits")
# (a) Unmoderated 3-A main-effects fit (confounder-set health)
log_msg("  (a) main-effects fit: Y ~ A_SE + A_PC + A_DI + C")
fit_main <- tryCatch(lm(as.formula(paste0(OUTCOME, " ~ ",
                                paste(c(A_CONT_VARS, C_VARS), collapse = " + "))),
                       data = dm),
                    error = function(e) { log_msg("  fit failed:", conditionMessage(e)); NULL })
if (!is.null(fit_main)) {
  v <- .safe_vif(fit_main, dm)
  if (!is.null(v)) {
    vdf <- data.frame(predictor = names(v), vif = unname(v), flag = .vif_flag(unname(v)),
                      stringsAsFactors = FALSE)
    vdf <- vdf[order(-vdf$vif), , drop = FALSE]
    write_csv_out(vdf, "08_vif_main_effects")
    n_sev <- sum(vdf$flag == "severe")
    log_msg(sprintf("    severe=%d, mild=%d, ok=%d (top 5: %s)",
                    n_sev, sum(vdf$flag == "mild"), sum(vdf$flag == "ok"),
                    paste(sprintf("%s=%.1f", utils::head(vdf$predictor, 5),
                                  utils::head(vdf$vif, 5)), collapse = ", ")))
    if (n_sev > 0L) add_flag("warn", "VIF main-effects", "≥1 predictor VIF > 10",
                              sprintf("%d severe; top: %s (VIF=%.1f)",
                                      n_sev, vdf$predictor[1], vdf$vif[1]))
  }
}
# (b) §1 friends_iso fit (the borderline finding)
log_msg("  (b) §1 friends_iso fit: Y ~ A_p_c + friends_iso + A_p_c:friends_iso + C(-LSNS-fri)")
if (nrow(dm) >= 50L && sum(dm$ai_init == 1L) >= 50L) {
  for (a in A_CONT_VARS) dm[[sub("_continuous$","_c", a)]] <- dm[[a]] - mean(dm[[a]])
  A_CENT_VARS <- paste0(A_VARS, "_c")
  cv <- setdiff(C_VARS, "baseline_lsns6_friends")
  cv <- cv[vapply(cv, function(v) length(unique(dm[[v]])) > 1L, logical(1))]
  rhs <- paste(c(A_CENT_VARS, "friends_iso", paste0(A_CENT_VARS, ":friends_iso"), cv), collapse = " + ")
  fit_fi <- tryCatch(lm(as.formula(paste0(OUTCOME, " ~ ", rhs)), data = dm),
                     error = function(e) { log_msg("  fit failed:", conditionMessage(e)); NULL })
  if (!is.null(fit_fi)) {
    v <- .safe_vif(fit_fi, dm)
    if (!is.null(v)) {
      vmat <- if (is.matrix(v)) v else cbind(GVIF = v, Df = 1, `GVIF^(1/(2*Df))` = sqrt(v))
      vdf <- data.frame(predictor = rownames(vmat),
                        vif = unname(vmat[, ncol(vmat)]^2),
                        gvif = unname(vmat[, 1]),
                        flag = .vif_flag(unname(vmat[, 1])),
                        stringsAsFactors = FALSE)
      vdf <- vdf[order(-vdf$gvif), , drop = FALSE]
      write_csv_out(vdf, "08_vif_friends_iso")
      log_msg(sprintf("    severe=%d (top: %s, GVIF=%.1f)",
                      sum(vdf$flag == "severe"), vdf$predictor[1], vdf$gvif[1]))
    }
  }
} else log_msg("    SKIP friends_iso VIF (insufficient initiators).")
# (b2) §1 friends_iso fit, CENTERED dummy (Aiken & West 1991; removes non-essential
# multicollinearity from products with uncentered binary moderators)
log_msg("  (b2) §1 friends_iso fit, CENTERED: A_p_c × (friends_iso_indicator - mean)")
if (nrow(dm) >= 50L && sum(dm$ai_init == 1L) >= 50L) {
  dm$friends_iso_c01 <- as.integer(dm$friends_iso == "Isolated") -
                        mean(as.integer(dm$friends_iso == "Isolated"))
  cv <- setdiff(C_VARS, "baseline_lsns6_friends")
  cv <- cv[vapply(cv, function(v) length(unique(dm[[v]])) > 1L, logical(1))]
  rhs_c <- paste(c(A_CENT_VARS, "friends_iso_c01",
                   paste0(A_CENT_VARS, ":friends_iso_c01"), cv), collapse = " + ")
  fit_fi_c <- tryCatch(lm(as.formula(paste0(OUTCOME, " ~ ", rhs_c)), data = dm),
                       error = function(e) { log_msg("  fit failed:", conditionMessage(e)); NULL })
  if (!is.null(fit_fi_c)) {
    v <- .safe_vif(fit_fi_c, dm)
    if (!is.null(v)) {
      v_num   <- if (is.matrix(v)) unname(v[, 1]) else unname(v)
      v_names <- if (is.matrix(v)) rownames(v)   else names(v)
      vdf <- data.frame(predictor = v_names, vif = v_num,
                        flag = .vif_flag(v_num), stringsAsFactors = FALSE)
      vdf <- vdf[order(-vdf$vif), , drop = FALSE]
      write_csv_out(vdf, "08_vif_friends_iso_centered")
      log_msg(sprintf("    [centered] severe=%d, mild=%d, ok=%d (top: %s=%.2f)",
                      sum(vdf$flag == "severe"), sum(vdf$flag == "mild"),
                      sum(vdf$flag == "ok"), vdf$predictor[1], vdf$vif[1]))
    }
  }
} else log_msg("    SKIP friends_iso centered VIF (insufficient initiators).")
# (c) §1 income fit (largest df moderator)
log_msg("  (c) §1 income fit: Y ~ A_p_c + income_cat + A_p_c:income_cat + C(-income dummies)")
if (nrow(dm) >= 50L && sum(dm$ai_init == 1L) >= 50L) {
  d_inc <- dm[!is.na(dm$income_cat), , drop = FALSE]
  cv <- setdiff(C_VARS, c("income_2_6m","income_6_10m","income_10m_plus","income_unknown"))
  cv <- cv[vapply(cv, function(v) length(unique(d_inc[[v]])) > 1L, logical(1))]
  rhs <- paste(c(A_CENT_VARS, "income_cat", paste0(A_CENT_VARS, ":income_cat"), cv), collapse = " + ")
  fit_in <- tryCatch(lm(as.formula(paste0(OUTCOME, " ~ ", rhs)), data = d_inc),
                     error = function(e) { log_msg("  fit failed:", conditionMessage(e)); NULL })
  if (!is.null(fit_in)) {
    v <- .safe_vif(fit_in, d_inc)
    if (!is.null(v)) {
      vmat <- if (is.matrix(v)) v else cbind(GVIF = v, Df = 1, `GVIF^(1/(2*Df))` = sqrt(v))
      vdf <- data.frame(predictor = rownames(vmat),
                        vif = unname(vmat[, ncol(vmat)]^2),
                        gvif = unname(vmat[, 1]),
                        flag = .vif_flag(unname(vmat[, 1])),
                        stringsAsFactors = FALSE)
      vdf <- vdf[order(-vdf$gvif), , drop = FALSE]
      write_csv_out(vdf, "08_vif_income")
      log_msg(sprintf("    severe=%d (top: %s, GVIF=%.1f)",
                      sum(vdf$flag == "severe"), vdf$predictor[1], vdf$gvif[1]))
    }
  }
} else log_msg("    SKIP income VIF (insufficient initiators).")
# (c2) §1 income fit, CENTERED dummies (Aiken & West; 3 centered dummies for 4-level income → scalar VIFs)
log_msg("  (c2) §1 income fit, CENTERED: A_p_c × (each income-level indicator − mean), reference = <2m")
if (nrow(dm) >= 50L && sum(dm$ai_init == 1L) >= 50L) {
  d_inc <- dm[!is.na(dm$income_cat), , drop = FALSE]
  d_inc$inc_26m_c01  <- as.integer(d_inc$income_cat == "2-6m")  - mean(as.integer(d_inc$income_cat == "2-6m"))
  d_inc$inc_610m_c01 <- as.integer(d_inc$income_cat == "6-10m") - mean(as.integer(d_inc$income_cat == "6-10m"))
  d_inc$inc_10p_c01  <- as.integer(d_inc$income_cat == "10m+")  - mean(as.integer(d_inc$income_cat == "10m+"))
  inc_dummies_c <- c("inc_26m_c01","inc_610m_c01","inc_10p_c01")
  cv <- setdiff(C_VARS, c("income_2_6m","income_6_10m","income_10m_plus","income_unknown"))
  cv <- cv[vapply(cv, function(v) length(unique(d_inc[[v]])) > 1L, logical(1))]
  rhs_c <- paste(c(A_CENT_VARS, inc_dummies_c,
                   unlist(lapply(inc_dummies_c, function(d) paste0(A_CENT_VARS, ":", d))),
                   cv), collapse = " + ")
  fit_in_c <- tryCatch(lm(as.formula(paste0(OUTCOME, " ~ ", rhs_c)), data = d_inc),
                       error = function(e) { log_msg("  fit failed:", conditionMessage(e)); NULL })
  if (!is.null(fit_in_c)) {
    v <- .safe_vif(fit_in_c, d_inc)
    if (!is.null(v)) {
      v_num   <- if (is.matrix(v)) unname(v[, 1]) else unname(v)
      v_names <- if (is.matrix(v)) rownames(v)   else names(v)
      vdf <- data.frame(predictor = v_names, vif = v_num,
                        flag = .vif_flag(v_num), stringsAsFactors = FALSE)
      vdf <- vdf[order(-vdf$vif), , drop = FALSE]
      write_csv_out(vdf, "08_vif_income_centered")
      log_msg(sprintf("    [centered] severe=%d, mild=%d, ok=%d (top: %s=%.2f)",
                      sum(vdf$flag == "severe"), sum(vdf$flag == "mild"),
                      sum(vdf$flag == "ok"), vdf$predictor[1], vdf$vif[1]))
    }
  }
} else log_msg("    SKIP income centered VIF (insufficient initiators).")

# ============================================================================
# Diagnostic 9 — Straight-lining on the 9 Q37S3 A_p items among initiators
# ============================================================================
log_msg("\n[9/15] Straight-lining on the 9 Q37S3 A_p items (initiators only)")
all_A_items <- unlist(A_SPEC, use.names = FALSE)
items_avail <- intersect(all_A_items, names(ds_init))
sl_df <- NULL
if (length(items_avail) < 2L || nrow(ds_init) < 50L) {
  log_msg(sprintf("  SKIP (items_avail=%d, n_init=%d)", length(items_avail), nrow(ds_init)))
} else {
  m <- as.matrix(ds_init[, items_avail, drop = FALSE]); mode(m) <- "numeric"
  m <- m[complete.cases(m), , drop = FALSE]; n_cc <- nrow(m)
  if (n_cc < 10L) {
    log_msg(sprintf("  SKIP (n_cc=%d after CCA on %d items)", n_cc, length(items_avail)))
  } else {
    is_flat <- apply(m, 1L, function(r) length(unique(r)) == 1L)
    n_flat <- sum(is_flat); pct <- 100 * n_flat / n_cc
    vals <- if (n_flat > 0L) m[is_flat, 1L] else numeric(0)
    val_tab <- if (n_flat > 0L) paste(sprintf("%s:%d", names(table(vals)), as.integer(table(vals))),
                                       collapse = ", ") else "none"
    log_msg(sprintf("  %d/%d initiator-CCA (%.1f%%) gave one identical value across all %d items; by value: %s",
                    n_flat, n_cc, pct, length(items_avail), val_tab))
    if (pct > 5) add_flag("warn", "straight-lining A items",
                          "identical-response respondents > 5%",
                          sprintf("%d/%d (%.1f%%) flat across %d Q37S3 items", n_flat, n_cc, pct, length(items_avail)))
    sl_df <- data.frame(item_set = "Q37S3_9items_initiators", n_items = length(items_avail),
                        n_complete = n_cc, n_straightline = n_flat,
                        pct_straightline = round(pct, 2), by_value = val_tab,
                        stringsAsFactors = FALSE)
    write_csv_out(sl_df, "09_straightlining")
  }
}

# ============================================================================
# Diagnostic 10 — EFA on the 9 AI-use purpose items (Q37S3) among initiators
# Validates the A_SE / A_PC / A_DI 3-purpose structure (→ Sup Table 1). complete.cases on
# the 9 raw items auto-restricts to initiators (never-users are NA on Q37S3).
# Oblique (oblimin) minres EFA; factor retention by Horn's parallel analysis.
# ============================================================================
run_efa_block <- function(efa_items, tag, label, n_theory, item_map, cor_type = c("cor", "poly"), data = ds) {
  # cor_type = "poly" runs the identical EFA on the polychoric correlation matrix (ordered
  # five-point items); "cor" is the Pearson solution. `data` allows another sample to be
  # analysed with the same function.
  cor_type <- match.arg(cor_type)
  efa_items <- intersect(efa_items, names(data))
  if (length(efa_items) < 3L) { log_msg(sprintf("  [%s] EFA SKIP: <3 items present (%d).", tag, length(efa_items)))
    add_flag("warn", sprintf("EFA %s", tag), "too few items", sprintf("found %d", length(efa_items))); return(invisible(FALSE)) }
  m <- as.matrix(data[, efa_items, drop = FALSE]); mode(m) <- "numeric"; m <- m[complete.cases(m), , drop = FALSE]
  if (nrow(m) < 100L) { log_msg(sprintf("  [%s] EFA SKIP: n_eff=%d < 100.", tag, nrow(m)))
    add_flag("warn", sprintf("EFA %s", tag), "n_eff < 100", sprintf("only %d complete cases", nrow(m))); return(invisible(FALSE)) }
  # correlation matrix written out: Pearson or polychoric
  Rm <- if (cor_type == "poly") tryCatch(suppressWarnings(suppressMessages(psych::polychoric(m, correct = 0)$rho)), error = function(e) NULL) else cor(m)
  if (is.null(Rm)) { log_msg(sprintf("  [%s] EFA SKIP: polychoric correlations failed.", tag)); return(invisible(FALSE)) }
  # Parallel analysis and the EFA are run on the RAW item matrix (psych computes the correlation
  # internally; `cor = "poly"` requests the polychoric matrix).
  .fa_cor <- if (cor_type == "poly") "poly" else "cor"
  set.seed(20260524)   # fa.parallel resamples; fixed seed for reproducible eigenvalues
  pa <- tryCatch({ invisible(capture.output(pa0 <- suppressWarnings(suppressMessages(psych::fa.parallel(m, fa = "fa", fm = "minres", plot = FALSE, n.iter = 20, cor = .fa_cor, correct = 0))))); pa0 }, error = function(e) NULL)
  nfact_suggest <- if (!is.null(pa) && is.finite(pa$nfact)) max(1L, as.integer(pa$nfact)) else NA_integer_
  max_fac <- max(1L, length(efa_items) %/% 3L)
  nfact_use <- if (is.na(nfact_suggest)) min(n_theory, max_fac) else min(max(nfact_suggest, 1L), max_fac)
  log_msg(sprintf("  [%s] parallel analysis suggests %s factors; fitting EFA with %d (%s).", tag,
                  ifelse(is.na(nfact_suggest), "NA", as.character(nfact_suggest)), nfact_use, label))
  if (!is.null(pa)) write.csv(data.frame(factor = seq_along(pa$fa.values), eigen_actual = pa$fa.values,
    eigen_resampled = if (!is.null(pa$fa.sim)) pa$fa.sim else NA_real_, nfact_suggested = nfact_suggest, stringsAsFactors = FALSE),
    file.path(OUT_BASE, sprintf("%s_parallel.csv", tag)), row.names = FALSE, fileEncoding = "UTF-8")
  rotate_use <- if (requireNamespace("GPArotation", quietly = TRUE)) "oblimin" else "varimax"
  fit_fa <- tryCatch(suppressWarnings(suppressMessages(psych::fa(m, nfactors = nfact_use, rotate = rotate_use, fm = "minres", cor = .fa_cor, correct = 0))),
                     error = function(e) { log_msg(sprintf("  [%s] fa() failed: %s", tag, conditionMessage(e))); NULL })
  if (is.null(fit_fa)) { log_msg(sprintf("  [%s] EFA produced no fit.", tag)); return(invisible(FALSE)) }
  kmo  <- tryCatch(suppressWarnings(suppressMessages(psych::KMO(Rm))), error = function(e) NULL)
  bart <- tryCatch(suppressWarnings(suppressMessages(psych::cortest.bartlett(Rm, n = nrow(m)))), error = function(e) NULL)
  msai <- if (!is.null(kmo) && !is.null(kmo$MSAi)) kmo$MSAi else setNames(rep(NA_real_, ncol(m)), colnames(m))
  h2 <- fit_fa$communality; u2 <- fit_fa$uniquenesses; cmplx <- fit_fa$complexity
  L <- unclass(fit_fa$loadings); Lm <- matrix(as.numeric(L), nrow = nrow(L), dimnames = dimnames(L))
  fac_names <- colnames(Lm)
  load_df <- data.frame(item = rownames(Lm), theoretical_group = item_map[rownames(Lm)], stringsAsFactors = FALSE)
  for (j in seq_len(ncol(Lm))) load_df[[fac_names[j]]] <- round(Lm[, j], 3)
  load_df$top_factor <- fac_names[apply(abs(Lm), 1, which.max)]
  load_df$h2         <- round(unname(h2[rownames(Lm)]), 3)
  load_df$u2         <- round(unname(u2[rownames(Lm)]), 3)
  load_df$complexity <- round(unname(cmplx[rownames(Lm)]), 3)
  load_df$MSA_item   <- round(unname(msai[rownames(Lm)]), 3)
  write.csv(load_df, file.path(OUT_BASE, sprintf("%s_loadings.csv", tag)), row.names = FALSE, fileEncoding = "UTF-8")
  va <- fit_fa$Vaccounted
  write.csv(data.frame(factor = colnames(va), SS_loadings = va["SS loadings", ], prop_var = va["Proportion Var", ],
    cum_var = va["Cumulative Var", ], stringsAsFactors = FALSE),
    file.path(OUT_BASE, sprintf("%s_variance.csv", tag)), row.names = FALSE, fileEncoding = "UTF-8")
  phi <- fit_fa$Phi
  if (!is.null(phi) && is.matrix(phi) && nrow(phi) > 1L) {
    phi_m <- matrix(as.numeric(phi), nrow = nrow(phi), dimnames = dimnames(phi))
    write.csv(data.frame(factor = rownames(phi_m), round(phi_m, 3), check.names = FALSE, stringsAsFactors = FALSE),
              file.path(OUT_BASE, sprintf("%s_factor_cor.csv", tag)), row.names = FALSE, fileEncoding = "UTF-8")
    off <- phi_m[upper.tri(phi_m)]
    log_msg(sprintf("  [%s] oblique factor correlations: mean |r|=%.2f, max |r|=%.2f (%s).", tag, mean(abs(off)), max(abs(off)), rotate_use))
  } else log_msg(sprintf("  [%s] no factor-correlation matrix (orthogonal rotation or single factor).", tag))
  cum_var_use <- as.numeric(va["Cumulative Var", ncol(va)])
  getf <- function(x, i = 1L) if (!is.null(x) && length(x) >= i && is.finite(x[i])) as.numeric(x[i]) else NA_real_
  fit_kv <- data.frame(
    metric = c("correlation_type", "n_obs", "n_items", "n_factors", "KMO_overall",
               "bartlett_chisq", "bartlett_df", "bartlett_p",
               "model_chisq", "model_chisq_df", "model_chisq_p",
               "RMSEA", "RMSEA_lower", "RMSEA_upper", "TLI", "RMSR", "BIC",
               "cum_var_pct", "fit_offdiag"),
    value = c(if (cor_type == "poly") "polychoric" else "Pearson", nrow(m), ncol(m), nfact_use,
              if (!is.null(kmo)) round(getf(kmo$MSA), 3) else NA_real_,
              if (!is.null(bart)) round(getf(bart$chisq), 2) else NA_real_,
              if (!is.null(bart)) getf(bart$df) else NA_real_,
              if (!is.null(bart)) signif(getf(bart$p.value), 3) else NA_real_,
              round(getf(fit_fa$STATISTIC), 2), getf(fit_fa$dof), signif(getf(fit_fa$PVAL), 3),
              round(getf(fit_fa$RMSEA, 1L), 3), round(getf(fit_fa$RMSEA, 2L), 3), round(getf(fit_fa$RMSEA, 3L), 3),
              round(getf(fit_fa$TLI), 3), round(getf(fit_fa$rms), 3), round(getf(fit_fa$BIC), 1),
              round(100 * cum_var_use, 1), round(getf(fit_fa$fit.off), 3)),
    stringsAsFactors = FALSE)
  write.csv(fit_kv, file.path(OUT_BASE, sprintf("%s_fit.csv", tag)), row.names = FALSE, fileEncoding = "UTF-8")
  log_msg(sprintf("  [%s] EFA (%d factors, %s): cum var=%.1f%%; KMO=%s; Bartlett p=%s; RMSEA=%.3f; TLI=%.3f; RMSR=%.3f; items->%d top-factors (vs %d groups).",
                  tag, nfact_use, rotate_use, 100 * cum_var_use,
                  if (!is.null(kmo)) sprintf("%.2f", getf(kmo$MSA)) else "NA",
                  if (!is.null(bart)) signif(getf(bart$p.value), 3) else "NA",
                  getf(fit_fa$RMSEA, 1L), getf(fit_fa$TLI), getf(fit_fa$rms),
                  length(unique(load_df$top_factor)), n_theory))
  if (!is.null(kmo) && is.finite(getf(kmo$MSA)) && getf(kmo$MSA) < 0.60)
    add_flag("warn", sprintf("EFA %s KMO", tag), "KMO < 0.60 (mediocre sampling adequacy)", sprintf("KMO = %.2f", getf(kmo$MSA)))
  if (!is.null(bart) && is.finite(getf(bart$p.value)) && getf(bart$p.value) >= 0.05)
    add_flag("warn", sprintf("EFA %s Bartlett", tag), "Bartlett p >= 0.05 (sphericity not rejected)", sprintf("p = %.3g", getf(bart$p.value)))
  .lowh2 <- names(h2)[is.finite(h2) & h2 < 0.30]
  if (length(.lowh2))
    add_flag("warn", sprintf("EFA %s communalities", tag), "item communality h2 < 0.30",
             paste(sprintf("%s h2=%.2f", .lowh2, h2[.lowh2]), collapse = "; "))
  invisible(TRUE)
}
log_msg("\n[10/15] EFA on the 9 AI-use purpose items (Q37S3): A_SE / A_PC / A_DI (initiators)")
if (!requireNamespace("psych", quietly = TRUE)) {
  log_msg("  EFA SKIP: 'psych' not installed.")
  add_flag("warn", "EFA purposes", "psych not installed", "install.packages(c('psych','GPArotation'))")
} else {
  A_ITEM_MAP <- setNames(rep(names(A_SPEC), lengths(A_SPEC)), unlist(A_SPEC, use.names = FALSE))
  run_efa_block(unlist(A_SPEC, use.names = FALSE), "10_efa_purposes", "3 AI-use purposes (9 items)",
                n_theory = length(A_SPEC), item_map = A_ITEM_MAP)                       # Pearson solution
  run_efa_block(unlist(A_SPEC, use.names = FALSE), "10_efa_purposes_poly", "3 AI-use purposes (9 items; polychoric)",
                n_theory = length(A_SPEC), item_map = A_ITEM_MAP, cor_type = "poly")    # polychoric solution
}

# ============================================================================
# Diagnostic 11 — holdout check of the three-factor structure (CFA, WLSMV)
# The respondents who initiated generative-AI use before 2025 (Q37S1 codes 3-4; excluded from
# every other analysis) serve as an independent sample. The assigned three-factor model is
# compared with a one-factor model (ordered items, WLSMV; lavaan).
# ============================================================================
log_msg("\n[11/15] CFA of the three-factor purpose structure in pre-2025 initiators (holdout)")
cfa_items <- unlist(A_SPEC, use.names = FALSE)
d_pre <- df[ai_start %in% c(3L, 4L), cfa_items, drop = FALSE]; d_pre <- d_pre[complete.cases(d_pre), , drop = FALSE]
d_pre_num <- as.data.frame(lapply(d_pre, .as_num)); names(d_pre_num) <- paste0("item", match(cfa_items, unlist(A_SPEC, use.names = FALSE)))
if (!requireNamespace("lavaan", quietly = TRUE)) {
  log_msg("  CFA SKIP: 'lavaan' not installed."); add_flag("warn", "CFA holdout", "lavaan not installed", "install.packages('lavaan')")
} else if (nrow(d_pre_num) < 200L) {
  log_msg(sprintf("  CFA SKIP: only %d pre-2025 initiators with complete items.", nrow(d_pre_num)))
} else {
  .ix <- function(a) paste0("item", match(A_SPEC[[a]], cfa_items))
  mod3 <- paste0("PROD =~ ", paste(.ix("A_PC"), collapse = " + "), "\nDAILY =~ ", paste(.ix("A_DI"), collapse = " + "), "\nSOCIAL =~ ", paste(.ix("A_SE"), collapse = " + "))
  mod1 <- paste0("F =~ ", paste(names(d_pre_num), collapse = " + "))
  for (v in names(d_pre_num)) d_pre_num[[v]] <- ordered(d_pre_num[[v]])
  run_cfa <- function(model, label) tryCatch(suppressWarnings(lavaan::cfa(model, data = d_pre_num, ordered = names(d_pre_num), estimator = "WLSMV", std.lv = TRUE)),
                                             error = function(e) { log_msg(sprintf("  CFA %s failed: %s", label, conditionMessage(e))); NULL })
  fit3 <- run_cfa(mod3, "three-factor"); fit1 <- run_cfa(mod1, "one-factor")
  cfa_summary <- function(fit, label) { if (is.null(fit)) return(NULL)
    fm <- lavaan::fitMeasures(fit, c("chisq.scaled", "df.scaled", "pvalue.scaled", "cfi.scaled", "tli.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled", "rmsea.ci.upper.scaled", "srmr"))
    data.frame(model = label, n = lavaan::lavInspect(fit, "nobs"), chisq_scaled = round(fm[["chisq.scaled"]], 2), df = fm[["df.scaled"]], p = signif(fm[["pvalue.scaled"]], 3),
               CFI = round(fm[["cfi.scaled"]], 3), TLI = round(fm[["tli.scaled"]], 3), RMSEA = round(fm[["rmsea.scaled"]], 3),
               RMSEA_lo = round(fm[["rmsea.ci.lower.scaled"]], 3), RMSEA_hi = round(fm[["rmsea.ci.upper.scaled"]], 3), SRMR = round(fm[["srmr"]], 3), stringsAsFactors = FALSE) }
  cfa_fit <- rbind(cfa_summary(fit3, "Three-factor (assigned structure)"), cfa_summary(fit1, "One-factor"))
  if (!is.null(fit3) && !is.null(fit1)) { lrt <- tryCatch(lavaan::lavTestLRT(fit1, fit3), error = function(e) NULL)
    if (!is.null(lrt)) cfa_fit$scaled_chisq_diff_p_vs_one_factor <- c(lrt[2, "Pr(>Chisq)"], NA_real_) }
  if (!is.null(cfa_fit)) write_csv_out(cfa_fit, "11_cfa_holdout_fit")
  if (!is.null(fit3)) {
    sl <- lavaan::standardizedSolution(fit3); ld <- sl[sl$op == "=~", ]
    load3 <- data.frame(factor = ld$lhs, item = cfa_items[as.integer(sub("item", "", ld$rhs))], theoretical_group = unname(A_ITEM_MAP[cfa_items[as.integer(sub("item", "", ld$rhs))]]),
                        std_loading = round(ld$est.std, 3), se = round(ld$se, 3), p = signif(ld$pvalue, 3), stringsAsFactors = FALSE)
    write_csv_out(load3, "11_cfa_holdout_loadings")
    fc <- sl[sl$op == "~~" & sl$lhs != sl$rhs & sl$lhs %in% c("PROD", "DAILY", "SOCIAL") & sl$rhs %in% c("PROD", "DAILY", "SOCIAL"), ]
    write_csv_out(data.frame(factor1 = fc$lhs, factor2 = fc$rhs, r = round(fc$est.std, 3), ci_lo = round(fc$ci.lower, 3), ci_hi = round(fc$ci.upper, 3), stringsAsFactors = FALSE), "11_cfa_holdout_factor_cor")
    log_msg(sprintf("  three-factor CFA (n=%d): CFI=%.3f TLI=%.3f RMSEA=%.3f; loadings %.2f-%.2f", lavaan::lavInspect(fit3, "nobs"), cfa_fit$CFI[1], cfa_fit$TLI[1], cfa_fit$RMSEA[1], min(load3$std_loading), max(load3$std_loading)))
    if (cfa_fit$CFI[1] < 0.90) add_flag("warn", "CFA holdout", "CFI < 0.90", sprintf("CFI = %.3f", cfa_fit$CFI[1]))
  }
}

# ============================================================================
# Diagnostic 12 — exposure distribution (share of zeros; any S/E use)
# ============================================================================
log_msg("\n[12/15] Exposure distribution")
dm$A_SE_any <- as.integer(dm$A_SE_continuous > 0)
exp_rows <- do.call(rbind, lapply(A_CONT_VARS, function(a) data.frame(purpose = sub("_continuous$", "", a),
  n = nrow(dm), pct_zero_cohort = 100 * mean(dm[[a]] == 0), pct_zero_initiators = 100 * mean(dm[[a]][dm$ai_init == 1L] == 0),
  pct_any_initiators = 100 * mean(dm[[a]][dm$ai_init == 1L] > 0), mean_cohort = mean(dm[[a]]), sd_cohort = sd(dm[[a]]),
  mean_initiators = mean(dm[[a]][dm$ai_init == 1L]), sd_initiators = sd(dm[[a]][dm$ai_init == 1L]), stringsAsFactors = FALSE)))
# all current users (Q37S1 codes 3-6): the composites are rebuilt from the raw items here, because
# df$A_*_continuous is set to 0 for everyone who is not a 2025 initiator (codes 3-4 included).
cur <- df[df$ai_current_user == 1L, , drop = FALSE]
cur_A <- lapply(A_VARS, function(a) .row_mean(cur, A_SPEC[[a]], na_rm = FALSE) - 1); names(cur_A) <- A_VARS
cur_ok <- Reduce(`&`, lapply(cur_A, is.finite))
exp_rows$pct_any_all_current_users <- unname(vapply(exp_rows$purpose, function(p) 100 * mean(cur_A[[p]][cur_ok] > 0), numeric(1)))
exp_rows$n_all_current_users <- sum(cur_ok)
write_csv_out(exp_rows, "12_exposure_distribution")
for (i in seq_len(nrow(exp_rows))) log_msg(sprintf("  %s: %.1f%% of the cohort at 0; %.1f%% of initiators report any use", exp_rows$purpose[i], exp_rows$pct_zero_cohort[i], exp_rows$pct_any_initiators[i]))
vd <- do.call(rbind, lapply(A_CONT_VARS, function(a) { tb <- table(round(dm[[a]][dm$ai_init == 1L], 3)); data.frame(purpose = sub("_continuous$", "", a), value = as.numeric(names(tb)), n_initiators = as.integer(tb), stringsAsFactors = FALSE) }))
write_csv_out(vd, "12_exposure_values_initiators")

# ============================================================================
# Diagnostic 13 — baseline characteristics by generative-AI use history (whole two-wave panel)
# ============================================================================
log_msg("\n[13/15] Baseline characteristics by use history (two-wave panel)")
grp <- df[!is.na(df$ai_start_label), , drop = FALSE]
# rows identical to Table 1 (all 39 covariates, with their reference categories, plus follow-up loneliness)
grp$edu_ref    <- as.integer(grp$edu_univ == 0L & grp$edu_grad == 0L)
grp$emp_ref    <- as.integer(grp$emp_exec == 0L & grp$emp_self == 0L & grp$emp_nonreg == 0L & grp$emp_student == 0L & grp$emp_notwork == 0L)
grp$income_ref <- as.integer(grp$income_2_6m == 0L & grp$income_6_10m == 0L & grp$income_10m_plus == 0L & grp$income_unknown == 0L)
char_vars <- list(list(v = "age_2024", l = "Age (years), mean (SD)", t = "cont"), list(v = "sex_female", l = "Female, n (%)", t = "bin"),
                  list(v = "edu_ref", l = "Education: < university (ref), n (%)", t = "bin"), list(v = "edu_univ", l = "Education: university, n (%)", t = "bin"), list(v = "edu_grad", l = "Education: graduate, n (%)", t = "bin"),
                  list(v = "emp_ref", l = "Employment: regular employee (ref), n (%)", t = "bin"), list(v = "emp_exec", l = "Employment: executive, n (%)", t = "bin"), list(v = "emp_self", l = "Employment: self-employed, n (%)", t = "bin"),
                  list(v = "emp_nonreg", l = "Employment: non-regular, n (%)", t = "bin"), list(v = "emp_student", l = "Employment: student, n (%)", t = "bin"), list(v = "emp_notwork", l = "Employment: not working, n (%)", t = "bin"),
                  list(v = "income_ref", l = "Income < 2 m JPY (ref), n (%)", t = "bin"), list(v = "income_2_6m", l = "Income 2-6 m JPY, n (%)", t = "bin"), list(v = "income_6_10m", l = "Income 6-10 m JPY, n (%)", t = "bin"),
                  list(v = "income_10m_plus", l = "Income 10+ m JPY, n (%)", t = "bin"), list(v = "income_unknown", l = "Income unknown, n (%)", t = "bin"),
                  list(v = "married", l = "Married, n (%)", t = "bin"), list(v = "living_alone", l = "Living alone, n (%)", t = "bin"),
                  list(v = "baseline_lsns6_family", l = "LSNS-6 family (0-15), mean (SD)", t = "cont"), list(v = "baseline_lsns6_friends", l = "LSNS-6 friends (0-15), mean (SD)", t = "cont"),
                  list(v = "baseline_ucla3", l = "Baseline UCLA-3 (3-12), mean (SD)", t = "cont"), list(v = "baseline_k6", l = "Baseline K6 (0-24), mean (SD)", t = "cont"),
                  list(v = "ace_0", l = "ACE: none (ref), n (%)", t = "bin"), list(v = "ace_1", l = "ACE: 1, n (%)", t = "bin"), list(v = "ace_2_3", l = "ACE: 2-3, n (%)", t = "bin"), list(v = "ace_4plus", l = "ACE: 4+, n (%)", t = "bin"),
                  list(v = "mental_physical_health", l = "Mental/physical health, mean (SD)", t = "cont"),
                  list(v = "smartphone_band_5plus", l = "Smartphone 6+ h/day, n (%)", t = "bin"), list(v = "pc_tablet_band_5plus", l = "PC/tablet 6+ h/day, n (%)", t = "bin"),
                  list(v = "sitting_band_5plus", l = "Sitting 6+ h/day, n (%)", t = "bin"), list(v = "walking_band_5plus", l = "Walking 6+ h/day, n (%)", t = "bin"),
                  list(v = "Y_ucla3", l = "Follow-up UCLA-3 (3-12), mean (SD)", t = "cont"))
levs <- levels(grp$ai_start_label)
.fmt13 <- function(x, t) if (t == "cont") sprintf("%.2f (%.2f)", mean(x, na.rm = TRUE), sd(x, na.rm = TRUE)) else sprintf("%d (%.1f%%)", sum(x == 1, na.rm = TRUE), 100 * mean(x == 1, na.rm = TRUE))
tab13 <- do.call(rbind, lapply(char_vars, function(s) { row <- data.frame(Characteristic = s$l, stringsAsFactors = FALSE)
  for (L in levs) row[[L]] <- .fmt13(grp[[s$v]][grp$ai_start_label == L], s$t)
  x0 <- grp[[s$v]][grp$ai_start_label == "Never used"]
  for (L in levs[-1]) { x1 <- grp[[s$v]][grp$ai_start_label == L]; row[[paste0("SMD vs never: ", L)]] <- round((mean(x1, na.rm = TRUE) - mean(x0, na.rm = TRUE)) / sqrt((var(x1, na.rm = TRUE) + var(x0, na.rm = TRUE)) / 2), 3) }
  row }))
n13 <- data.frame(Characteristic = "n", stringsAsFactors = FALSE); for (L in levs) n13[[L]] <- as.character(sum(grp$ai_start_label == L)); for (L in levs[-1]) n13[[paste0("SMD vs never: ", L)]] <- NA
write_csv_out(rbind(n13, tab13), "13_characteristics_by_ai_use_history")

# ============================================================================
# Diagnostic 14 — attrition: retained vs lost among all valid 2024 respondents
# Needs the all-2024 file (jacsis_2024_all.csv); linked to the two-wave file by Monitor_ID.
# ============================================================================
log_msg("\n[14/15] Attrition: retained vs lost to follow-up")
if (!file.exists(DATA_2024_ALL_PATH)) {
  log_msg("  SKIP: all-2024 file not found (", DATA_2024_ALL_PATH, ")")
} else tryCatch({   # any failure (e.g. a raw 2024 column missing from the all-2024 file) is logged and skipped, not fatal
  all24 <- readr::read_csv(DATA_2024_ALL_PATH, show_col_types = FALSE, guess_max = 30000)
  .need24 <- c(ID_COL, "AGE_2024", "SEX_2024", "Q21.1_2024", "Q5.1_2024", "Q80.1_2024", "Q2_2024", "Q1.1_2024", "Q76.3_2024", "Q76.4_2024",
               paste0("Q66.", 1:3, "_2024"), paste0("Q65.", 1:6, "_2024"), paste0("Q77.", c(1:9, 13), "_2024"), paste0("Q17.", 1:6, "_2024"), paste0("Q28.", c(5, 6, 13, 14), "_2024"))
  .miss24 <- setdiff(.need24, names(all24)); if (length(.miss24)) { log_msg("  WARNING: all-2024 file lacks column(s): ", paste(.miss24, collapse = ", "))
    add_flag("warn", "Attrition", "all-2024 file lacks raw columns", paste(.miss24, collapse = ", ")) }
  .s24 <- function(cn) .safe(all24, cn)
  all24$baseline_ucla3 <- .row_sum_fn(all24, paste0("Q66.", 1:3, "_2024"), fn = .ucla_recode); all24$baseline_k6 <- .row_sum_fn(all24, paste0("Q65.", 1:6, "_2024"), fn = .k6_recode)
  ace24 <- intersect(paste0("Q77.", c(1:8,13), "_2024"), names(all24))
  if (length(ace24)) { ap <- vapply(all24[ace24], function(v) as.integer(.as_num(v) == 1L), integer(nrow(all24))); aps <- rowSums(ap, na.rm = TRUE)
    if ("Q77.9_2024" %in% names(all24)) { q9 <- .as_num(all24$Q77.9_2024); a9 <- as.integer(q9 == 2L); a9[is.na(q9)] <- 0L } else a9 <- 0L; all24$ace_score <- aps + a9 } else all24$ace_score <- NA_real_
  all24$ace_1 <- as.integer(all24$ace_score == 1L); all24$ace_2_3 <- as.integer(all24$ace_score %in% 2:3); all24$ace_4plus <- as.integer(all24$ace_score >= 4L)
  all24$age_2024 <- .s24("AGE_2024"); all24$sex_female <- as.integer(.s24("SEX_2024") == 2L)
  e24 <- .s24("Q21.1_2024"); all24$edu_univ <- as.integer(e24 %in% 6:8); all24$edu_grad <- as.integer(e24 == 9L)
  m24 <- .s24("Q5.1_2024"); all24$emp_exec <- as.integer(m24 == 1L); all24$emp_self <- as.integer(m24 %in% 2:4); all24$emp_nonreg <- as.integer(m24 %in% 7:11); all24$emp_student <- as.integer(m24 %in% 12:13); all24$emp_notwork <- as.integer(m24 %in% 14:16 | is.na(m24))
  i24 <- .s24("Q80.1_2024"); all24$income_2_6m <- as.integer(i24 %in% 5:8); all24$income_6_10m <- as.integer(i24 %in% 9:12); all24$income_10m_plus <- as.integer(i24 %in% 13:18); all24$income_unknown <- as.integer(is.na(i24) | i24 %in% c(19L, 20L))
  all24$married <- as.integer(.s24("Q2_2024") %in% 1:3); l24 <- .s24("Q1.1_2024"); all24$living_alone <- as.integer(!is.na(l24) & l24 == 1L)
  all24$baseline_lsns6_family <- .row_sum_fn(all24, paste0("Q17.", 1:3, "_2024"), fn = .lsns_recode); all24$baseline_lsns6_friends <- .row_sum_fn(all24, paste0("Q17.", 4:6, "_2024"), fn = .lsns_recode)
  all24$mental_physical_health <- .row_mean(all24, c("Q76.3_2024", "Q76.4_2024"))
  all24 <- .td(all24, "Q28.13_2024", "smartphone"); all24 <- .td(all24, "Q28.14_2024", "pc_tablet"); all24 <- .td(all24, "Q28.5_2024", "sitting"); all24 <- .td(all24, "Q28.6_2024", "walking")
  if (!(ID_COL %in% names(all24) && ID_COL %in% names(df))) { log_msg("  SKIP: Monitor_ID missing in one of the files") } else {
    # link on a common text key, so that it does not matter whether readr read Monitor_ID as a number in one file and as text in the other
    .id_key <- function(x) { k <- if (is.numeric(x)) sprintf("%.0f", x) else trimws(as.character(x)); k[is.na(x)] <- NA_character_; k }
    k24 <- .id_key(all24[[ID_COL]]); kdf <- .id_key(df[[ID_COL]]); all24$retained <- as.integer(!is.na(k24) & k24 %in% kdf[!is.na(kdf)])
    a24 <- all24[complete.cases(all24[, C_VARS]), , drop = FALSE]
    log_msg(sprintf("  all-2024 file %d rows; %d linked to the two-wave file (%d rows); %d complete on the 39 covariates", nrow(all24), sum(all24$retained), nrow(df), nrow(a24)))
    if (nrow(a24) < 100L || length(unique(a24$retained)) < 2L) { log_msg(sprintf("  SKIP: %d usable all-2024 rows, %d of them retained — check that the all-2024 file has the same *_2024 column names and the same Monitor_ID values as the two-wave file", nrow(a24), sum(a24$retained)))
      add_flag("warn", "Attrition", "all-2024 file could not be used", sprintf("%d usable rows, %d retained", nrow(a24), sum(a24$retained))) } else {
    .smd2 <- function(x1, x0) (mean(x1) - mean(x0)) / sqrt((var(x1) + var(x0)) / 2)
    cmp <- do.call(rbind, lapply(C_VARS, function(v) { x1 <- a24[[v]][a24$retained == 1L]; x0 <- a24[[v]][a24$retained == 0L]; bin <- all(a24[[v]] %in% c(0, 1))
      data.frame(variable = v, retained = if (bin) sprintf("%d (%.1f%%)", sum(x1), 100 * mean(x1)) else sprintf("%.2f (%.2f)", mean(x1), sd(x1)),
                 lost = if (bin) sprintf("%d (%.1f%%)", sum(x0), 100 * mean(x0)) else sprintf("%.2f (%.2f)", mean(x0), sd(x0)),
                 mean_retained = mean(x1), mean_lost = mean(x0), smd = .smd2(x1, x0), stringsAsFactors = FALSE) }))
    cmp <- rbind(data.frame(variable = "n", retained = as.character(sum(a24$retained == 1L)), lost = as.character(sum(a24$retained == 0L)), mean_retained = NA, mean_lost = NA, smd = NA), cmp)
    write_csv_out(cmp, "14_attrition_comparison")
    ret_fit <- glm(as.formula(paste("retained ~", paste(C_VARS, collapse = " + "))), data = a24, family = binomial())
    ct <- summary(ret_fit)$coefficients
    write_csv_out(data.frame(predictor = rownames(ct), OR = exp(ct[, 1]), OR_lo = exp(ct[, 1] - 1.96 * ct[, 2]), OR_hi = exp(ct[, 1] + 1.96 * ct[, 2]), p = ct[, 4], stringsAsFactors = FALSE), "14_attrition_retention_model")
    pr <- fitted(ret_fit); auc <- { r <- rank(pr); n1 <- sum(a24$retained == 1L); n0 <- sum(a24$retained == 0L); (sum(r[a24$retained == 1L]) - n1 * (n1 + 1) / 2) / (n1 * n0) }
    write_csv_out(data.frame(statistic = c("n_valid_2024", "n_retained", "retention_pct", "retention_model_auc"),
                             value = c(nrow(a24), sum(a24$retained), 100 * mean(a24$retained), auc)), "14_attrition_summary")
    big <- cmp$variable[!is.na(cmp$smd) & abs(cmp$smd) > 0.2]
    log_msg(sprintf("  retention %.1f%% (%d of %d); AUC %.2f; |SMD| > 0.2: %s", 100 * mean(a24$retained), sum(a24$retained), nrow(a24), auc, if (length(big)) paste(big, collapse = ", ") else "none"))
    }
  }
}, error = function(e) { log_msg("  attrition FAILED (skipped): ", conditionMessage(e)); add_flag("warn", "Attrition", "block failed", conditionMessage(e)) })

# ============================================================================
# Diagnostic 15 — baseline completion dates and overlap with the earlier analytic sample
# ============================================================================
log_msg("\n[15/15] Baseline completion dates and overlap with the earlier analytic sample")
if (any(!is.na(df$baseline_date))) {
  .bd <- function(d, label) data.frame(sample = label, n = sum(!is.na(d$baseline_date)), first_date = as.character(min(d$baseline_date, na.rm = TRUE)),
    median_date = as.character(median(d$baseline_date, na.rm = TRUE)), last_date = as.character(max(d$baseline_date, na.rm = TRUE)),
    n_on_or_after_2025_01_01 = sum(d$baseline_after_2025), pct = round(100 * mean(d$baseline_after_2025), 1), stringsAsFactors = FALSE)
  write_csv_out(rbind(.bd(df, "two-wave panel"), .bd(dm, "analytic cohort"), .bd(dm[dm$ai_init == 1L, ], "initiators"),
                      .bd(dm[dm$Q37S1_2025 == 5L, ], "initiators Jan-Jun 2025"), .bd(dm[dm$Q37S1_2025 == 6L, ], "initiators Jul-Dec 2025"), .bd(dm[dm$ai_init == 0L, ], "never-users")), "15_baseline_dates")
} else log_msg("  baseline dates SKIP: no completion timestamp column")
if (file.exists(REF5_LIST_PATH) && REF5_ID_COL %in% names(df)) {
  ref5 <- readr::read_csv(REF5_LIST_PATH, show_col_types = FALSE, col_types = readr::cols(.default = "c"))
  if (REF5_ID_COL %in% names(ref5)) { ids <- unique(as.character(ref5[[REF5_ID_COL]])); in5 <- as.character(dm[[REF5_ID_COL]]) %in% ids
    ov5 <- data.frame(quantity = c("ref5_list_ids", "analytic_cohort_n", "analytic_cohort_in_ref5", "initiators_in_ref5", "never_users_in_ref5", "two_wave_panel_n", "two_wave_panel_in_ref5"),
                      n = c(length(ids), nrow(dm), sum(in5), sum(in5 & dm$ai_init == 1L), sum(in5 & dm$ai_init == 0L), nrow(df), sum(as.character(df[[REF5_ID_COL]]) %in% ids)), stringsAsFactors = FALSE)
    ov5$pct <- round(100 * ov5$n / c(NA, nrow(dm), nrow(dm), sum(dm$ai_init == 1L), sum(dm$ai_init == 0L), nrow(df), nrow(df)), 1)
    write_csv_out(ov5, "15_overlap_with_earlier_sample"); log_msg(sprintf("  %d of %d analytic-cohort respondents (%.1f%%) were in the earlier analytic sample", sum(in5), nrow(dm), 100 * mean(in5))) }
} else log_msg("  overlap SKIP: ref-5 list not found or RI_ID_2024 missing")

# ============================================================================
# Flags markdown + sessionInfo
# ============================================================================
write_flags_md <- function(path, rejected, warned) {
  lines <- c(sprintf("# Flags — diagnosis run %s", .timestamp), "",
             sprintf("Total: %d rejected, %d warned.%s",
                     length(rejected), length(warned),
                     if (length(rejected) + length(warned) == 0L) " All clear." else ""), "")
  if (length(rejected)) {
    lines <- c(lines, "## Rejected", "", "| Item | Reason | Detail |", "|---|---|---|")
    for (r in rejected) lines <- c(lines, sprintf("| %s | %s | %s |", r$item, r$rule, r$detail))
    lines <- c(lines, "")
  }
  if (length(warned)) {
    lines <- c(lines, "## Warned", "", "| Item | Reason | Detail |", "|---|---|---|")
    for (w in warned) lines <- c(lines, sprintf("| %s | %s | %s |", w$item, w$rule, w$detail))
    lines <- c(lines, "")
  }
  lines <- c(lines,
             "## Reading order",
             "",
             sprintf("- `%s` — full run log.", basename(LOG_FILE)),
             "- `01_cohort_flow.csv` — raw → cohort → CCA → initiated/never.",
             "- `04_cronbach_alpha.csv` — A composites + UCLA-3 + K6 + LSNS + ACE.",
             "- `05_moderator_distribution.csv` — n by ai_init within each moderator level (sparseness check).",
             "- `07_A_cutpoints.csv` — §4 categorical-coding decisions (tertile / median / binary).",
             "- `08_vif_*.csv` — confounder-set health on §1 main-effects, friends_iso, income fits.",
             "")
  writeLines(lines, path)
}
write_flags_md(FLAGS_FILE, .flags_rejected, .flags_warned)
log_msg(sprintf("\nFlags written: %d rejected, %d warned -> %s",
                length(.flags_rejected), length(.flags_warned), FLAGS_FILE))

writeLines(capture.output(sessionInfo()),
           file.path(OUT_BASE, sprintf("sessionInfo_%s.txt", .timestamp)))
log_msg("=== diagnosis.R END ===")
