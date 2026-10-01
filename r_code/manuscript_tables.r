# ============================================================
# AI use and loneliness — manuscript packager
# ============================================================
#
# Pure downstream reader → publication tables and figures. Reads the aggregated mwinit_*
# CSVs (modwide_init/tables), the diagnosis CSVs (diagnosis/), and — for the by-category
# Table 1s only — the analytic cohort CSV (skipped if not present). It re-estimates
# nothing: if an estimate is wrong, fix moderator-wide.r / diagnosis.r, re-run, and the
# packager re-reads.
#
# Sensitivity analyses of the primary main effect: 1 categorical exposure (all three purposes
#   0/T1/T2/T3 with an indicator of generative-AI use), 2 users only, 3 social/emotional items
#   separately, 4 overlap weighting (any social/emotional use), 5 inverse-probability-of-retention
#   weighting, 6 baseline completed before 1 January 2025.
# Effect-modification sensitivity analyses: 1 categorical exposure, 2 users only,
#   3 alternative LSNS-6 thresholds.
#   Main : Table 1 (characteristics × social/emotional category) · Table 2 (main effects +
#          sensitivity analyses 1 and 2) · Figure 1 (spline marginal associations) · Figure 2 (forest)
#   Sup  : T1 EFA (Pearson, polychoric) + CFA holdout · T2/T3 characteristics × A_PC/A_DI
#          T4 characteristics by use history · T5 attrition · T6 full coefficients + VIF
#          T7 main-effect sensitivity analyses · T8 overlap-weighting diagnostics · T9 E-values
#          T10 effect-modification tests · T11 spline percentiles · T12 subgroups
#          T13 LSNS thresholds · T14 users-only subgroups
#   SupFig: 1 flow (drawn separately) · 2 balance · 3 propensity overlap · 4 predicted
#           5 categorical-exposure forest · 6 users-only forest
# ============================================================

# ---- 0a. Setup --------------------------------------------------------
suppressPackageStartupMessages({ library(here); library(readr); library(dplyr); library(tidyr); library(ggplot2) })
set.seed(20260524)

# ---- 0b. Path discovery + mode detection ------------------------------
find_proj_root <- function() {
  candidates <- character(0); ch <- tryCatch(here::here(), error = function(e) NA_character_)
  if (!is.na(ch)) candidates <- c(candidates, ch)
  args <- commandArgs(trailingOnly = FALSE); fa <- args[grepl("^--file=", args)]
  if (length(fa)) { sd <- tryCatch(normalizePath(dirname(sub("^--file=", "", fa[1])), winslash = "/"), error = function(e) NA_character_)
    if (!is.na(sd)) candidates <- c(candidates, sd, dirname(sd)) }
  candidates <- c(candidates, getwd(), dirname(getwd()))
  for (cand in unique(candidates)) { if (!nzchar(cand)) next
    if (basename(cand) == "r_code" && dir.exists(file.path(cand, "data"))) return(normalizePath(cand, winslash = "/"))
    if (dir.exists(file.path(cand, "r_code", "data"))) return(normalizePath(file.path(cand, "r_code"), winslash = "/")) }
  stop("Could not find r_code/")
}
.proj_root <- find_proj_root()
PIPELINE <- "ai_mod"; DESIGN <- "modwide_init"; OUTCOME <- "Y_ucla3"

.sub      <- function(...) file.path(.proj_root, "output", PIPELINE, ...)
TABLES_IN <- .sub(DESIGN, "tables"); FIGS_IN <- .sub(DESIGN, "figures"); DIAG_IN <- .sub("diagnosis")
MAN_ROOT  <- .sub("manuscript")
TABLES_OUT <- file.path(MAN_ROOT, "tables"); FIGS_OUT <- file.path(MAN_ROOT, "figures"); LOGS_OUT <- file.path(MAN_ROOT, "logs")
for (d in c(TABLES_OUT, FIGS_OUT, LOGS_OUT)) dir.create(d, recursive = TRUE, showWarnings = FALSE)
.timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
LOG_FILE   <- file.path(LOGS_OUT, sprintf("packager_%s.log", .timestamp))

log_msg <- function(...) { msg <- paste0(format(Sys.time(), "[%H:%M:%S] "), paste(..., collapse = " "))
  cat(msg, "\n", sep = ""); cat(msg, "\n", sep = "", file = LOG_FILE, append = TRUE); invisible(msg) }
write_table <- function(x, name) { fp <- file.path(TABLES_OUT, paste0(name, ".csv"))
  utils::write.csv(x, fp, row.names = FALSE, fileEncoding = "UTF-8"); log_msg("wrote table:", name); invisible(fp) }

# ---- 0c. Universal helpers --------------------------------------------
read_in <- function(stem, dir = TABLES_IN) { fp <- file.path(dir, paste0(stem, ".csv"))
  if (!file.exists(fp)) { log_msg("MISSING:", fp); return(NULL) }
  utils::read.csv(fp, stringsAsFactors = FALSE, fileEncoding = "UTF-8") }
copy_figure <- function(src_stem, dst_name) {
  any_ok <- FALSE
  for (ext in c("png", "pdf")) { src <- file.path(FIGS_IN, sprintf("%s.%s", src_stem, ext)); dst <- file.path(FIGS_OUT, sprintf("%s.%s", dst_name, ext))
    if (file.exists(src)) { file.copy(src, dst, overwrite = TRUE); any_ok <- TRUE } }
  log_msg(if (any_ok) paste("copied figure:", dst_name) else paste("MISSING figure:", src_stem)) }
save_fig <- function(g, dst) for (ext in c("png", "pdf")) tryCatch(ggsave(file.path(FIGS_OUT, sprintf("%s.%s", dst, ext)), g, width = 9, height = 6.5, dpi = 200), error = function(e) log_msg("ggsave failed:", dst, conditionMessage(e)))

log_msg("=== START packager ===  in:", TABLES_IN)

# Wipe prior packager outputs so a structure change leaves no stale files behind.
# file.remove() fails silently on locked files (Excel / sync clients holding a CSV open),
# which can leave a dropped panel in the manuscript — so report any file we could NOT delete.
.wipe_dir <- function(dir, pattern, label) {
  old <- list.files(dir, pattern = pattern, full.names = TRUE)
  if (!length(old)) return(invisible())
  ok  <- suppressWarnings(file.remove(old))
  if (any(!ok)) log_msg(sprintf("WARNING: could not delete %d stale %s (locked/open?) — close them and re-run: %s",
                                 sum(!ok), label, paste(basename(old[!ok]), collapse = ", ")))
}
.wipe_dir(TABLES_OUT, "\\.csv$",        "tables")
.wipe_dir(FIGS_OUT,   "\\.(png|pdf)$",  "figures")

# ============================================================
# 1. Table 1 + Sup Tables 2/3 — cohort characteristics by A_p category
#    GUARDED raw build: reconstructs the analytic sample (matches moderator-wide.r),
#    builds the 4-level A factors, and tabulates characteristics by category + Total.
# ============================================================
DATA_PATH <- { p <- Sys.getenv("JACSIS_2WAVE_2425_PATH"); if (nzchar(p)) p else file.path(.proj_root, "data", "jacsis_2wave2425.csv") }
if (!file.exists(DATA_PATH)) {
  log_msg("Table 1 SKIP: raw cohort CSV not available (", DATA_PATH, ") — by-category characteristics need the analytic sample.")
} else {
  log_msg("Building by-category Table 1 / Sup T2 / Sup T3 from:", basename(DATA_PATH))
  .as_num <- function(x) suppressWarnings(as.numeric(x))
  .row_mean <- function(d, cols, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowMeans(vapply(d[cols], .as_num, numeric(nrow(d))), na.rm = na_rm) }
  .row_sum_fn <- function(d, cols, fn = identity, na_rm = FALSE) { cols <- intersect(cols, names(d)); if (!length(cols)) return(rep(NA_real_, nrow(d))); rowSums(vapply(d[cols], function(v) fn(.as_num(v)), numeric(nrow(d))), na.rm = na_rm) }
  .ucla <- function(M) pmax(1, pmin(4, 5 - M)); .k6 <- function(M) pmax(0, pmin(4, 5 - M)); .lsns <- function(M) pmax(0, pmin(5, M - 1L))   # UCLA-3 3-12
  .td <- function(d, raw, p) { v <- .as_num(d[[raw]]); d[[paste0(p,"_band_1_2")]] <- as.integer(v %in% c(4L,5L)); d[[paste0(p,"_band_3_4")]] <- as.integer(v %in% c(6L,7L)); d[[paste0(p,"_band_5plus")]] <- as.integer(v %in% 8:11); d[[paste0(p,"_unknown")]] <- as.integer(is.na(v) | v==12L); d }
  df <- readr::read_csv(DATA_PATH, show_col_types = FALSE)
  .safe <- function(cn) if (cn %in% names(df)) .as_num(df[[cn]]) else rep(NA_real_, nrow(df))
  df$Y_ucla3 <- .row_sum_fn(df, paste0("Q66.",1:3,"_2025"), fn=.ucla)
  ai_start <- .safe("Q37S1_2025"); df$ai_init <- as.integer(ai_start %in% c(5L,6L))
  A_SPEC <- list(A_SE=paste0("Q37S3.",c(8,9),"_2025"), A_PC=paste0("Q37S3.",c(1,2,4,5),"_2025"), A_DI=paste0("Q37S3.",c(3,6,7),"_2025"))
  A_CONT_VARS <- paste0(names(A_SPEC), "_continuous")
  for (a in names(A_SPEC)) { v <- .row_mean(df, A_SPEC[[a]], na_rm = FALSE) - 1; v[df$ai_init == 0L] <- 0; df[[paste0(a,"_continuous")]] <- v }
  df$baseline_ucla3 <- .row_sum_fn(df, paste0("Q66.",1:3,"_2024"), fn=.ucla)
  df$baseline_k6    <- .row_sum_fn(df, paste0("Q65.",1:6,"_2024"), fn=.k6)
  ace_cols <- intersect(paste0("Q77.",c(1:8,13),"_2024"), names(df))
  if (length(ace_cols)) { ap <- vapply(df[ace_cols], function(v) as.integer(.as_num(v)==1L), integer(nrow(df))); aps <- rowSums(ap, na.rm=TRUE)
    if ("Q77.9_2024" %in% names(df)) { q9 <- .as_num(df$Q77.9_2024); a9 <- as.integer(q9==2L); a9[is.na(q9)] <- 0L } else a9 <- 0L; df$ace_score <- aps + a9 } else df$ace_score <- NA_real_
  df$ace_0 <- as.integer(df$ace_score==0L); df$ace_1 <- as.integer(df$ace_score==1L); df$ace_2_3 <- as.integer(df$ace_score %in% 2:3); df$ace_4plus <- as.integer(df$ace_score>=4L)
  df$age_2024 <- .safe("AGE_2024"); df$sex_female <- as.integer(.safe("SEX_2024")==2L)
  edu24 <- .safe("Q21.1_2024"); df$edu_univ <- as.integer(edu24 %in% 6:8); df$edu_grad <- as.integer(edu24==9L)
  df$edu_ref <- as.integer(df$edu_univ==0L & df$edu_grad==0L)  # descriptive ref level (< university); model base, not a covariate
  emp24 <- .safe("Q5.1_2024"); df$emp_exec <- as.integer(emp24==1L); df$emp_self <- as.integer(emp24 %in% 2:4); df$emp_nonreg <- as.integer(emp24 %in% 7:11); df$emp_student <- as.integer(emp24 %in% 12:13); df$emp_notwork <- as.integer(emp24 %in% 14:16 | is.na(emp24))
  df$emp_ref <- as.integer(df$emp_exec==0L & df$emp_self==0L & df$emp_nonreg==0L & df$emp_student==0L & df$emp_notwork==0L)  # descriptive ref level (regular employee)
  inc <- .safe("Q80.1_2024"); df$income_2_6m <- as.integer(inc %in% 5:8); df$income_6_10m <- as.integer(inc %in% 9:12); df$income_10m_plus <- as.integer(inc %in% 13:18); df$income_unknown <- as.integer(is.na(inc) | inc %in% c(19L,20L))
  df$income_ref <- as.integer(df$income_2_6m==0L & df$income_6_10m==0L & df$income_10m_plus==0L & df$income_unknown==0L)  # descriptive ref level (< 2m JPY)
  df$married <- as.integer(.safe("Q2_2024") %in% 1:3); liv <- .safe("Q1.1_2024"); df$living_alone <- as.integer(!is.na(liv) & liv==1L)
  df$baseline_lsns6_family <- .row_sum_fn(df, paste0("Q17.",1:3,"_2024"), fn=.lsns); df$baseline_lsns6_friends <- .row_sum_fn(df, paste0("Q17.",4:6,"_2024"), fn=.lsns)
  df$mental_physical_health <- .row_mean(df, c("Q76.3_2024","Q76.4_2024"))
  df <- .td(df,"Q28.13_2024","smartphone"); df <- .td(df,"Q28.14_2024","pc_tablet"); df <- .td(df,"Q28.5_2024","sitting"); df <- .td(df,"Q28.6_2024","walking")
  C_VARS <- c("age_2024","sex_female","edu_univ","edu_grad","emp_exec","emp_self","emp_nonreg","emp_student","emp_notwork",
    "income_2_6m","income_6_10m","income_10m_plus","income_unknown","married","living_alone",
    "baseline_lsns6_family","baseline_lsns6_friends","baseline_ucla3","baseline_k6","ace_1","ace_2_3","ace_4plus","mental_physical_health",
    "smartphone_band_1_2","smartphone_band_3_4","smartphone_band_5plus","smartphone_unknown","pc_tablet_band_1_2","pc_tablet_band_3_4","pc_tablet_band_5plus","pc_tablet_unknown",
    "sitting_band_1_2","sitting_band_3_4","sitting_band_5plus","sitting_unknown","walking_band_1_2","walking_band_3_4","walking_band_5plus","walking_unknown")
  ds <- df[ai_start %in% c(1L,5L,6L), , drop = FALSE]
  dm <- ds[complete.cases(ds[, c("Y_ucla3", A_CONT_VARS, C_VARS), drop = FALSE]), , drop = FALSE]
  log_msg(sprintf("  analytic sample: cohort{1,5,6}=%d, complete-case=%d (init %d / never %d)", nrow(ds), nrow(dm), sum(dm$ai_init==1L), sum(dm$ai_init==0L)))

  # 4-level A factor (tertiles of non-zero among initiators; fallback ladder), per §2.1
  make_A_cat <- function(x, idx_init) {
    nz <- x[idx_init & is.finite(x) & x > 0]
    if (length(nz) < 50L) return(factor(rep(NA_character_, length(x))))
    q33 <- unname(quantile(nz, 1/3, type = 7)); q67 <- unname(quantile(nz, 2/3, type = 7)); q50 <- unname(quantile(nz, 0.5, type = 7)); qmn <- min(nz)
    f <- if (q33 < q67 - 1e-8) cut(x, c(-Inf,0,q33,q67,Inf), c("0","T1","T2","T3"), include.lowest = TRUE)
         else if (q50 > qmn + 1e-8) cut(x, c(-Inf,0,q50,Inf), c("0","low","high"), include.lowest = TRUE)
         else factor(ifelse(x > 0, "any", "0"), levels = c("0","any"))
    factor(f, levels = c("0", setdiff(levels(f), "0")))
  }
  for (a in names(A_SPEC)) dm[[paste0(a, "_cat")]] <- make_A_cat(dm[[paste0(a, "_continuous")]], dm$ai_init == 1L)
  # the "0" column is split into never-users of AI and AI users (2025 initiators) without the
  # focal use; tertile cut points are those of the users' non-zero distribution.
  for (a in names(A_SPEC)) { f <- as.character(dm[[paste0(a, "_cat")]])
    f[f == "0" & dm$ai_init == 0L] <- "Never-users"; f[f == "0" & dm$ai_init == 1L] <- "AI users, no use"
    dm[[paste0(a, "_cat5")]] <- factor(f, levels = c("Never-users", "AI users, no use", setdiff(levels(dm[[paste0(a, "_cat")]]), "0"))) }

  char_cont <- function(x) { x <- x[is.finite(x)]; if (!length(x)) "—" else sprintf("%.2f (%.2f)", mean(x), sd(x)) }
  char_bin  <- function(x) { x <- x[!is.na(x)]; if (!length(x)) "—" else sprintf("%d (%.1f%%)", sum(x == 1), 100 * mean(x == 1)) }
  # standardized mean difference, any use of the focal purpose vs never-users
  smd_fmt <- function(x1, x0) { x1 <- x1[is.finite(x1)]; x0 <- x0[is.finite(x0)]; if (!length(x1) || !length(x0)) return("—")
    s <- (mean(x1) - mean(x0)) / sqrt((var(x1) + var(x0)) / 2); if (is.finite(s)) sprintf("%.2f", s) else "—" }
  t1_spec <- list(
    list(l="n",                                   e=NA,                      t="n"),
    list(l="Age (years), mean (SD)",              e="age_2024",              t="cont"),
    list(l="Female, n (%)",                       e="sex_female",            t="bin"),
    list(l="Education: < university (ref), n (%)",  e="edu_ref",               t="bin"),
    list(l="Education: university, n (%)",         e="edu_univ",              t="bin"),
    list(l="Education: graduate, n (%)",           e="edu_grad",              t="bin"),
    list(l="Employment: regular employee (ref), n (%)", e="emp_ref",          t="bin"),
    list(l="Employment: executive, n (%)",         e="emp_exec",              t="bin"),
    list(l="Employment: self-employed, n (%)",     e="emp_self",              t="bin"),
    list(l="Employment: non-regular, n (%)",       e="emp_nonreg",            t="bin"),
    list(l="Employment: student, n (%)",           e="emp_student",           t="bin"),
    list(l="Employment: not working, n (%)",       e="emp_notwork",           t="bin"),
    list(l="Income < 2 m JPY (ref), n (%)",        e="income_ref",            t="bin"),
    list(l="Income 2-6 m JPY, n (%)",              e="income_2_6m",           t="bin"),
    list(l="Income 6-10 m JPY, n (%)",             e="income_6_10m",          t="bin"),
    list(l="Income 10+ m JPY, n (%)",              e="income_10m_plus",       t="bin"),
    list(l="Income unknown, n (%)",                e="income_unknown",        t="bin"),
    list(l="Married, n (%)",                       e="married",               t="bin"),
    list(l="Living alone, n (%)",                  e="living_alone",          t="bin"),
    list(l="LSNS-6 family (0-15), mean (SD)",      e="baseline_lsns6_family", t="cont"),
    list(l="LSNS-6 friends (0-15), mean (SD)",     e="baseline_lsns6_friends",t="cont"),
    list(l="Baseline UCLA-3 (3-12), mean (SD)",    e="baseline_ucla3",        t="cont"),
    list(l="Baseline K6 (0-24), mean (SD)",        e="baseline_k6",           t="cont"),
    list(l="ACE: none (ref), n (%)",               e="ace_0",                 t="bin"),    # categories, as entered in the models
    list(l="ACE: 1, n (%)",                        e="ace_1",                 t="bin"),
    list(l="ACE: 2-3, n (%)",                      e="ace_2_3",               t="bin"),
    list(l="ACE: 4+, n (%)",                       e="ace_4plus",             t="bin"),
    list(l="Mental/physical health, mean (SD)",    e="mental_physical_health",t="cont"),
    list(l="Smartphone 6+ h/day, n (%)",           e="smartphone_band_5plus", t="bin"),    # band codes 8-11 = 6 h/day or more
    list(l="PC/tablet 6+ h/day, n (%)",            e="pc_tablet_band_5plus",  t="bin"),
    list(l="Sitting 6+ h/day, n (%)",              e="sitting_band_5plus",    t="bin"),
    list(l="Walking 6+ h/day, n (%)",              e="walking_band_5plus",    t="bin"),
    list(l="Follow-up UCLA-3 (Y; 3-12), mean (SD)",e="Y_ucla3",               t="cont"))
  build_t1 <- function(d, cat_col) {
    g <- d[[cat_col]]; levs <- levels(droplevels(g))
    grp <- c(list(Total = d), setNames(lapply(levs, function(L) d[!is.na(g) & g == L, ]), levs))
    any_use <- !is.na(g) & !(g %in% c("Never-users", "AI users, no use")); never <- !is.na(g) & g == "Never-users"
    do.call(rbind, lapply(t1_spec, function(s) {
      vals <- vapply(grp, function(gd) {
        if (s$t == "n") sprintf("%d", nrow(gd)) else if (s$t == "cont") char_cont(gd[[s$e]]) else char_bin(gd[[s$e]])
      }, character(1))
      smd <- if (s$t == "n") "" else smd_fmt(d[[s$e]][any_use], d[[s$e]][never])
      as.data.frame(c(list(Characteristic = s$l), as.list(vals), list(`SMD (any use vs never-users)` = smd)), stringsAsFactors = FALSE, check.names = FALSE)
    }))
  }
  write_table(build_t1(dm, "A_SE_cat5"), "table1_characteristics_A_SE")
  write_table(build_t1(dm, "A_PC_cat5"), "sup_table2_characteristics_A_PC")
  write_table(build_t1(dm, "A_DI_cat5"), "sup_table3_characteristics_A_DI")
}

# ============================================================
# 2. Table 2 — three-purpose unmoderated main effects (PRIMARY)
# ============================================================
ov <- read_in("mwinit_overall")
fmt_ci <- function(lo, hi) sprintf("(%+.3f, %+.3f)", lo, hi)
if (!is.null(ov)) {
  ov$Purpose <- factor(ov$purpose, levels = c("A_SE","A_PC","A_DI"))
  table2 <- ov |> dplyr::arrange(Purpose) |> dplyr::transmute(
    Block = "Main model (continuous, per one unit)", Purpose, Level = "—",
    `β` = round(beta, 4), `95% CI` = fmt_ci(ci_lo, ci_hi), `p-value` = signif(p_value, 3), n = n)
  # sensitivity analysis 1 (social/emotional use in five categories, vs never-users) and sensitivity
  # analysis 2 (users only) blocks of Table 2; the complete set of contrasts is in Sup Table 7.
  cat_ov <- read_in("mwinit_cat_overall"); init_ov <- read_in("mwinit_overall_init")
  if (!is.null(cat_ov)) { c2 <- cat_ov[cat_ov$purpose == "A_SE", ]
    table2 <- rbind(table2, data.frame(Block = "Sensitivity analysis 1: social/emotional use in five categories (reference: never-users)",
      Purpose = "A_SE", Level = c2$contrast, `β` = round(c2$beta, 4), `95% CI` = fmt_ci(c2$ci_lo, c2$ci_hi), `p-value` = signif(c2$p_value, 3), n = c2$n, check.names = FALSE)) }
  if (!is.null(init_ov)) { init_ov$Purpose <- factor(init_ov$purpose, levels = c("A_SE","A_PC","A_DI")); i2 <- init_ov[order(init_ov$Purpose), ]
    table2 <- rbind(table2, data.frame(Block = sprintf("Sensitivity analysis 2: users only (n=%s)", format(i2$n[1], big.mark = ",")),
      Purpose = as.character(i2$Purpose), Level = "—", `β` = round(i2$beta, 4), `95% CI` = fmt_ci(i2$ci_lo, i2$ci_hi), `p-value` = signif(i2$p_value, 3), n = i2$n, check.names = FALSE)) }
  write_table(table2, "table2_main_effects")
  # footnote data: n, R², exposure SD, per-SD β and share at zero for each block
  fn <- data.frame(block = "Main model", purpose = ov$purpose, n = ov$n, r2 = round(ov$r2, 3), sd_exposure = round(ov$sd_A, 3), beta_per_sd = round(ov$beta_per_sd, 4), pct_zero = round(ov$pct_zero, 1), sd_outcome = round(ov$sd_Y, 3), stringsAsFactors = FALSE)
  if (!is.null(init_ov)) fn <- rbind(fn, data.frame(block = "Users only", purpose = init_ov$purpose, n = init_ov$n, r2 = round(init_ov$r2, 3), sd_exposure = round(init_ov$sd_A, 3), beta_per_sd = round(init_ov$beta_per_sd, 4), pct_zero = NA, sd_outcome = NA))
  if (!is.null(cat_ov)) fn <- rbind(fn, data.frame(block = "Five categories", purpose = "A_SE", n = cat_ov$n[1], r2 = round(cat_ov$r2[1], 3), sd_exposure = NA, beta_per_sd = NA, pct_zero = NA, sd_outcome = NA))
  write_table(fn, "table2_footnote_statistics")
  cuts <- read_in("mwinit_cat_cutpoints"); if (!is.null(cuts)) write_table(cuts, "table2_tertile_cutpoints")
}

# ============================================================
# 3. Main Figures: Figure 1 = spline AME (all 3 purposes), Figure 2 = forest (all 3 purposes).
# ============================================================
# Both are COPIED from the moderator-wide.r outputs in section 8 (fig_pass); no packager re-render.

# ============================================================
# 4. Sup Table 1 — EFA of the 9 purpose items (from diagnosis/)
# ============================================================
# Polychoric solution (part A), Pearson solution (part B), inter-factor correlations (both), and
# the CFA in the pre-2025 holdout sample (part C).
efa_load <- read_in("10_efa_purposes_loadings", DIAG_IN)
if (!is.null(efa_load)) {
  write_table(efa_load, "sup_table1_efa_purposes")
  for (suf in c("fit","factor_cor","variance")) { x <- read_in(paste0("10_efa_purposes_", suf), DIAG_IN)
    if (!is.null(x)) write_table(x, paste0("sup_table1_efa_purposes_", suf)) }
  for (suf in c("loadings","fit","factor_cor","variance")) { x <- read_in(paste0("10_efa_purposes_poly_", suf), DIAG_IN)
    if (!is.null(x)) write_table(x, paste0("sup_table1_efa_purposes_polychoric_", suf)) }
  for (suf in c("fit","loadings","factor_cor")) { x <- read_in(paste0("11_cfa_holdout_", suf), DIAG_IN)
    if (!is.null(x)) write_table(x, paste0("sup_table1_cfa_holdout_", suf)) }
} else log_msg("Sup Table 1 (EFA) SKIP: diagnosis/10_efa_purposes_loadings.csv not found.")

# ============================================================
# 5. Sup Tables 4-6 — characteristics by use history, attrition, full coefficients + VIF
# ============================================================
x <- read_in("13_characteristics_by_ai_use_history", DIAG_IN); if (!is.null(x)) write_table(x, "sup_table4_characteristics_by_ai_use_history")
for (suf in c("comparison", "retention_model", "summary")) { x <- read_in(paste0("14_attrition_", suf), DIAG_IN); if (!is.null(x)) write_table(x, paste0("sup_table5_attrition_", suf)) }
full0 <- read_in("mwinit_overall_full_coefficients"); vif0 <- read_in("08_vif_main_effects", DIAG_IN)
if (!is.null(full0)) { if (!is.null(vif0)) { vif0$predictor <- sub("_continuous$", "", vif0$predictor); full0$vif <- vif0$vif[match(full0$term, vif0$predictor)] }
  write_table(full0, "sup_table6_full_coefficients_primary_model") }
x <- read_in("mwinit_overall_fit_statistics"); if (!is.null(x)) write_table(x, "sup_table6_fit_statistics_primary_model")

# ============================================================
# 6. Sup Table 7 — sensitivity analyses of the primary main effect (consolidated)
# ============================================================
tidy_ov <- function(stem, model, keep = NULL) { x <- read_in(stem); if (is.null(x)) return(NULL)
  if (!is.null(keep)) x <- x[keep(x), , drop = FALSE]
  data.frame(Model = if ("model" %in% names(x)) paste0(model, ": ", x$model) else model, Purpose = x$purpose,
             Level = if ("contrast" %in% names(x)) x$contrast else if ("item" %in% names(x)) x$item else "—",
             beta = round(x$beta, 4),
             CI = fmt_ci(x$ci_lo, x$ci_hi), p = signif(x$p_value, 3),
             n = if ("n" %in% names(x)) x$n else NA_integer_, R2 = if ("r2" %in% names(x)) round(x$r2, 3) else NA_real_, stringsAsFactors = FALSE) }
sens7 <- dplyr::bind_rows(
  tidy_ov("mwinit_overall",                     "Main model"),
  tidy_ov("mwinit_cat_overall",                 "Sensitivity analysis 1: categorical exposure (all three purposes 0/T1/T2/T3 + AI-user indicator)"),
  tidy_ov("mwinit_overall_init",                "Sensitivity analysis 2: users only"),
  tidy_ov("mwinit_overall_items",               "Sensitivity analysis 3: social/emotional items"),
  tidy_ov("mwinit_overall_overlap_weighted",    "Sensitivity analysis 4: any social/emotional use, overlap weighting"),
  tidy_ov("mwinit_overall_attrition_weighted",  "Sensitivity analysis 5: inverse-probability-of-retention weighting"),
  tidy_ov("mwinit_overall_pre2025_baseline",    "Sensitivity analysis 6: baseline completed before 1 Jan 2025"))
if (!is.null(sens7) && nrow(sens7)) write_table(sens7, "sup_table7_main_effects_sensitivities")

# ============================================================
# 7. Sup Tables 8-14 — pass-through long tables
# ============================================================
sup_pass <- list(
  list(stem = "mwinit_ow_balance",                    out = "sup_table8_overlap_balance"),
  list(stem = "mwinit_ow_weight_summary",             out = "sup_table8_overlap_weight_summary"),
  list(stem = "mwinit_ow_propensity_summary",         out = "sup_table8_overlap_propensity_summary"),
  list(stem = "mwinit_evalues",                       out = "sup_table9_evalues"),
  list(stem = "mwinit_spline_moderation_tests",       out = "sup_table10_effect_modification_tests"),
  list(stem = "mwinit_spline_AME_quantiles",          out = "sup_table11_spline_quantiles"),
  list(stem = "mwinit_stratified",                    out = "sup_table12_subgroup_primary"),
  list(stem = "mwinit_interaction_contrasts",         out = "sup_table12_primary_contrasts"),
  list(stem = "mwinit_interaction",                   out = "sup_table12_primary_omnibus_tests"),
  list(stem = "mwinit_lsns_threshold_sweep",          out = "sup_table13_lsns_thresholds"),
  list(stem = "mwinit_stratified_init",               out = "sup_table14_subgroup_initiators"),
  list(stem = "mwinit_interaction_contrasts_init",    out = "sup_table14_initiators_contrasts"),
  list(stem = "mwinit_interaction_init",              out = "sup_table14_initiators_omnibus_tests"),
  list(stem = "mwinit_baseline_timing",               out = "sup_table7_baseline_timing"),
  list(stem = "mwinit_ipaw_weight_summary",           out = "sup_table7_ipaw_weight_summary"),
  list(stem = "mwinit_package_versions",              out = "methods_package_versions"))
for (S in sup_pass) { df0 <- read_in(S$stem); if (!is.null(df0)) write_table(df0, S$out) }
for (S in list(list(stem = "12_exposure_distribution", out = "methods_exposure_distribution"), list(stem = "15_baseline_dates", out = "methods_baseline_dates"),
               list(stem = "15_overlap_with_earlier_sample", out = "methods_overlap_with_earlier_sample"), list(stem = "01_cohort_flow", out = "sup_figure1_flow_counts"),
               list(stem = "04_cronbach_alpha", out = "methods_cronbach_alpha"))) { df0 <- read_in(S$stem, DIAG_IN); if (!is.null(df0)) write_table(df0, S$out) }

# ============================================================
# 8. Figures — copy moderator-wide.r 3-purpose figures to manuscript names
# ============================================================
fig_pass <- list(
  # --- main figures (all 3 purposes) ---
  list(src = sprintf("mwinit_spline_AME_%s", OUTCOME),                   dst = "figure1_spline_AME"),
  list(src = sprintf("mwinit_forest_%s", OUTCOME),                       dst = "figure2_primary_forest"),
  # --- supplement figures (Sup Fig 1 flow drawn separately) ---
  list(src = "mwinit_ow_balance",                                        dst = "sup_figure2_overlap_balance"),
  list(src = "mwinit_ow_propensity_overlap",                             dst = "sup_figure3_propensity_overlap"),
  list(src = sprintf("mwinit_spline_predicted_%s", OUTCOME),             dst = "sup_figure4_predicted"),
  list(src = sprintf("mwinit_cat_forest_%s", OUTCOME),                   dst = "sup_figure5_categorical_exposure"),
  list(src = sprintf("mwinit_forest_init_%s", OUTCOME),                  dst = "sup_figure6_users_only"))
for (F in fig_pass) copy_figure(F$src, F$dst)
log_msg("Sup Figure 1 (flow) is drawn separately from sup_figure1_flow_counts.csv / mwinit_sample_flow.csv — not auto-generated.")

# ============================================================
# 9. README + sessionInfo
# ============================================================
readme <- c(
  "# Manuscript-ready tables and figures",
  sprintf("Generated by manuscript_tables.r on %s.", format(Sys.time(), "%Y-%m-%d %H:%M")),
  "Sensitivity analyses of the primary main effect: 1 categorical exposure (all purposes 0/T1/T2/T3 + AI-user indicator), 2 users only,",
  "3 social/emotional items separately, 4 overlap weighting (any social/emotional use), 5 inverse-probability-of-retention weighting,",
  "6 baseline completed before 1 Jan 2025. Effect-modification sensitivity analyses: 1 categorical exposure, 2 users only, 3 LSNS-6 thresholds.",
  "",
  "## Main text",
  "- table1_characteristics_A_SE.csv — cohort characteristics by social/emotional use category (never-users / AI users, no use / T1-T3) + total + SMD.",
  "- table2_main_effects.csv (+ _footnote_statistics, _tertile_cutpoints) — main effects per one unit + sensitivity analyses 1 (five categories) and 2 (users only).",
  "- figures/figure1_spline_AME.{png,pdf} — spline marginal-association grid, 3 purposes × 3 moderators.",
  "- figures/figure2_primary_forest.{png,pdf} — moderation forest, 3 purposes × 4 moderators (per unit).",
  "",
  "## Supplementary tables",
  "- sup_table1_efa_purposes*.csv — EFA: polychoric + Pearson; factor correlations; CFA holdout (pre-2025 initiators).",
  "- sup_table2/3_characteristics_A_PC/A_DI.csv — characteristics by productivity/creative and daily/information category (same layout as Table 1).",
  "- sup_table4_characteristics_by_ai_use_history.csv — whole two-wave panel by Q37S1 code.",
  "- sup_table5_attrition_*.csv — retained vs lost 2024 respondents; retention model; summary.",
  "- sup_table6_full_coefficients_primary_model.csv (+ _fit_statistics) — every coefficient of the primary model with VIF.",
  "- sup_table7_main_effects_sensitivities.csv (+ _baseline_timing, _ipaw_weight_summary) — main model + sensitivity analyses 1-6.",
  "- sup_table8_overlap_*.csv — overlap weighting: balance, weight summary, propensity summary.",
  "- sup_table9_evalues.csv — E-values.",
  "- sup_table10_effect_modification_tests.csv — F tests of the moderator product terms (categorical and spline) with Holm P.",
  "- sup_table11_spline_quantiles.csv — spline marginal associations at p10/p25/p50/p75/p90.",
  "- sup_table12_subgroup_primary.csv / _primary_contrasts / _primary_omnibus_tests — stratified slopes (per unit), γ contrasts, F tests + Holm.",
  "- sup_table13_lsns_thresholds.csv — effect-modification sensitivity analysis 3: LSNS-6 thresholds <3, <6, <9, <12.",
  "- sup_table14_subgroup_initiators.csv / _initiators_contrasts / _initiators_omnibus_tests — effect-modification sensitivity analysis 2: users only.",
  "- methods_*.csv — package versions, exposure distribution, baseline dates, overlap with the earlier sample, Cronbach α.",
  "",
  "## Supplementary figures",
  "- sup_figure1_flow — flow chart (drawn separately from sup_figure1_flow_counts.csv).",
  "- sup_figure2_overlap_balance · 3_propensity_overlap · 4_predicted · 5_categorical_exposure · 6_users_only.",
  "",
  "Provenance: estimates from output/ai_mod/modwide_init/tables/mwinit_*.csv + diagnosis/*.csv. Packager re-estimates nothing.")
writeLines(readme, file.path(MAN_ROOT, "README.md")); log_msg("wrote README")
writeLines(capture.output(sessionInfo()), file.path(LOGS_OUT, sprintf("sessionInfo_%s.txt", .timestamp)))
log_msg("=== END packager ===")
