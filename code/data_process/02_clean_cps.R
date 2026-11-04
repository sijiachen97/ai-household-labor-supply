# =============================================================================
# 02_clean_cps.R
# CPS Sample Construction — Married Couples, Jan 2019 – May 2026
#
# Purpose: Load IPUMS-CPS monthly microdata, filter to the analysis sample
#   (married couples, age 25–64, basic monthly only), reshape to couple-level
#   (one row per couple × month), merge AI exposure from occ_ai_exposure.csv
#   for both spouses, construct outcome variables, and recompute AI exposure
#   percentile ranks over the analysis sample.
#
# Requires:
#   data/raw/cps_00001.xml           — IPUMS DDI codebook (XML format)
#   data/raw/cps_00001.dat           — IPUMS CPS microdata (fixed-width)
#   data/processed/occ_ai_exposure.csv — output of 01_merge_ai_exposure.R
#
# NOTE on the DDI file:
#   read_ipums_ddi() requires the XML codebook (.xml), NOT the HTML viewer
#   (.dat.html). If you only have the HTML file, re-download from IPUMS:
#     https://cps.ipums.org → My Extracts → click "DDI" link for cps_00001
#   The XML should arrive alongside the .dat file in the same download.
#
# Outputs:
#   data/processed/cps_couples.csv   — couple × month analysis dataset
#   data/processed/sample_counts.csv — sample attrition table
#
# Convention: "a_" prefix = Spouse A (the row person, carrier of AI exposure
#             treatment in H→W regressions when A is husband; reversed for W→H)
#             "b_" prefix = Spouse B (the other spouse, whose outcomes are the
#             dependent variable in H→W direction)
#             Both spouses' exposure and outcomes are kept so the same dataset
#             can run both directions of the event study.
# =============================================================================

library(tidyverse)
library(ipumsr)

# --- Paths -------------------------------------------------------------------
# Anchor to THIS project explicitly. Do NOT use here::here() — it resolves to
# whichever RStudio project / working directory is active, which may be a
# different project entirely. Edit ROOT if you move the project folder.
ROOT     <- path.expand(".")
RAW_DIR  <- file.path(ROOT, "data", "raw")
PROC_DIR <- file.path(ROOT, "data", "processed")
TEMP_DIR <- file.path(ROOT, "data", "temp")

dir.create(PROC_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TEMP_DIR, recursive = TRUE, showWarnings = FALSE)

# Extract 2 (Aug 2026): adds telework variables (TELWRKPAY/HR/BFCVD/DIFFCVD +
# allocation flags). ipumsr reads the .gz directly — no need to unzip.
# Extract 3 (Aug 2026): adds EARNWEEK2 + HOURWAGE2. The legacy EARNWEEK series
# TERMINATES in March 2023 (availability cliff — verified: 0 valid values from
# Apr 2023 in extract 2); EARNWEEK2 covers the full sample window. Same
# universe (ORG earner study, wage/salary, excl. self-employed).
# Extract 4 (Aug 2026): SAME variable set as extract 3, samples extended back
# to JANUARY 2015 (verified identical DDI variable list; Jan 2015 - May 2026).
# Purpose: long pre-period — the 2019-only pre base proved too thin to judge
# whether treatment-era swings are regime changes or normal volatility
# (see doc/exploratory_findings.md, 2026-08-22 entry), and the DiFilippo-style
# 2018 timing placebo requires 2016+ data.
# NOTE on rerun: the `_s` exposure percentiles are recomputed over the full
# analysis sample, so cut points shift slightly vs the 2019+ extract —
# downstream estimates will move in the third decimal. Expected, not a bug.
# NOTE: 2015-2019 CPS uses the 2010 Census occupation vintage (no new code
# switch introduced); the pending 2020 OCC/IND harmonization check now
# matters more — do it before trusting cross-2020 comparisons.
DDI_FILE      <- file.path(RAW_DIR, "cps_00004.xml")
DAT_FILE      <- file.path(RAW_DIR, "cps_00004.dat.gz")
EXPOSURE_FILE <- file.path(PROC_DIR, "occ_ai_exposure.csv")

# ChatGPT treatment date — November 2022 = k=0
TREATMENT_YEAR  <- 2022L
TREATMENT_MONTH <- 11L

# =============================================================================
# 1. Load CPS microdata
# =============================================================================

if (!file.exists(DDI_FILE)) {
  stop(
    "\n\nDDI XML file not found: ", DDI_FILE,
    "\n\nread_ipums_ddi() needs the XML codebook, not the HTML viewer.",
    "\nTo download: https://cps.ipums.org → My Extracts → 'DDI' link for cps_00001.",
    "\nThe .xml file should arrive alongside the .dat file.\n"
  )
}

message("Loading DDI...")
ddi <- read_ipums_ddi(DDI_FILE)

message("Loading CPS microdata (may take a few minutes for ~85M rows)...")
cps_raw <- read_ipums_micro(ddi, verbose = FALSE)

message(sprintf("Loaded: %s person-month records", format(nrow(cps_raw), big.mark = ",")))

# =============================================================================
# 2. Select variables
# =============================================================================

# All variables requested in the IPUMS extract (from doc/research_plan.md)
KEEP_VARS <- c(
  # Identifiers & linking
  "YEAR", "MONTH", "SERIAL", "CPSID", "CPSIDP", "CPSIDV",
  "PERNUM", "SPLOC", "SPRULE", "MISH",
  # Sample flag (2 = basic monthly; 1 = ASEC supplement)
  "ASECFLAG",
  # Labor outcomes
  "EMPSTAT", "LABFORCE",
  "OCC2010",    # IPUMS-harmonized Census 2010 occ codes — use throughout
  "IND1990",    # 1990 Census industry codes (harmonized)
  "CLASSWKR",   # class of worker
  "UHRSWORKT",  # usual hrs worked at all jobs (997 = hrs vary, 999 = NIU)
  "UHRSWORK1",  # usual hrs worked at main job
  "WKSTAT",     # detailed work status
  "DURUNEMP",   # weeks unemployed
  # Telework (extract 2). TELWRKPAY/HR: Oct 2022 onward.
  # TELWRKBFCVD/DIFFCVD: Oct 2022 – Nov 2023 only (retrospective to Feb 2020).
  "TELWRKPAY", "TELWRKHR", "TELWRKBFCVD", "TELWRKDIFFCVD",
  "QTELWRKPAY", "QTELWRKHR", "QTELWRKBFCVD", "QTELWRKDIFFCVD",
  # Earnings — ORG rotation subsample only.
  #   EARNWEEK  : legacy series, ENDS March 2023 (kept for cross-checks only).
  #   EARNWEEK2 : full-window replacement. NIU sentinel = 999999.99.
  #               Topcode $2,884.61 through Mar 2023; DYNAMIC topcode from
  #               Apr 2023 (weighted avg of top-3% earners, recomputed monthly;
  #               Apr 2023–Mar 2024 differs by MISH 4 vs 8). For log-earnings
  #               regressions censor uniformly at 2884.61 for comparability.
  #   HOURWAGE2 : hourly wage, PAIDHOUR==2 universe only. NIU = 999.99.
  #               (Old HOURWAGE is not in extract 3.)
  "EARNWEEK", "EARNWEEK2", "HOURWAGE2", "PAIDHOUR", "ELIGORG",
  # Demographics
  "AGE", "SEX", "RACE", "HISPAN", "EDUC",
  "MARST", "NCHILD", "NCHLT5", "RELATE",
  # Geography
  "STATEFIP", "METAREA", "CBSASZ", "COUNTY",
  # Weights
  "WTFINL", "EARNWT"
)

missing_vars <- setdiff(KEEP_VARS, names(cps_raw))
if (length(missing_vars) > 0) {
  warning("Variables not in extract: ", paste(missing_vars, collapse = ", "),
          "\nCheck your IPUMS extract and add them if needed.")
  KEEP_VARS <- intersect(KEEP_VARS, names(cps_raw))
}

cps <- cps_raw %>%
  select(all_of(KEEP_VARS))

rm(cps_raw); gc()  # free memory — the raw file is large

# --- Diagnostic: EARNWEEK vs EARNWEEK2 in the overlap (through Mar 2023) -----
# EARNWEEK2 is "rounded" but should track EARNWEEK almost exactly where both
# exist. A large mismatch here means a variable-definition misunderstanding —
# stop and investigate before trusting the spliced series.
if (all(c("EARNWEEK", "EARNWEEK2") %in% names(cps))) {
  ov <- cps$ELIGORG == 1 &
        (cps$YEAR < 2023 | (cps$YEAR == 2023 & cps$MONTH <= 3)) &
        !is.na(cps$EARNWEEK)  & cps$EARNWEEK  > 0 & cps$EARNWEEK  < 9999 &
        !is.na(cps$EARNWEEK2) & cps$EARNWEEK2 > 0 & cps$EARNWEEK2 < 999999
  ov[is.na(ov)] <- FALSE
  message(sprintf(
    "EARNWEEK vs EARNWEEK2 overlap check (ORG, through 2023-03): n = %s | cor = %.4f | within $1: %.1f%%",
    format(sum(ov), big.mark = ","),
    stats::cor(cps$EARNWEEK[ov], cps$EARNWEEK2[ov]),
    100 * mean(abs(cps$EARNWEEK[ov] - cps$EARNWEEK2[ov]) <= 1)))
  post_gap <- cps$ELIGORG == 1 &
              (cps$YEAR > 2023 | (cps$YEAR == 2023 & cps$MONTH >= 4)) &
              !is.na(cps$EARNWEEK2) & cps$EARNWEEK2 > 0 & cps$EARNWEEK2 < 999999
  post_gap[is.na(post_gap)] <- FALSE
  message(sprintf(
    "EARNWEEK2 coverage after the EARNWEEK cliff (Apr 2023+, ORG): %s valid person-months",
    format(sum(post_gap), big.mark = ",")))
}

# =============================================================================
# 3. Sample filters — person level
# =============================================================================
# Track attrition at each step for the sample_counts.csv output

attrition <- list()
attrition[["raw"]] <- tibble(
  step  = "Raw person-months",
  n_obs = nrow(cps)
)

# 3a. Basic monthly only — DROP the ASEC supplement, keep all basic-monthly
#   months. ASECFLAG coding in this extract:
#     1  = ASEC supplement record            → DROP
#     2  = March Basic monthly               → KEEP
#     NA = all other basic monthly months    → KEEP
#   NOTE: do NOT filter `ASECFLAG == 2` — that keeps ONLY March (the 8 March
#   basic files, ~805K rows) and silently drops Jan/Feb/Apr–Dec, which are
#   NA-coded. That would restrict the whole analysis to March and break the
#   monthly event study. Instead, drop only ASEC records and keep everything
#   else (NA treated as keep, since `NA != 1` evaluates to NA → dropped).
#
# Diagnostic: month × ASECFLAG composition before filtering (verify all 12
# months are present and only ASEC==1 gets removed).
message("MONTH x ASECFLAG (raw, before basic-monthly filter):")
print(table(MONTH = as.integer(cps$MONTH),
            ASECFLAG = as.integer(cps$ASECFLAG), useNA = "ifany"))

cps <- cps %>% filter(is.na(ASECFLAG) | ASECFLAG != 1)
attrition[["basic"]] <- tibble(
  step  = "Basic monthly only (dropped ASEC: ASECFLAG == 1)",
  n_obs = nrow(cps)
)

# 3b. Married with spouse present (MARST == 1)
cps <- cps %>% filter(MARST == 1)
attrition[["married"]] <- tibble(
  step  = "Married, spouse present (MARST == 1)",
  n_obs = nrow(cps)
)

# 3c. Spouse in same household (SPLOC > 0)
cps <- cps %>% filter(SPLOC > 0)
attrition[["sploc"]] <- tibble(
  step  = "Spouse located in household (SPLOC > 0)",
  n_obs = nrow(cps)
)

# 3d. Age 25–64
cps <- cps %>% filter(AGE >= 25, AGE <= 64)
attrition[["age_a"]] <- tibble(
  step  = "Own age 25-64",
  n_obs = nrow(cps)
)

# =============================================================================
# 4. Construct couple-level dataset
# =============================================================================
# Each person has SPLOC = PERNUM of their spouse in the same household-month.
# Strategy: join person i to person j where j's PERNUM == i's SPLOC,
# within the same household-month (identified by SERIAL × YEAR × MONTH —
# SERIAL uniquely identifies a household within a monthly sample; CPSID can be
# 0 for unlinkable households and would collide).
#
# Result: one row per ordered pair (i, j). Deduplicate by keeping only rows
# where PERNUM < SPLOC — for couple (i, j), person i has (PERNUM=i, SPLOC=j)
# and person j has (PERNUM=j, SPLOC=i), so PERNUM < SPLOC keeps exactly one.

cps <- cps %>%
  mutate(hh_key = paste(SERIAL, YEAR, MONTH, sep = "_"))

# Build spouse lookup table: all variables suffixed with "_sp"
spouse_lookup <- cps %>%
  rename_with(~ paste0(.x, "_sp"), -hh_key)
# spouse_lookup now has: hh_key, PERNUM_sp, SPLOC_sp, YEAR_sp, ...

# Join: for each person, find their spouse (SPLOC == spouse's PERNUM)
couples_raw <- cps %>%
  inner_join(
    spouse_lookup,
    by      = c("hh_key", "SPLOC" = "PERNUM_sp"),
    suffix  = c("", "_sp")   # "_sp" applied to any remaining name clashes
  )

# Deduplicate — keep one row per couple per month
couples <- couples_raw %>%
  filter(PERNUM < SPLOC)

rm(cps, couples_raw, spouse_lookup); gc()

attrition[["couple"]] <- tibble(
  step  = "Couple-level join (PERNUM < SPLOC dedup)",
  n_obs = nrow(couples)
)

# 4a. Both spouses aged 25–64
couples <- couples %>%
  filter(AGE_sp >= 25, AGE_sp <= 64)
attrition[["age_both"]] <- tibble(
  step  = "Both spouses age 25-64",
  n_obs = nrow(couples)
)

# =============================================================================
# 5. Time variables
# =============================================================================
# Compute relative month k = months since ChatGPT launch (Nov 2022 = k=0)
# Reference period for event study: k = -1 (Oct 2022)

couples <- couples %>%
  mutate(
    ym       = YEAR * 12L + MONTH,
    ym_treat = TREATMENT_YEAR * 12L + TREATMENT_MONTH,
    rel_month = ym - ym_treat,
    post     = as.integer(rel_month >= 0),
    ym_label = paste0(YEAR, "M", sprintf("%02d", MONTH))
  )

attrition[["full_range"]] <- tibble(
  step  = "Full date range in extract",
  n_obs = nrow(couples)
)

# =============================================================================
# 6. Merge AI exposure onto both spouses
# =============================================================================

ai_exp <- read_csv(EXPOSURE_FILE, show_col_types = FALSE) %>%
  mutate(occ2010 = as.integer(occ2010)) %>%
  # Compute aioe_pctile (missing from 01_merge_ai_exposure.R output)
  mutate(aioe_pctile = percent_rank(aioe)) %>%
  select(
    occ2010,
    genai_total, genai_core, genai_supp,  # Eisfeldt GenAI measures
    aioe,                                  # Felten AIOE
    wfh,                                   # Dingel-Neiman WFH teleworkability
    genai_total_pctile, genai_core_pctile, genai_supp_pctile,
    aioe_pctile, wfh_pctile                # pre-computed over all OCC2010 codes
  )

# Coerce join keys to plain integer (ipumsr returns haven_labelled numerics,
# which won't reliably match the integer occ2010 column in the exposure file).
couples <- couples %>%
  mutate(
    OCC2010    = as.integer(OCC2010),
    OCC2010_sp = as.integer(OCC2010_sp)
  )

# Merge onto Spouse A (treatment variable for H→W direction)
couples <- couples %>%
  left_join(
    ai_exp %>% rename_with(~ paste0("a_", .x), -occ2010),
    by = c("OCC2010" = "occ2010")
  )

# Merge onto Spouse B (treatment variable for W→H direction)
couples <- couples %>%
  left_join(
    ai_exp %>% rename_with(~ paste0("b_", .x), -occ2010),
    by = c("OCC2010_sp" = "occ2010")
  )

n_a_matched <- sum(!is.na(couples$a_genai_total))
n_b_matched <- sum(!is.na(couples$b_genai_total))
n_total     <- nrow(couples)

message(sprintf(
  "AI exposure merge: Spouse A matched %s / %s (%.1f%%);  Spouse B %s / %s (%.1f%%)",
  format(n_a_matched, big.mark = ","), format(n_total, big.mark = ","),
  100 * n_a_matched / n_total,
  format(n_b_matched, big.mark = ","), format(n_total, big.mark = ","),
  100 * n_b_matched / n_total
))

# =============================================================================
# 7. Outcome variables
# =============================================================================
# LABFORCE:  0=NIU, 1=not in LF, 2=in LF
# EMPSTAT:   00=NIU, 01=Armed Forces, 10=At work, 12=Has job/not at work,
#            20/21/22=Unemployed, 30-36=Not in labor force
# UHRSWORKT: usual hrs at ALL jobs;  997=hrs vary, 999=NIU → NA
# UHRSWORK1: usual hrs at MAIN job;  997=hrs vary, 999=NIU → NA (000=0 kept)
# full-time: derived from UHRSWORKT (all jobs) ≥ 35, per BLS convention
# (WKSTAT retained in raw form; finer breakdowns available for robustness)
#
# Two distinct extensive-margin outcomes:
#   inlf = labor force PARTICIPATION (employed OR unemployed job-seeker)
#   emp  = EMPLOYMENT (employed only; unemployed job-seekers coded 0)
# Armed forces (EMPSTAT 01) set to NA to match LABFORCE's NIU treatment.

couples <- couples %>%
  mutate(
    # --- Spouse B outcomes (main dependent variables for H→W direction) ---
    b_inlf = case_when(
      LABFORCE_sp == 2 ~ 1L,
      LABFORCE_sp == 1 ~ 0L,
      TRUE             ~ NA_integer_       # 0 = NIU
    ),
    b_emp = case_when(                     # employment (EMPSTAT-based)
      as.integer(EMPSTAT_sp) %in% c(10L, 12L)              ~ 1L,
      as.integer(EMPSTAT_sp) %in% c(20L, 21L, 22L, 30L, 31L,
                                    32L, 33L, 34L, 35L, 36L) ~ 0L,
      TRUE ~ NA_integer_                   # 00 = NIU, 01 = Armed Forces
    ),
    b_hrs = if_else(                       # usual hrs, ALL jobs
      as.integer(UHRSWORKT_sp) >= 997, NA_real_,
      as.numeric(UHRSWORKT_sp)
    ),
    b_hrs_main = if_else(                  # usual hrs, MAIN job
      as.integer(UHRSWORK1_sp) >= 997, NA_real_,
      as.numeric(UHRSWORK1_sp)
    ),
    # Full-time from all-jobs hours (BLS 35-hour convention). NA if hours
    # missing / "hours vary". A multiple-jobholder with 35+ total counts as FT.
    b_fulltime = if_else(is.na(b_hrs), NA_integer_, as.integer(b_hrs >= 35)),

    # --- Spouse A outcomes (for W→H direction, where A becomes "B") ---
    a_inlf = case_when(
      LABFORCE == 2 ~ 1L,
      LABFORCE == 1 ~ 0L,
      TRUE          ~ NA_integer_
    ),
    a_emp = case_when(                     # employment (EMPSTAT-based)
      as.integer(EMPSTAT) %in% c(10L, 12L)              ~ 1L,
      as.integer(EMPSTAT) %in% c(20L, 21L, 22L, 30L, 31L,
                                 32L, 33L, 34L, 35L, 36L) ~ 0L,
      TRUE ~ NA_integer_                   # 00 = NIU, 01 = Armed Forces
    ),
    a_hrs = if_else(                       # usual hrs, ALL jobs
      as.integer(UHRSWORKT) >= 997, NA_real_,
      as.numeric(UHRSWORKT)
    ),
    a_hrs_main = if_else(                  # usual hrs, MAIN job
      as.integer(UHRSWORK1) >= 997, NA_real_,
      as.numeric(UHRSWORK1)
    ),
    a_fulltime = if_else(is.na(a_hrs), NA_integer_, as.integer(a_hrs >= 35))
  )

# =============================================================================
# 7b. Telework variables (extract 2)
# =============================================================================
# Codes (verify against the new DDI — the check printed below will flag
# anything unexpected):
#   TELWRKPAY      0 = NIU, 1 = Yes, 2 = No                 [Oct 2022 onward]
#   TELWRKHR       hours; large sentinel values = NIU/missing
#   TELWRKBFCVD    0 = NIU, 1 = Yes, 2 = No                 [Oct 2022–Nov 2023]
#   TELWRKDIFFCVD  0 = NIU, 1 = More, 2 = Less, 3 = Same    [Oct 2022–Nov 2023]
#
# USE WITH CARE — these are measured POST-treatment:
#   telework / telework_hrs / rto are outcome-adjacent. Do NOT use them as
#   controls in the main AI regressions (bad control). They are MECHANISM
#   variables: use them descriptively, or to split the sample for a mechanism
#   probe, always labelling the result as conditioning on a post-treatment
#   variable.
#   telework_pre (TELWRKBFCVD) refers to Feb 2020 and is pre-determined, so it
#   is the safest of the four to use as a control or split (recall error aside).

tw_clean <- function(x) {                      # 1 = Yes, 2 = No, else NA
  x <- as.integer(x)
  dplyr::case_when(x == 1L ~ 1L, x == 2L ~ 0L, TRUE ~ NA_integer_)
}

couples <- couples %>%
  mutate(
    # --- Spouse A ---
    a_telework      = tw_clean(TELWRKPAY),
    a_telework_hrs  = if_else(as.integer(TELWRKHR) >= 900, NA_real_,
                              as.numeric(TELWRKHR)),
    a_telework_pre  = tw_clean(TELWRKBFCVD),                    # pre-COVID
    a_rto           = case_when(                                # "less than before"
      as.integer(TELWRKDIFFCVD) == 2L            ~ 1L,
      as.integer(TELWRKDIFFCVD) %in% c(1L, 3L)   ~ 0L,
      TRUE                                       ~ NA_integer_),
    a_tw_imputed    = as.integer(
      (as.integer(QTELWRKPAY) > 0 & !is.na(QTELWRKPAY)) |
      (as.integer(QTELWRKHR)  > 0 & !is.na(QTELWRKHR))),

    # --- Spouse B ---
    b_telework      = tw_clean(TELWRKPAY_sp),
    b_telework_hrs  = if_else(as.integer(TELWRKHR_sp) >= 900, NA_real_,
                              as.numeric(TELWRKHR_sp)),
    b_telework_pre  = tw_clean(TELWRKBFCVD_sp),
    b_rto           = case_when(
      as.integer(TELWRKDIFFCVD_sp) == 2L          ~ 1L,
      as.integer(TELWRKDIFFCVD_sp) %in% c(1L, 3L) ~ 0L,
      TRUE                                        ~ NA_integer_),
    b_tw_imputed    = as.integer(
      (as.integer(QTELWRKPAY_sp) > 0 & !is.na(QTELWRKPAY_sp)) |
      (as.integer(QTELWRKHR_sp)  > 0 & !is.na(QTELWRKHR_sp)))
  )

# --- verification: coverage and coding sanity -------------------------------
message("\nTelework variable check (non-missing share by period):")
print(
  couples %>%
    mutate(period = case_when(
      ym <  2022 * 12 + 10 ~ "before Oct 2022 (expect all NA)",
      ym <= 2023 * 12 + 11 ~ "Oct 2022 - Nov 2023",
      TRUE                 ~ "Dec 2023 onward (BFCVD/DIFF expect NA)")) %>%
    group_by(period) %>%
    summarise(n = n(),
              telework   = mean(!is.na(a_telework)),
              tw_hours   = mean(!is.na(a_telework_hrs)),
              pre_covid  = mean(!is.na(a_telework_pre)),
              rto        = mean(!is.na(a_rto)),
              imputed    = mean(a_tw_imputed, na.rm = TRUE),
              .groups = "drop") %>%
    mutate(across(where(is.numeric), ~ round(.x, 3)))
)
message("Telework rate among employed (Spouse A, Oct 2022+): ",
        round(mean(couples$a_telework[couples$a_emp == 1 &
                                        couples$ym >= 2022 * 12 + 10],
                   na.rm = TRUE), 3))

# =============================================================================
# 8. Demographic controls
# =============================================================================
# EDUC codes (IPUMS): <73=<HS, 73=HS, 80–100=some college, 110=BA, >110=graduate
# HISPAN: 0=not Hispanic, 1–499=Hispanic (901–902 = uncodable/unknown)
# RACE:   100=White, 200=Black, 300=American Indian, etc.

couples <- couples %>%
  mutate(
    # Sex
    a_female = as.integer(SEX    == 2),
    b_female = as.integer(SEX_sp == 2),

    # Education: categorical
    a_educ_cat = case_when(
      as.integer(EDUC)    <  73  ~ "lths",
      as.integer(EDUC)    == 73  ~ "hs",
      as.integer(EDUC)    <  110 ~ "somecol",
      as.integer(EDUC)    == 110 ~ "ba",
      as.integer(EDUC)    >  110 ~ "grad",
      TRUE                       ~ NA_character_
    ),
    b_educ_cat = case_when(
      as.integer(EDUC_sp) <  73  ~ "lths",
      as.integer(EDUC_sp) == 73  ~ "hs",
      as.integer(EDUC_sp) <  110 ~ "somecol",
      as.integer(EDUC_sp) == 110 ~ "ba",
      as.integer(EDUC_sp) >  110 ~ "grad",
      TRUE                       ~ NA_character_
    ),
    # Education: binary college+
    a_college = as.integer(as.integer(EDUC)    >= 110),
    b_college = as.integer(as.integer(EDUC_sp) >= 110),

    # Hispanic origin
    a_hispan = as.integer(as.integer(HISPAN)    %in% 1:499),
    b_hispan = as.integer(as.integer(HISPAN_sp) %in% 1:499),

    # Race × Hispanic → non-Hispanic White indicator
    a_white_nh = as.integer(as.integer(RACE)    == 100 & a_hispan == 0),
    b_white_nh = as.integer(as.integer(RACE_sp) == 100 & b_hispan == 0),

    # Household children
    n_children = as.integer(NCHILD),
    has_child  = as.integer(NCHILD > 0),
    # Children under 5 — the margin the RTO/household-flexibility story predicts.
    # Pre-school children require in-home care, so a husband's return to the
    # office bites hardest here. NCHILD alone cannot test this.
    n_child_u5   = as.integer(NCHLT5),
    has_child_u5 = as.integer(NCHLT5 > 0)
  )

# =============================================================================
# 9. Recompute AI exposure percentile ranks over the analysis sample
# =============================================================================
# The pre-computed ranks in occ_ai_exposure.csv span all OCC2010 codes (~500).
# Recompute over the set of occupations actually present in the main-spec
# sample (Jan 2022 – May 2026) for a within-sample relative ranking.
# Use the suffix "_s" to distinguish sample-recomputed ranks from global ranks.

main_window <- couples %>%
  filter(rel_month >= -10)   # Jan 2022 onward (k ≥ -10)

occ_in_sample <- unique(c(
  main_window$OCC2010[!is.na(main_window$OCC2010)],
  main_window$OCC2010_sp[!is.na(main_window$OCC2010_sp)]
))

occ_pctiles_s <- ai_exp %>%
  filter(occ2010 %in% occ_in_sample) %>%
  mutate(
    genai_total_pctile_s = percent_rank(genai_total),
    genai_core_pctile_s  = percent_rank(genai_core),
    genai_supp_pctile_s  = percent_rank(genai_supp),
    aioe_pctile_s        = percent_rank(aioe),
    wfh_pctile_s         = percent_rank(wfh)
  ) %>%
  select(occ2010, ends_with("_s"))

# Merge sample-recomputed ranks for Spouse A
couples <- couples %>%
  left_join(
    occ_pctiles_s %>% rename_with(~ paste0("a_", .x), -occ2010),
    by = c("OCC2010" = "occ2010")
  )

# Merge sample-recomputed ranks for Spouse B
couples <- couples %>%
  left_join(
    occ_pctiles_s %>% rename_with(~ paste0("b_", .x), -occ2010),
    by = c("OCC2010_sp" = "occ2010")
  )

# =============================================================================
# 10. Couple panel identifier (for within-couple FE robustness, Section 4.5)
# =============================================================================
# CPSIDV uniquely identifies the same person across CPS survey waves.
# A stable couple ID = concatenation of min/max CPSIDV across the two spouses.
# Use CPSIDV_sp = CPSIDV of Spouse B (as identified via the _sp join).

couples <- couples %>%
  mutate(
    coupleID = paste0(
      pmin(as.character(CPSIDV), as.character(CPSIDV_sp), na.rm = TRUE),
      "_",
      pmax(as.character(CPSIDV), as.character(CPSIDV_sp), na.rm = TRUE)
    )
  )

# =============================================================================
# 11. Final column selection and save
# =============================================================================
# Naming convention in the output file:
#   - No prefix  = household / couple-level (shared by both spouses)
#   - a_ prefix  = Spouse A  (the "own" person = lower PERNUM after the join)
#   - b_ prefix  = Spouse B  (the spouse, carried in via the _sp join)
# NOTE: A vs. B is arbitrary w.r.t. gender — use a_SEX / b_SEX (or a_female /
# b_female) to identify husband vs. wife. A is NOT "husband".
#
# col_map: names = NEW column name in output, values = existing name in `couples`.
# any_of() silently drops entries whose source column is absent (e.g. HOURWAGE
# not requested in the extract).

col_map <- c(
  # ---- Household / couple-level (unprefixed) ----
  YEAR = "YEAR", MONTH = "MONTH", ym = "ym", ym_label = "ym_label",
  rel_month = "rel_month", post = "post",
  CPSID = "CPSID", coupleID = "coupleID",
  n_children = "n_children", has_child = "has_child",
  n_child_u5 = "n_child_u5", has_child_u5 = "has_child_u5",
  STATEFIP = "STATEFIP", METAREA = "METAREA", CBSASZ = "CBSASZ", COUNTY = "COUNTY",

  # ---- Spouse A identifiers / linking ----
  a_PERNUM = "PERNUM", a_SPLOC = "SPLOC",
  a_CPSIDP = "CPSIDP", a_CPSIDV = "CPSIDV", a_MISH = "MISH", a_SPRULE = "SPRULE",

  # ---- Spouse B identifiers / linking ----
  b_CPSIDV = "CPSIDV_sp", b_MISH = "MISH_sp", b_SPRULE = "SPRULE_sp",

  # ---- Occupation / industry (FE / treatment carriers) ----
  a_OCC2010 = "OCC2010",    a_IND1990 = "IND1990",
  b_OCC2010 = "OCC2010_sp", b_IND1990 = "IND1990_sp",

  # ---- AI exposure — Spouse A ----
  a_genai_total = "a_genai_total", a_genai_core = "a_genai_core",
  a_genai_supp = "a_genai_supp", a_aioe = "a_aioe", a_wfh = "a_wfh",
  a_genai_total_pctile = "a_genai_total_pctile",
  a_genai_core_pctile  = "a_genai_core_pctile",
  a_genai_supp_pctile  = "a_genai_supp_pctile",
  a_aioe_pctile = "a_aioe_pctile", a_wfh_pctile = "a_wfh_pctile",
  a_genai_total_pctile_s = "a_genai_total_pctile_s",
  a_genai_core_pctile_s  = "a_genai_core_pctile_s",
  a_genai_supp_pctile_s  = "a_genai_supp_pctile_s",
  a_aioe_pctile_s = "a_aioe_pctile_s", a_wfh_pctile_s = "a_wfh_pctile_s",

  # ---- AI exposure — Spouse B ----
  b_genai_total = "b_genai_total", b_genai_core = "b_genai_core",
  b_genai_supp = "b_genai_supp", b_aioe = "b_aioe", b_wfh = "b_wfh",
  b_genai_total_pctile = "b_genai_total_pctile",
  b_genai_core_pctile  = "b_genai_core_pctile",
  b_genai_supp_pctile  = "b_genai_supp_pctile",
  b_aioe_pctile = "b_aioe_pctile", b_wfh_pctile = "b_wfh_pctile",
  b_genai_total_pctile_s = "b_genai_total_pctile_s",
  b_genai_core_pctile_s  = "b_genai_core_pctile_s",
  b_genai_supp_pctile_s  = "b_genai_supp_pctile_s",
  b_aioe_pctile_s = "b_aioe_pctile_s", b_wfh_pctile_s = "b_wfh_pctile_s",

  # ---- Outcomes (already prefixed) ----
  a_inlf = "a_inlf", a_emp = "a_emp", a_hrs = "a_hrs",
  a_hrs_main = "a_hrs_main", a_fulltime = "a_fulltime",
  b_inlf = "b_inlf", b_emp = "b_emp", b_hrs = "b_hrs",
  b_hrs_main = "b_hrs_main", b_fulltime = "b_fulltime",

  # ---- Telework (post-treatment; mechanism variables, not controls) ----
  a_telework = "a_telework", a_telework_hrs = "a_telework_hrs",
  a_telework_pre = "a_telework_pre", a_rto = "a_rto",
  a_tw_imputed = "a_tw_imputed",
  b_telework = "b_telework", b_telework_hrs = "b_telework_hrs",
  b_telework_pre = "b_telework_pre", b_rto = "b_rto",
  b_tw_imputed = "b_tw_imputed",

  # ---- Demographics — Spouse A ----
  a_AGE = "AGE", a_SEX = "SEX",
  a_female = "a_female", a_college = "a_college", a_educ_cat = "a_educ_cat",
  a_hispan = "a_hispan", a_white_nh = "a_white_nh",
  a_EDUC = "EDUC", a_RACE = "RACE", a_HISPAN = "HISPAN",

  # ---- Demographics — Spouse B ----
  b_AGE = "AGE_sp", b_SEX = "SEX_sp",
  b_female = "b_female", b_college = "b_college", b_educ_cat = "b_educ_cat",
  b_hispan = "b_hispan", b_white_nh = "b_white_nh",
  b_EDUC = "EDUC_sp", b_RACE = "RACE_sp", b_HISPAN = "HISPAN_sp",

  # ---- Raw labor-supply variables (for additional outcome construction) ----
  a_EMPSTAT = "EMPSTAT", a_LABFORCE = "LABFORCE", a_WKSTAT = "WKSTAT",
  a_DURUNEMP = "DURUNEMP", a_UHRSWORK1 = "UHRSWORK1", a_CLASSWKR = "CLASSWKR",
  b_EMPSTAT = "EMPSTAT_sp", b_LABFORCE = "LABFORCE_sp", b_WKSTAT = "WKSTAT_sp",
  b_DURUNEMP = "DURUNEMP_sp", b_UHRSWORK1 = "UHRSWORK1_sp", b_CLASSWKR = "CLASSWKR_sp",

  # ---- ORG earnings (subsample robustness) ----
  # EARNWEEK ends Mar 2023 (legacy; cross-checks only) — EARNWEEK2 is the
  # analysis series. Sentinels carried RAW (EARNWEEK2: 999999.99 = NIU;
  # HOURWAGE2: 999.99 = NIU) — analysis scripts must filter, as they already
  # do via ELIGORG + range checks.
  a_ELIGORG = "ELIGORG", a_EARNWEEK = "EARNWEEK",
  a_EARNWEEK2 = "EARNWEEK2", a_HOURWAGE2 = "HOURWAGE2",
  a_PAIDHOUR = "PAIDHOUR",
  b_ELIGORG = "ELIGORG_sp", b_EARNWEEK = "EARNWEEK_sp",
  b_EARNWEEK2 = "EARNWEEK2_sp", b_HOURWAGE2 = "HOURWAGE2_sp",
  b_PAIDHOUR = "PAIDHOUR_sp",

  # ---- Person weights (A and B carry their own final weight) ----
  a_WTFINL = "WTFINL", a_EARNWT = "EARNWT",
  b_WTFINL = "WTFINL_sp", b_EARNWT = "EARNWT_sp"
)

couples_out <- couples %>% select(any_of(col_map))

# Save
write_csv(couples_out, file.path(PROC_DIR, "cps_couples.csv"))
message(sprintf(
  "\ncps_couples.csv saved: %s couple-month rows × %d columns",
  format(nrow(couples_out), big.mark = ","), ncol(couples_out)
))

# =============================================================================
# 12. Attrition table
# =============================================================================

attrition_df <- bind_rows(attrition)
write_csv(attrition_df, file.path(PROC_DIR, "sample_counts.csv"))

message("\nSample attrition:")
print(attrition_df, n = Inf)

message("\n=== 02_clean_cps.R complete ===")
message("Next step: 03_event_study.R")
