# ============================================================
# 01_merge_ai_exposure.R
# Intra-Household AI Spillover Project
#
# PURPOSE:
#   Build an occupation-level AI exposure file keyed by CPS
#   OCC2010 (4-digit Census code), ready to merge onto CPS data.
#
# CROSSWALK CHAIN:
#   CPS OCC2010 (4-digit integer)
#     ← 2010summ sheet of Census crosswalk XLS
#   SOC 2010 (6-digit, "XX-XXXX")
#     ← three-way match to Eisfeldt / WFH / AIOE:
#         (a) EXACT   — SOC code appears verbatim in exposure data
#         (b) GROUP   — SOC code is a broad-group entry not in exposure data
#                       (e.g. "11-2020" covers 11-2021, 11-2022 …)
#                       → prefix-match first 6 chars, average sub-codes
#         (c) WILDCARD— SOC code contains X (e.g. "15-113X")
#                       → regex-match digits, exclude explicitly mapped codes
#
# OUTPUT:
#   data/processed/occ_ai_exposure.csv   — one row per OCC2010 code
#   output/tables/01_merge_coverage.csv     — match-rate diagnostics
#
# REQUIREMENTS:
#   install.packages(c("tidyverse", "readxl", "here"))
# ============================================================

rm(list = ls())

library(tidyverse)
library(readxl)

# ── Paths ────────────────────────────────────────────────────
PROJECT_ROOT <- normalizePath(Sys.getenv("PROJECT_ROOT", unset = "."), mustWork = TRUE)
RAW_DIR      <- file.path(PROJECT_ROOT, "data", "raw")
PROC_DIR     <- file.path(PROJECT_ROOT, "data", "processed")
TABLE_DIR    <- file.path(PROJECT_ROOT, "output", "data_prep", "tables")

for (d in c(PROC_DIR, TABLE_DIR)) dir.create(d, recursive = TRUE, showWarnings = FALSE)

# ── Canonical SOC code normalisation ─────────────────────────
# All three exposure datasets may store SOC codes in different formats.
# This function converts any reasonable representation to "XX-XXXX"
# (7 chars with hyphen), which is what the Census crosswalk uses.
#
# Handles:
#   "11-1011"    → "11-1011"  (already canonical)
#   "111011"     → "11-1011"  (6 raw digits, no hyphen — common in Eisfeldt/AIOE)
#   "11-1011.00" → "11-1011"  (O*NET 10-char, strip suffix)
#   "11-1011.0"  → "11-1011"  (variant O*NET)
#
# Wildcard codes like "15-113X" or "25-90XX" are left unchanged because
# str_detect(x, "^\\d{6}$") is FALSE for strings containing "X".
normalise_soc <- function(x) {
  x <- str_to_upper(str_trim(as.character(x)))
  x <- str_remove(x, "\\..*$")                     # strip O*NET ".XX" / ".X" suffix
  x <- ifelse(str_detect(x, "^\\d{6}$"),            # "151011" → "15-1011"
              paste0(str_sub(x, 1, 2), "-", str_sub(x, 3, 6)),
              x)
  x
}

# ── 1. Load Census–SOC crosswalk ─────────────────────────────
# Sheet "2010summ": rows 0–1 are title/date, row 2 is the column header.
# col_names = TRUE with skip = 2: readxl uses row 2 as column names,
# avoiding positional fragility when it silently drops a blank first column.
cat("Loading Census-SOC crosswalk...\n")

xwalk_raw <- read_xls(
  file.path(RAW_DIR, "2010-occ-codes-with-crosswalk-from-2002-2011.xls"),
  sheet     = "2010summ",
  skip      = 2,
  col_names = TRUE
)
cat(sprintf("  Raw columns: %s\n", paste(names(xwalk_raw), collapse = " | ")))

xwalk <- xwalk_raw %>%
  select(
    soc2010     = `2010 SOC Code`,
    occ2010_raw = `2010 Census Code`,
    title       = `2010 Occupation Title`
  ) %>%
  filter(
    !is.na(soc2010),
    !is.na(occ2010_raw),
    !str_detect(as.character(soc2010), "SOC Code")  # drop any residual header rows
  ) %>%
  mutate(
    # Normalise to uppercase and trim — wildcards sometimes have trailing spaces
    soc2010 = str_to_upper(str_trim(as.character(soc2010))),
    # Census codes read as floats (e.g. 1005.0) — strip decimal, zero-pad to 4
    occ2010 = str_pad(
      str_remove(as.character(occ2010_raw), "\\..*"),
      width = 4, side = "left", pad = "0"
    ),
    title       = str_trim(as.character(title)),
    # soc_prefix: first 6 chars (kept for reference)
    soc_prefix  = str_sub(soc2010, 1, 6),
    # soc_family_prefix: strip trailing zeros from the post-hyphen digits so
    # broad group codes match their detailed sub-codes in the exposure data.
    #   "25-1000" → "25-1"   (matches 25-1011, 25-1021, 25-1022 …)
    #   "25-3000" → "25-3"   (matches 25-3011, 25-3021 …)
    #   "11-2020" → "11-202" (matches 11-2021, 11-2022)
    #   "11-1011" → "11-1011" (no trailing zeros, used as exact key)
    # Wildcard codes ("15-113X") contain X so no trailing zeros to strip.
    soc_family_prefix = paste0(
      str_sub(soc2010, 1, 2), "-",
      str_remove(str_sub(soc2010, 4), "0+$")
    ),
    is_wildcard = str_detect(soc2010, "X")
  )

cat(sprintf("  Crosswalk rows: %d  |  Wildcard entries: %d\n",
            nrow(xwalk), sum(xwalk$is_wildcard)))
cat("  Wildcard entries:\n")
xwalk %>% filter(is_wildcard) %>% select(soc2010, occ2010, title) %>% print(n = Inf)

# ── 2. Load Eisfeldt GenAI Exposure ──────────────────────────
cat("\nLoading Eisfeldt GenAI exposure...\n")

genai <- read_csv(
  file.path(RAW_DIR, "genaiexp_estz_occscores.csv"),
  show_col_types = FALSE
) %>%
  mutate(soc2010 = normalise_soc(soc2010))

cat(sprintf("  GenAI: %d SOC codes  |  NAs in total score: %d\n",
            nrow(genai), sum(is.na(genai$genaiexp_estz_total))))

# ── 3. Load Dingel-Neiman WFH ─────────────────────────────────
cat("\nLoading Dingel-Neiman WFH...\n")

# SOURCE: onet_teleworkable_blscodes.csv
#   From DingelNeiman-workathome/onet_to_BLS_crosswalk/output/
#   This is preferable to occupations_workathome.csv because it uses D&N's
#   fully-aggregated pipeline output:
#     Step 1: O*NET (8-digit) → SOC_2010 (6-digit) via survey-respondent-
#             weighted averages [w=n] — better than our naive unweighted mean
#     Step 2: SOC_2010 → OES_2018 hybrid codes via simple averages
#             (employment only available at OES_2018 aggregate level)
#   OES_2018 codes ≈ SOC 2010 codes (OES 2018 was last OES on SOC 2010),
#   so these are compatible with our Census crosswalk.
#   Columns: OCC_CODE (OES_2018 / SOC 2010 6-digit), OES_TITLE, teleworkable
wfh <- read_csv(
  file.path(RAW_DIR, "onet_teleworkable_blscodes.csv"),
  show_col_types = FALSE
) %>%
  select(soc2010 = OCC_CODE, wfh_teleworkable = teleworkable) %>%
  mutate(soc2010 = normalise_soc(soc2010))

cat(sprintf("  WFH: %d 6-digit SOC codes\n", nrow(wfh)))

# ── 4. Load Felten AIOE ───────────────────────────────────────
cat("\nLoading Felten AIOE...\n")

aioe_raw <- read_excel(
  file.path(RAW_DIR, "AIOE_DataAppendix.xlsx"),
  sheet = "Appendix A"
)
cat(sprintf("  Rows: %d  |  Columns: %s\n",
            nrow(aioe_raw), paste(names(aioe_raw), collapse = ", ")))

soc_col  <- names(aioe_raw)[str_detect(names(aioe_raw), regex("soc|occ",   ignore_case = TRUE))][1]
aioe_col <- names(aioe_raw)[str_detect(names(aioe_raw), regex("aioe|score", ignore_case = TRUE))][1]

if (is.na(soc_col) || is.na(aioe_col)) {
  warning("Could not auto-detect SOC/AIOE columns — set soc_col and aioe_col manually.")
  aioe_ok <- FALSE
} else {
  cat(sprintf("  → SOC col: '%s'  |  AIOE col: '%s'\n", soc_col, aioe_col))
  aioe <- aioe_raw %>%
    select(soc2010 = all_of(soc_col), aioe = all_of(aioe_col)) %>%
    filter(!is.na(soc2010), !is.na(aioe)) %>%
    mutate(soc2010 = normalise_soc(soc2010))
  cat(sprintf("  AIOE: %d SOC codes\n", nrow(aioe)))
  aioe_ok <- TRUE
}

# ── 5. Load BLS OES 2018 employment weights ───────────────────
# Source: national_M2018_dl.xlsx  (May 2018 OES national estimates)
# Download: https://www.bls.gov/oes/special.requests/oesm18nat.zip
# → unzip and place national_M2018_dl.xlsx in data/raw/
#
# We use OES 2018 — the LAST year BLS published OES under SOC 2010.
# OES 2019 and later use SOC 2018, which would NOT match our crosswalk,
# Eisfeldt GenAI, Felten AIOE, or Dingel-Neiman WFH (all SOC 2010).
# Dingel-Neiman (2020) make the same choice for the same reason.
cat("\nLoading BLS OES 2018 employment weights (SOC 2010 basis)...\n")

oes_path <- file.path(RAW_DIR, "national_M2018_dl.xlsx")

if (!file.exists(oes_path)) {
  cat("  [!] Not found: national_M2018_dl.xlsx\n")
  cat("      Download oesm18nat.zip from bls.gov/oes and unzip to data/raw/.\n")
  cat("      Falling back to equal weights for all group/wildcard averaging.\n")
  oes_ok  <- FALSE
  oes_emp <- tibble(soc2010 = character(), emp_oes = numeric())
} else {
  oes_raw <- read_excel(oes_path) %>%
    filter(OCC_GROUP == "detailed") %>%
    select(soc2010 = OCC_CODE, emp_oes = TOT_EMP) %>%
    mutate(
      soc2010 = normalise_soc(soc2010),
      # TOT_EMP may contain "**" (suppressed) — coerce to numeric (NA for **)
      emp_oes = suppressWarnings(
        as.numeric(str_remove_all(as.character(emp_oes), ","))
      )
    ) %>%
    filter(!is.na(soc2010), emp_oes > 0 | is.na(emp_oes))

  oes_emp <- oes_raw %>% filter(!is.na(emp_oes))
  cat(sprintf("  OES: %d detailed SOC codes  |  %d with employment > 0\n",
              nrow(oes_raw), nrow(oes_emp)))
  oes_ok <- TRUE
}

# ── 6. Build SOC-level exposure table ─────────────────────────
cat("\nBuilding SOC-level exposure table...\n")

soc_exposure <- genai %>%
  full_join(wfh, by = "soc2010") %>%
  { if (isTRUE(aioe_ok)) full_join(., aioe, by = "soc2010") else . } %>%
  left_join(oes_emp, by = "soc2010")   # attach OES employment weights

n_with_w <- sum(!is.na(soc_exposure$emp_oes))
cat(sprintf("  Combined SOC rows: %d  |  with OES weight: %d (%.1f%%)\n",
            nrow(soc_exposure), n_with_w,
            100 * n_with_w / nrow(soc_exposure)))

# ── DIAGNOSTIC: verify SOC code format consistency ────────────
# If wildcards return 0 matches, this block will reveal the format mismatch.
cat("\n── SOC code format diagnostics ──\n")
cat("Crosswalk sample (non-wildcard):\n")
print(head(filter(xwalk, !is_wildcard)$soc2010, 8))
cat("Crosswalk wildcard entries:\n")
print(filter(xwalk, is_wildcard)$soc2010)

cat("\nEisfeldt soc2010 sample:\n")
print(head(genai$soc2010, 8))
cat(sprintf("  nchar range: %d – %d  |  has hyphen: %s\n",
            min(nchar(genai$soc2010), na.rm = TRUE),
            max(nchar(genai$soc2010), na.rm = TRUE),
            any(str_detect(genai$soc2010, "-"))))

cat("\nWFH soc2010 sample:\n")
print(head(wfh$soc2010, 8))

# Spot-check: does any Eisfeldt code match the 15-113X wildcard?
test_pat  <- "^15-113\\d$"
test_hits <- genai$soc2010[str_detect(genai$soc2010, test_pat)]
cat(sprintf("\nTest  |  regex: %s  |  Eisfeldt hits: %d\n",
            test_pat, length(test_hits)))
if (length(test_hits) > 0) cat("  Matched codes:", paste(test_hits, collapse = ", "), "\n")
cat("\n")

# Set of SOC codes present in the exposure data (for match-type classification)
exposure_soc_set <- soc_exposure$soc2010

# ── 7. Classify crosswalk entries ─────────────────────────────
# Three types:
#   EXACT    — SOC code is a detailed occupation (last digit 1–9) AND appears
#              verbatim in soc_exposure → direct join
#   GROUP    — SOC code ends in 0 (broad/minor group in SOC hierarchy) → match
#              all sub-codes via family prefix, including any verbatim row.
#              Key case: "19-1020" appears in AIOE but is still a group code
#              covering 19-1021, 19-1022, 19-1023, 19-1029. Using only its
#              own AIOE row would miss GenAI/WFH scores for the sub-codes.
#   WILDCARD — SOC code contains "X" → regex match, exclude explicit codes
#
# The SOC 2010 rule: detailed occupations never end in 0. Codes ending in 0
# are always broad occupations or minor groups. We enforce this to prevent
# accidental "exact" classification of group-level codes.
xwalk <- xwalk %>%
  mutate(
    exact_in_exposure = soc2010 %in% exposure_soc_set & !is_wildcard,
    # TRUE if the last character of the post-hyphen digits is 0
    # (i.e. "19-1020" → "1020" → ends in 0 → group code)
    ends_in_zero = str_detect(str_sub(soc2010, 4), "0$"),
    match_type = case_when(
      is_wildcard                          ~ "wildcard",
      exact_in_exposure & !ends_in_zero   ~ "exact",
      TRUE                                 ~ "group"
    )
  )

cat("\nCrosswalk entry classification:\n")
xwalk %>% count(match_type) %>%
  mutate(pct = round(100 * n / sum(n), 1)) %>% print()

# ── 8. Resolve exposure scores for each match type ────────────

xwalk_exact <- xwalk %>% filter(match_type == "exact")
xwalk_group <- xwalk %>% filter(match_type == "group")
xwalk_wild  <- xwalk %>% filter(match_type == "wildcard")

# SOC codes that have their own explicit crosswalk entry AND an exact hit in the
# exposure data. These are excluded from wildcard pools because they already map
# to a DIFFERENT Census OCC2010 code via their own crosswalk row.
# e.g.: 15-1131, 15-1134 are explicitly mapped → excluded from 15-113X.
#       25-9041 is explicitly mapped → excluded from 25-90XX.
#
# NOTE: We do NOT exclude xwalk_group codes here. Those are broad group codes
# (e.g. "11-2020") that don't appear verbatim in the exposure data, so they
# can't be matched by the wildcard regex anyway. Adding them to explicit_soc_set
# risks over-exclusion if a detailed sub-code coincidentally shares a prefix.
explicit_soc_set <- xwalk_exact$soc2010

# ── Helper: employment-weighted average exposure ───────────────
# Weights come from BLS OES 2021 TOT_EMP attached to soc_exposure.
# Falls back to equal weights when OES data is missing for all matched
# codes (e.g. suppressed occupations, residual "All Other" categories).
# Any individual NA weight is imputed to the within-group mean so that
# codes with missing OES employment still contribute to the average.
#
# Only exposure measure columns are averaged; emp_oes itself is summed
# and stored as emp_oes_total for reference.
average_exposure <- function(matched_df, key_soc, n_matched) {
  w <- matched_df$emp_oes

  all_missing <- all(is.na(w) | w <= 0)
  if (all_missing) {
    w_use       <- rep(1.0, nrow(matched_df))
    wt_type     <- "equal"
  } else {
    w_mean  <- mean(w[!is.na(w) & w > 0])
    w_use   <- ifelse(is.na(w) | w <= 0, w_mean, w)
    wt_type <- "OES"
  }

  # Exposure measure columns only — exclude metadata (emp_oes, n_onet, etc.)
  measure_cols <- intersect(
    names(matched_df),
    c("genaiexp_estz_total", "genaiexp_estz_core", "genaiexp_estz_supplemental",
      "wfh_teleworkable", "aioe")
  )

  matched_df %>%
    summarise(across(all_of(measure_cols),
                     ~weighted.mean(.x, w = w_use, na.rm = TRUE))) %>%
    mutate(
      soc2010           = key_soc,
      n_soc_matched     = n_matched,
      soc_matched_codes = paste(matched_df$soc2010, collapse = "; "),
      emp_oes_total     = sum(w_use),
      weight_type       = wt_type
    )
}

## 8a. EXACT: direct join ───────────────────────────────────────
cat("\nExact matches: direct join...\n")

occ_exact <- xwalk_exact %>%
  left_join(
    soc_exposure %>%
      mutate(n_soc_matched     = 1L,
             soc_matched_codes = soc2010,
             emp_oes_total     = emp_oes,
             weight_type       = "exact"),
    by = "soc2010"
  )

cat(sprintf("  %d entries joined\n", nrow(occ_exact)))

## 8b. GROUP: family-prefix match, average sub-codes ─────────
# Use soc_family_prefix (trailing zeros stripped) so broad group codes
# reach their detailed sub-codes:
#   "25-1000" (soc_prefix "25-100") → family prefix "25-1"
#             → matches 25-1011, 25-1021, 25-1022 … in exposure data  ✓
#   "11-2020" (soc_prefix "11-202") → family prefix "11-202"
#             → matches 11-2021, 11-2022 …                             ✓
# Also exclude explicit_soc_set to avoid double-counting codes that have
# their own crosswalk entry (e.g. if 15-1131 sits under a group code too).
cat("\nGroup codes: family-prefix match (trailing zeros stripped)...\n")

resolve_group <- function(group_soc, family_prefix, exposure_df, exclude_soc = character(0)) {
  matched <- exposure_df %>%
    filter(str_starts(soc2010, family_prefix),
           !soc2010 %in% exclude_soc)
  if (nrow(matched) == 0) return(NULL)
  average_exposure(matched, group_soc, nrow(matched))
}

group_resolved <- map_dfr(seq_len(nrow(xwalk_group)), function(i) {
  row <- xwalk_group[i, ]
  res <- resolve_group(row$soc2010, row$soc_family_prefix, soc_exposure, explicit_soc_set)
  if (is.null(res)) {
    cat(sprintf("  [!] No match: %s  (family prefix: %s) — %s\n",
                row$soc2010, row$soc_family_prefix, row$title))
    return(NULL)
  }
  cat(sprintf("  %s  (family prefix: %s)  → %d codes: %s\n",
              row$soc2010, row$soc_family_prefix,
              res$n_soc_matched, res$soc_matched_codes))
  res
})

group_ok <- nrow(group_resolved) > 0 && "soc2010" %in% names(group_resolved)

occ_group <- if (group_ok) {
  xwalk_group %>% left_join(group_resolved, by = "soc2010")
} else {
  cat("  [!] No group codes resolved — all will have NA exposure.\n")
  xwalk_group
}

## 8c. WILDCARD: regex match, excluding explicit codes ──────────
# Each X in the crosswalk SOC code is a digit placeholder.
# We replace X with [0-9] (NOT \d) — stringr's replacement processor
# strips the backslash from \d, collapsing it to the literal letter d.
# [0-9] is unambiguous and handles both single-X and multi-X codes:
#   "15-113X"  → ^15-113[0-9]$    (matches 15-1132, 15-1133, etc.)
#   "25-90XX"  → ^25-90[0-9][0-9]$ (matches 25-9011, 25-9021, etc.)
#
# Codes in explicit_soc_set are excluded because they map to a DIFFERENT
# Census code via their own crosswalk row.
# e.g.: 15-1131, 15-1134 have explicit entries → excluded from 15-113X.
#       25-9041 has an explicit entry → excluded from 25-90XX.
cat("\nWildcard codes: regex match (excluding explicitly-mapped codes)...\n")
cat(sprintf("  Excluding %d explicitly-mapped SOC codes from wildcard pools\n",
            length(explicit_soc_set)))

resolve_wildcard <- function(wild_soc, exposure_df, exclude_soc) {
  # Replace every X with [0-9] — works for both single-X ("15-113X") and
  # multi-X ("25-90XX") wildcards. Using [0-9] avoids the stringr replacement
  # escape issue where \d collapses to literal d.
  inner   <- str_replace_all(wild_soc, "X", "[0-9]")
  pattern <- paste0("^", inner, "$")
  matched <- exposure_df %>%
    filter(str_detect(soc2010, pattern),
           !soc2010 %in% exclude_soc)
  if (nrow(matched) == 0) return(NULL)
  average_exposure(matched, wild_soc, nrow(matched))
}

wild_resolved <- map_dfr(xwalk_wild$soc2010, function(ws) {
  inner   <- str_replace_all(ws, "X", "[0-9]")
  pattern <- paste0("^", inner, "$")
  res <- resolve_wildcard(ws, soc_exposure, explicit_soc_set)
  if (is.null(res)) {
    cat(sprintf("  [!] No match: %s  (regex: %s)\n", ws, pattern))
    return(NULL)
  }
  cat(sprintf("  %s  (regex: %s)  → %d codes: %s\n",
              ws, pattern, res$n_soc_matched, res$soc_matched_codes))
  res
})

wild_ok <- nrow(wild_resolved) > 0 && "soc2010" %in% names(wild_resolved)

occ_wild <- if (wild_ok) {
  xwalk_wild %>% left_join(wild_resolved, by = "soc2010")
} else {
  cat("  [!] No wildcard codes resolved — all will have NA exposure.\n")
  xwalk_wild
}

# ── 9. Combine and finalise ────────────────────────────────────
occ_exposure <- bind_rows(occ_exact, occ_group, occ_wild) %>%
  select(
    occ2010, soc2010, occ_title = title,
    match_type, weight_type, n_soc_matched,
    emp_oes_total, soc_matched_codes,
    genai_total  = genaiexp_estz_total,
    genai_core   = genaiexp_estz_core,
    genai_supp   = genaiexp_estz_supplemental,
    wfh          = wfh_teleworkable,
    matches("^aioe")
  ) %>%
  arrange(occ2010)

# Percentile ranks (preferred treatment variable form in regressions)
occ_exposure <- occ_exposure %>%
  mutate(
    genai_total_pctile = percent_rank(genai_total),
    genai_core_pctile  = percent_rank(genai_core),
    genai_supp_pctile  = percent_rank(genai_supp),
    wfh_pctile         = percent_rank(wfh)
  )

cat(sprintf("\nFinal table: %d OCC2010 codes\n", nrow(occ_exposure)))

# ── 10. Coverage diagnostics ──────────────────────────────────
cat("\n── Coverage by match type ──\n")
occ_exposure %>%
  group_by(match_type) %>%
  summarise(
    n            = n(),
    genai_ok     = sum(!is.na(genai_total)),
    wfh_ok       = sum(!is.na(wfh)),
    aioe_ok_n    = if ("aioe" %in% names(.)) sum(!is.na(aioe)) else NA_integer_,
    avg_n_soc    = round(mean(n_soc_matched, na.rm = TRUE), 1),
    .groups = "drop"
  ) %>% print()

cat("\n── Overall coverage ──\n")
n_total    <- nrow(occ_exposure)
n_genai_ok <- sum(!is.na(occ_exposure$genai_total))
n_wfh_ok   <- sum(!is.na(occ_exposure$wfh))

cat(sprintf("  OCC2010 codes total:      %d\n", n_total))
cat(sprintf("  With GenAI score:         %d (%.1f%%)\n",
            n_genai_ok, 100 * n_genai_ok / n_total))
cat(sprintf("  With WFH score:           %d (%.1f%%)\n",
            n_wfh_ok, 100 * n_wfh_ok / n_total))

no_genai <- occ_exposure %>%
  filter(is.na(genai_total)) %>%
  select(occ2010, soc2010, match_type, occ_title)
if (nrow(no_genai) > 0) {
  cat(sprintf("\n  OCC codes with no GenAI score (%d) — inspect for data gaps:\n",
              nrow(no_genai)))
  print(no_genai, n = Inf)
}

# ── 10b. Exposure SOC codes with no crosswalk mapping ─────────
# These codes appear in GenAI / WFH / AIOE but could not be linked to
# any CPS OCC2010 code (neither as an exact match nor as a sub-code
# captured by a group/wildcard entry).  They are excluded from the
# final output and contribute nothing to the regression sample.
#
# How we collect "used" codes:
#   • exact entries: soc_matched_codes = the code itself
#   • group/wildcard entries: soc_matched_codes = "A; B; C …"
# Splitting and pooling gives every soc_exposure code that was used.
soc_codes_used <- occ_exposure %>%
  filter(!is.na(soc_matched_codes)) %>%
  pull(soc_matched_codes) %>%
  str_split(";\\s*") %>%
  unlist() %>%
  str_trim() %>%
  discard(~ is.na(.x) | .x == "") %>%
  unique()

not_in_xwalk <- soc_exposure %>%
  filter(!soc2010 %in% soc_codes_used) %>%
  select(soc2010,
         genai_total = genaiexp_estz_total,
         wfh         = wfh_teleworkable,
         matches("^aioe")) %>%
  arrange(soc2010)

cat(sprintf(
  "\n── Exposure SOC codes with no crosswalk mapping: %d (out of %d) ──\n",
  nrow(not_in_xwalk), nrow(soc_exposure)))

if (nrow(not_in_xwalk) > 0) {
  print(not_in_xwalk, n = Inf)
  write_csv(not_in_xwalk,
            file.path(TABLE_DIR, "01_unmatched_exposure_soc.csv"))
  cat("  Saved: output/tables/01_unmatched_exposure_soc.csv\n")
} else {
  cat("  All exposure SOC codes are covered by the crosswalk.\n")
}

# ── 11. Save ──────────────────────────────────────────────────
write_csv(occ_exposure, file.path(PROC_DIR, "occ_ai_exposure.csv"))
cat(sprintf("\nSaved: data/processed/occ_ai_exposure.csv\n"))

coverage_summary <- tibble(
  measure     = c("GenAI total", "GenAI core", "GenAI supp.",
                  "WFH", if (isTRUE(aioe_ok)) "AIOE" else NULL),
  n_occ2010   = n_total,
  n_matched   = c(n_genai_ok,
                  sum(!is.na(occ_exposure$genai_core)),
                  sum(!is.na(occ_exposure$genai_supp)),
                  n_wfh_ok,
                  if (isTRUE(aioe_ok) && "aioe" %in% names(occ_exposure))
                    sum(!is.na(occ_exposure$aioe)) else NULL),
  pct_matched = round(100 * n_matched / n_occ2010, 1)
)
write_csv(coverage_summary, file.path(TABLE_DIR, "01_merge_coverage.csv"))
cat("Saved: output/tables/01_merge_coverage.csv\n")

# ── 12. Distribution check ────────────────────────────────────
cat("\n── GenAI / WFH distribution across OCC2010 codes ──\n")
occ_exposure %>%
  summarise(across(c(genai_total, genai_core, genai_supp, wfh),
                   list(mean = ~mean(.x, na.rm = TRUE),
                        sd   = ~sd(.x,   na.rm = TRUE),
                        p25  = ~quantile(.x, .25, na.rm = TRUE),
                        p50  = ~median(.x,        na.rm = TRUE),
                        p75  = ~quantile(.x, .75, na.rm = TRUE)),
                   .names = "{.col}__{.fn}")) %>%
  pivot_longer(everything(), names_to = c("var", "stat"), names_sep = "__") %>%
  pivot_wider(names_from = stat, values_from = value) %>%
  print()

cat("\n── WFH × GenAI correlation ──\n")
with(occ_exposure %>% filter(!is.na(genai_total), !is.na(wfh)),
     cat(sprintf("  cor(genai_total, wfh) = %.3f\n", cor(genai_total, wfh))))

cat("\n", strrep("=", 60), "\n")
cat("MERGE COMPLETE\n")
cat("Next: source('code/02_clean_cps.R') then merge occ_ai_exposure\n")
cat("      by c('OCC2010' = 'occ2010')\n")
cat(strrep("=", 60), "\n")
