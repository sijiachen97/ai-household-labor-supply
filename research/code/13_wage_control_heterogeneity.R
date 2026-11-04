# =============================================================================
# 13_wage_control_heterogeneity.R
# (A) A rival-attribute control we have been missing: occupation mean WAGE.
# (B,C) Two PRE-SPECIFIED heterogeneity tests with directional predictions.
#
# -----------------------------------------------------------------------------
# READ THIS BEFORE INTERPRETING ANYTHING
# -----------------------------------------------------------------------------
# The aggregate W->H effect is 0.287 (0.158), p = 0.069, and is NOT robust to
# discretization (quartile Q2 = +0.32 vs tercile T2 = -0.10). Five separate
# attempts to establish it have failed (binary pooling, Felten cross-check,
# tercile design, zero-dose reference, core/supp decomposition).
#
# Therefore:
#   * Splitting the sample REDUCES power. Heterogeneity cannot rescue
#     significance and is not intended to. What it CAN do is show whether the
#     effect concentrates where theory says it should — which would be a
#     different, and defensible, finding.
#   * Only TWO subgroup splits are run, both with a directional prediction
#     stated in advance (below). This is deliberately not a grid search. Adding
#     more splits later would convert this into specification search and must
#     be disclosed if done.
#   * The wage control (Part A) is different in kind: it can change the POINT
#     ESTIMATE by removing a rival explanation, not just the precision.
#
# -----------------------------------------------------------------------------
# PRE-SPECIFIED PREDICTIONS (recorded before estimation)
# -----------------------------------------------------------------------------
# (B) WIFE'S ECONOMIC WEIGHT.  The insurance motive should scale with how much
#     household income is at risk. If the wife is a minor earner, her
#     displacement risk barely matters for household finances and the husband
#     has little reason to add hours.
#     PREDICTION: in W->H, the effect is LARGER where the wife's economic weight
#     is high, i.e. the triple-interaction coefficient is POSITIVE.
#
# (C) PRESENCE OF CHILDREN.  Households with children have less financial slack
#     and a stronger precautionary motive.
#     PREDICTION: in W->H, the effect is LARGER for couples with children, i.e.
#     the triple-interaction coefficient is POSITIVE.
#
#   H->W is run symmetrically for completeness, but we have already attributed
#   that direction to teleworkability rather than AI, so no prediction is made.
#
# -----------------------------------------------------------------------------
# WAGE MEASURE
# -----------------------------------------------------------------------------
#   OES May 2018 national annual mean wage (A_MEAN), mapped from SOC 2010 to
#   Census OCC2010 using the `soc_matched_codes` column already built in
#   01_merge_ai_exposure.R (semicolon-separated), employment-weighted by TOT_EMP.
#   Pre-treatment by construction (2018), occupation-level, so not a bad control.
#
#   WHY IT MATTERS: AI exposure correlates strongly with occupational wages, and
#   high-wage white-collar occupations had a distinctive post-2022 trajectory
#   (white-collar hiring cycle, interest rates). Nothing in the design so far
#   controls for this. It is the same logic as the teleworkability horse race,
#   applied to a second rival attribute.
#
# Inputs:  data/processed/cps_couples.csv
#          data/processed/occ_ai_exposure.csv   (soc_matched_codes)
#          data/raw/national_M2018_dl.xlsx      (TOT_EMP, A_MEAN)
# Outputs: output/wage_hetero/tables/13_occ_wage.csv
#          output/wage_hetero/tables/13_wage_control.csv
#          output/wage_hetero/tables/13_heterogeneity.csv
#          output/wage_hetero/figures/13_wage_control_<dir>.pdf
#          output/wage_hetero/figures/13_heterogeneity_<dir>.pdf
# =============================================================================

library(tidyverse)
library(fixest)
library(data.table)
library(readxl)

ROOT     <- path.expand(".")
PROC_DIR <- file.path(ROOT, "data", "processed")
RAW_DIR  <- file.path(ROOT, "data", "raw")
FIG_DIR  <- file.path(ROOT, "output", "wage_hetero", "figures")
TAB_DIR  <- file.path(ROOT, "output", "wage_hetero", "tables")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

WINDOW_START <- -10L
OUTCOMES <- c("inlf", "emp", "hrs", "hrs_main", "fulltime")
OUTCOME_LAB <- c(
  inlf = "Labor force participation", emp = "Employed (EMPSTAT)",
  hrs = "Usual weekly hours (all jobs)", hrs_main = "Usual weekly hours (main job)",
  fulltime = "Full-time (35+ hrs)")
# NOTE: every spec includes occupation FE — labels spell this out.
SPEC_LAB <- c("1" = "(1) occ+ym", "2" = "(2) occ+ind×ym", "4" = "(4) occ+ind×ym+st×ym")

# =============================================================================
# 1. Build occupation mean wage (OES 2018 -> OCC2010)
# =============================================================================

oes <- read_excel(file.path(RAW_DIR, "national_M2018_dl.xlsx")) %>%
  select(soc = OCC_CODE, emp = TOT_EMP, wage = A_MEAN) %>%
  mutate(emp  = suppressWarnings(as.numeric(gsub("[^0-9.]", "", emp))),
         wage = suppressWarnings(as.numeric(gsub("[^0-9.]", "", wage)))) %>%
  filter(!is.na(wage))

xw <- fread(file.path(PROC_DIR, "occ_ai_exposure.csv")) %>% as_tibble() %>%
  select(occ2010, soc_matched_codes) %>%
  filter(!is.na(soc_matched_codes), soc_matched_codes != "") %>%
  separate_rows(soc_matched_codes, sep = ";\\s*") %>%
  rename(soc = soc_matched_codes) %>%
  mutate(soc = trimws(soc))

occ_wage <- xw %>%
  left_join(oes, by = "soc") %>%
  filter(!is.na(wage)) %>%
  group_by(occ2010) %>%
  summarise(
    # employment-weighted where employment is available, equal weights otherwise
    occ_wage = if (all(is.na(emp)) || sum(emp, na.rm = TRUE) == 0)
                 mean(wage, na.rm = TRUE)
               else weighted.mean(wage, w = emp, na.rm = TRUE),
    n_soc = n(), .groups = "drop") %>%
  mutate(occ2010 = as.integer(occ2010),
         log_wage = log(occ_wage))
fwrite(occ_wage, file.path(TAB_DIR, "13_occ_wage.csv"))
message(sprintf("Occupation wages built for %d OCC2010 codes.", nrow(occ_wage)))
print(summary(occ_wage$occ_wage))

# =============================================================================
# 2. Load CPS, prep, merge wages
# =============================================================================

d <- fread(file.path(PROC_DIR, "cps_couples.csv")) %>% as_tibble()
d <- d %>%
  filter(rel_month >= WINDOW_START) %>%
  filter((a_SEX == 1 & b_SEX == 2) | (a_SEX == 2 & b_SEX == 1))

d$husb_is_a <- d$a_SEX == 1
hw_vars <- c("genai_total_pctile_s", "wfh", "OCC2010", "IND1990", "AGE",
             "educ_cat", "white_nh", "hispan",
             "inlf", "emp", "hrs", "hrs_main", "fulltime", "WTFINL")
for (v in hw_vars) {
  av <- d[[paste0("a_", v)]]; bv <- d[[paste0("b_", v)]]
  d[[paste0("h_", v)]] <- ifelse(d$husb_is_a, av, bv)
  d[[paste0("w_", v)]] <- ifelse(d$husb_is_a, bv, av)
}
d$h_tele <- ifelse(is.na(d$h_wfh), NA_integer_, as.integer(d$h_wfh > 0.5))
d$w_tele <- ifelse(is.na(d$w_wfh), NA_integer_, as.integer(d$w_wfh > 0.5))

# merge occupation wage for each spouse
d <- d %>%
  left_join(occ_wage %>% select(occ2010, h_logwage = log_wage, h_occwage = occ_wage),
            by = c("h_OCC2010" = "occ2010")) %>%
  left_join(occ_wage %>% select(occ2010, w_logwage = log_wage, w_occwage = occ_wage),
            by = c("w_OCC2010" = "occ2010"))

message(sprintf("Wage merged: husband %.1f%%, wife %.1f%% of couple-months",
                100 * mean(!is.na(d$h_occwage)), 100 * mean(!is.na(d$w_occwage))))

# --- treatment: binary scheme A (exposed = above bottom quartile) -------------
for (s in c("h", "w")) {
  pct <- d[[paste0(s, "_genai_total_pctile_s")]]
  d[[paste0(s, "_exposed")]] <- as.integer(pct > quantile(pct, .25, na.rm = TRUE, type = 7))
}

# --- (B) wife's economic weight ----------------------------------------------
d$wife_wage_share <- d$w_occwage / (d$w_occwage + d$h_occwage)
med_share <- median(d$wife_wage_share, na.rm = TRUE)
d$hi_wifewt <- ifelse(is.na(d$wife_wage_share), NA_integer_,
                      as.integer(d$wife_wage_share > med_share))

# --- (C) presence of children -------------------------------------------------
d$hi_child <- as.integer(d$has_child == 1)

message(sprintf("\nWife wage share: median %.3f; share of couples above = %.3f",
                med_share, mean(d$hi_wifewt, na.rm = TRUE)))
message(sprintf("Couples with children: %.3f", mean(d$hi_child, na.rm = TRUE)))

KEEP <- c("ym", "post", "coupleID", "n_children", "has_child", "has_child_u5", "STATEFIP",
          "h_tele", "w_tele", "hi_wifewt", "hi_child", "wife_wage_share",
          grep("^(h|w)_", names(d), value = TRUE))
d <- d %>% select(all_of(unique(intersect(KEEP, names(d))))); gc()

CTRL <- paste(
  "h_AGE + I(h_AGE^2) + w_AGE + I(w_AGE^2)",
  "+ factor(h_educ_cat) + factor(w_educ_cat)",
  "+ h_white_nh + w_white_nh + h_hispan + w_hispan",
  "+ n_children")

DIRECTIONS <- list(
  HtoW = list(side_t = "h", side_o = "w", label = "Husband's exposure -> Wife's labor supply"),
  WtoH = list(side_t = "w", side_o = "h", label = "Wife's exposure -> Husband's labor supply"))

fe_string <- function(spec, st) {
  occ <- paste0(st, "_OCC2010"); ind <- paste0(st, "_IND1990")
  switch(spec, "1" = sprintf("%s + ym", occ),
               "2" = sprintf("%s + %s^ym", occ, ind),
               "4" = sprintf("%s + %s^ym + STATEFIP^ym", occ, ind))
}
grab <- function(m, pat) {
  pc <- as.data.frame(coeftable(m)); r <- grep(pat, rownames(pc))[1]
  if (is.na(r)) c(NA_real_, NA_real_) else c(pc[r, 1], pc[r, 2])
}

# =============================================================================
# 3. PART A — wage as a rival-attribute control
# =============================================================================
#   base : treated:post + tele:post                       (current primary)
#   wage : treated:post + tele:post + logwage:post        (adds the rival)
# A large drop in the treated coefficient means the "AI effect" was partly an
# occupational-wage trend.

wc_rows <- list()
for (dn in names(DIRECTIONS)) {
  cfg <- DIRECTIONS[[dn]]; st <- cfg$side_t; so <- cfg$side_o
  occ <- paste0(st, "_OCC2010"); tele <- paste0(st, "_tele")
  expo <- paste0(st, "_exposed"); lw <- paste0(st, "_logwage")
  sub <- d[!is.na(d[[expo]]) & !is.na(d[[lw]]), ]
  for (oc in OUTCOMES) {
    yv <- paste0(so, "_", oc); wt <- paste0(so, "_WTFINL")
    for (spec in c("1", "2", "4")) for (mod in c("base", "wage")) {
      message(sprintf("Wage control | %s | %s | spec %s | %s", dn, oc, spec, mod))
      rhs <- sprintf("%s:post + %s:post + %s", expo, tele, CTRL)
      if (mod == "wage") rhs <- sprintf("%s + %s:post", rhs, lw)
      m <- tryCatch(
        feols(as.formula(sprintf("%s ~ %s | %s", yv, rhs, fe_string(spec, st))),
              data = sub, weights = as.formula(paste0("~", wt)),
              cluster = as.formula(paste0("~", occ, " + coupleID")),
              lean = TRUE, mem.clean = TRUE, notes = FALSE),
        error = function(e) { message("  FAILED: ", conditionMessage(e)); NULL })
      if (is.null(m)) next
      tr <- grab(m, paste0("^", expo, ":post$|^post:", expo, "$"))
      wg <- if (mod == "wage") grab(m, paste0(lw, ".*post|post.*", lw)) else c(NA, NA)
      wc_rows[[paste(dn, oc, spec, mod)]] <- data.frame(
        direction = dn, outcome = oc, fe_spec = spec, model = mod,
        att = tr[1], se = tr[2], p_value = 2 * pnorm(-abs(tr[1] / tr[2])),
        wage_coef = wg[1], wage_se = wg[2], n_obs = nobs(m))
      rm(m); gc(verbose = FALSE)
    }
  }
  rm(sub); gc(verbose = FALSE)
}
wc <- bind_rows(wc_rows)
fwrite(wc, file.path(TAB_DIR, "13_wage_control.csv"))

message("\n=== PART A: does the effect survive an occupation-wage control? (spec 2) ===")
print(wc %>% filter(fe_spec == "2") %>%
        mutate(est = sprintf("%.4f (%.4f) p=%.3f", att, se, p_value)) %>%
        select(direction, outcome, model, est) %>%
        pivot_wider(names_from = model, values_from = est))

# =============================================================================
# 4. PARTS B & C — pre-specified heterogeneity
# =============================================================================
#   y ~ hi + treated:hi + treated:post + hi:post + treated:hi:post + tele:post
# The triple interaction `treated:hi:post` is the differential effect.
# PREDICTION for W->H: POSITIVE for both splits.

het_rows <- list()
for (dn in names(DIRECTIONS)) {
  cfg <- DIRECTIONS[[dn]]; st <- cfg$side_t; so <- cfg$side_o
  occ <- paste0(st, "_OCC2010"); tele <- paste0(st, "_tele"); expo <- paste0(st, "_exposed")
  for (split in c("hi_wifewt", "hi_child")) {
    sub <- d[!is.na(d[[expo]]) & !is.na(d[[split]]), ]
    for (oc in OUTCOMES) {
      yv <- paste0(so, "_", oc); wt <- paste0(so, "_WTFINL")
      for (spec in c("1", "2", "4")) {
        message(sprintf("Hetero | %s | %s | %s | spec %s", dn, split, oc, spec))
        rhs <- sprintf("%s + %s:%s + %s:post + %s:post + %s:%s:post + %s:post + %s",
                       split, expo, split, expo, split, expo, split, tele, CTRL)
        m <- tryCatch(
          feols(as.formula(sprintf("%s ~ %s | %s", yv, rhs, fe_string(spec, st))),
                data = sub, weights = as.formula(paste0("~", wt)),
                cluster = as.formula(paste0("~", occ, " + coupleID")),
                lean = TRUE, mem.clean = TRUE, notes = FALSE),
          error = function(e) { message("  FAILED: ", conditionMessage(e)); NULL })
        if (is.null(m)) next
        # main effect (low group) and differential (high group)
        base <- grab(m, paste0("^", expo, ":post$|^post:", expo, "$"))
        # triple interaction: match a term containing expo, split and post
        pc <- as.data.frame(coeftable(m)); nm <- rownames(pc)
        idx <- which(grepl(expo, nm) & grepl(split, nm) & grepl("post", nm))
        dif <- if (length(idx)) c(pc[idx[1], 1], pc[idx[1], 2]) else c(NA_real_, NA_real_)
        het_rows[[paste(dn, split, oc, spec)]] <- data.frame(
          direction = dn, split = split, outcome = oc, fe_spec = spec,
          att_low = base[1], se_low = base[2],
          p_low = 2 * pnorm(-abs(base[1] / base[2])),
          att_diff = dif[1], se_diff = dif[2],
          p_diff = 2 * pnorm(-abs(dif[1] / dif[2])),
          att_high = base[1] + dif[1],
          n_obs = nobs(m))
        rm(m); gc(verbose = FALSE)
      }
    }
    rm(sub); gc(verbose = FALSE)
  }
}
het <- bind_rows(het_rows)
fwrite(het, file.path(TAB_DIR, "13_heterogeneity.csv"))

message("\n=== PARTS B & C: heterogeneity, spec (2). PREDICTION: att_diff > 0 in W->H ===")
print(het %>% filter(fe_spec == "2") %>%
        mutate(low  = sprintf("%.3f (%.3f)", att_low, se_low),
               diff = sprintf("%.3f (%.3f) p=%.3f", att_diff, se_diff, p_diff),
               high = sprintf("%.3f", att_high)) %>%
        select(direction, split, outcome, low, diff, high))

# =============================================================================
# 5. Figures
# =============================================================================

for (dn in names(DIRECTIONS)) {
  # Labels spell out the FULL model — both variants include tele×post; the wage
  # variant ADDS the wage term. Never abbreviate to "+ wage×post" alone, which
  # reads as if tele×post had been dropped.
  gw <- wc %>% filter(direction == dn) %>%
    mutate(spec_lab = SPEC_LAB[fe_spec],
           model = if_else(model == "base",
                           "tele×post",
                           "tele×post + occ wage×post"),
           outcome = factor(outcome, levels = OUTCOMES, labels = OUTCOME_LAB[OUTCOMES]))
  g <- ggplot(gw, aes(spec_lab, att, colour = model)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
    geom_pointrange(aes(ymin = att - 1.96 * se, ymax = att + 1.96 * se),
                    position = position_dodge(width = .5), size = .3) +
    facet_wrap(~ outcome, scales = "free_y", ncol = 2) + coord_flip() +
    labs(title = paste0(DIRECTIONS[[dn]]$label, " — occupation-wage control"),
         subtitle = "Both variants include occupation FE, year-month controls and tele×post; the second ADDS log(occupation mean wage)×post.\nTreatment = binary scheme A (exposed = above bottom quartile) — the wage control has NOT been tested on other cutting schemes.",
         x = NULL, y = "Post-period ATT vs. unexposed", colour = NULL) +
    theme_minimal(base_size = 9) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  ggsave(file.path(FIG_DIR, sprintf("13_wage_control_%s.pdf", dn)), g, width = 10, height = 8)

  gh <- het %>% filter(direction == dn) %>%
    mutate(split = recode(split, hi_wifewt = "Wife's economic weight (high vs low)",
                          hi_child = "Has children (yes vs no)"),
           spec_lab = SPEC_LAB[fe_spec],
           outcome = factor(outcome, levels = OUTCOMES, labels = OUTCOME_LAB[OUTCOMES]))
  g2 <- ggplot(gh, aes(spec_lab, att_diff, colour = split)) +
    geom_hline(yintercept = 0, linetype = "dashed", colour = "grey55") +
    geom_pointrange(aes(ymin = att_diff - 1.96 * se_diff, ymax = att_diff + 1.96 * se_diff),
                    position = position_dodge(width = .5), size = .3) +
    facet_wrap(~ outcome, scales = "free_y", ncol = 2) + coord_flip() +
    labs(title = paste0(DIRECTIONS[[dn]]$label, " — pre-specified heterogeneity"),
         subtitle = "Differential effect in the HIGH group (triple interaction).\nPre-specified prediction for W→H: positive for both splits.",
         x = NULL, y = "Differential ATT (high group − low group)", colour = NULL) +
    theme_minimal(base_size = 9) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  ggsave(file.path(FIG_DIR, sprintf("13_heterogeneity_%s.pdf", dn)), g2, width = 10, height = 8)
}

message("\n=== 13_wage_control_heterogeneity.R complete ===")
message("Read in order:")
message(" 1. PART A — does the effect survive the occupation-wage control?")
message("    (this can move the POINT ESTIMATE; it is a rival-explanation test)")
message(" 2. PARTS B & C — is att_diff POSITIVE in W->H, as predicted?")
message("    Both predicted-positive => coherent concentration even with a weak average.")
message("    Mixed or wrong-signed => the insurance story gets no support.")
message("\nReminder: subgroup splits reduce power. Do not read an insignificant")
message("att_diff as evidence of homogeneity, and do not add further splits.")
