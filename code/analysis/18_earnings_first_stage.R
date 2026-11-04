# =============================================================================
# 18_ai_winners.R
# The original motivating hypothesis, finally tested directly:
# do WEALTH EFFECTS in AI-dividend households reduce spousal labor supply?
#
# -----------------------------------------------------------------------------
# BACKGROUND
# -----------------------------------------------------------------------------
# The project's original observation: spouses of AI-boom beneficiaries (ML/
# software engineers, tech managers with surging pay and equity) seemed to lose
# "employment anxiety" — a wealth effect pushing the spouse toward less work.
# The EXPOSURE measures used in scripts 03-15 cannot see this: they score task
# substitutability, pooling AI winners and AI losers into the same "exposed"
# group, where opposite-signed spousal responses would cancel — one candidate
# explanation for the overall null.
#
# Separately, the surviving H->W finding (wives of TELEWORKABLE husbands cut
# hours post-2022Q4) may partly BE this wealth effect: AI-winner occupations
# sit almost entirely inside the teleworkable group, and the horse race only
# ever tested teleworkability against the exposure SCORE, never against
# AI-winner OCCUPATIONS.
#
# SCOPE DISCIPLINE: this script tests the WINNER/wealth side only. The
# loser/insurance side is the exhausted W->H search (scripts 10-15) and stays
# closed under the stopping rule. Parts 1-2 probe the composition of the
# SURVIVING H->W phenomenon.
#
# -----------------------------------------------------------------------------
# DESIGN: three parts
# -----------------------------------------------------------------------------
# PART 0 — FIRST STAGE (descriptive; ORG subsample, EARNWT weights)
#   Did AI-core husbands' earnings actually rise relative to comparable
#   professionals after 2022Q4? EARNWEEK trajectories: weighted median and
#   topcode share (the topcode ~ $2,885/wk bites exactly in this population,
#   so the SHARE AT TOPCODE is itself an outcome; median is topcode-robust).
#   Also: exposed vs unexposed WIVES' earnings paths — the never-checked first
#   stage of the old insurance story (descriptive only; that search stays closed).
#   ⚠️ EARNWEEK misses equity (RSUs/options) — the likely bulk of the AI
#   dividend. A flat wage first stage therefore does NOT kill the wealth
#   channel; it only closes the WAGE version of it. Stated in advance.
#
# PART 1 — WEALTH-EFFECT TEST (within-teleworkable DiD)
#   Wives of AI-CORE husbands vs wives of COMPARABLE PROFESSIONAL husbands
#   (lawyers, accountants, financial managers, ... — all D&N teleworkable,
#   similar wage levels). Both groups teleworkable => the tele channel is held
#   fixed BY CONSTRUCTION; log occ wage x post added for residual wage-level
#   differences. PREDICTION (pre-stated, from the motivating anecdote): wives
#   of AI-core husbands reduce labor supply relative to comparator wives, with
#   the EXTENSIVE margin ("no more employment anxiety") the emphasized outcome.
#
# PART 2 — WHOSE TELEWORK IS IT? (husband vs wife tele horse race)
#   The H->W finding used the HUSBAND's teleworkability. But spouses match
#   assortatively on teleworkability, so the wife's own remote-work adjustment
#   (e.g., remote-linked hour reductions, Pabilonia-Vernon) could masquerade
#   as an intra-household spillover. Enter h_tele:post and w_tele:post jointly.
#   If the wife's own term absorbs the effect, the phenomenon is not
#   "intra-household" at all.
#
# POWER NOTE (pre-stated): AI-core husbands are ~4-5% of couple-months. Read
# signs and trajectories, not p-values alone; an insignificant Part-1 estimate
# with the predicted sign is weak support, not refutation — and a clean zero
# closes the wage-visible version of the hypothesis in this data.
#
# Inputs:  data/processed/cps_couples.csv
#          output/wage_hetero/tables/13_occ_wage.csv
# Outputs: output/ai_winners/tables/18_group_defs.csv
#          output/ai_winners/tables/18_firststage_earnings.csv
#          output/ai_winners/tables/18_wealth_did.csv
#          output/ai_winners/tables/18_tele_horserace.csv
#          output/ai_winners/tables/18_symmetric.csv      (Part 3 hardening)
#          output/ai_winners/figures/18_firststage_<panel>.pdf
#          output/ai_winners/figures/18_wealth_did.pdf
#          output/ai_winners/figures/18_tele_horserace.pdf
#          output/ai_winners/figures/18_symmetric.pdf
# =============================================================================

library(tidyverse)
library(fixest)
library(data.table)

ROOT     <- path.expand(".")
PROC_DIR <- file.path(ROOT, "data", "processed")
FIG_DIR  <- file.path(ROOT, "output", "ai_winners", "figures")
TAB_DIR  <- file.path(ROOT, "output", "ai_winners", "tables")
dir.create(FIG_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(TAB_DIR, recursive = TRUE, showWarnings = FALSE)

WAGE_CSV <- file.path(ROOT, "output", "wage_hetero", "tables", "13_occ_wage.csv")
WINDOW_START <- -10L
OUTCOMES <- c("inlf", "emp", "hrs", "hrs_main", "fulltime")
OUTCOME_LAB <- c(
  inlf = "Labor force participation", emp = "Employed (EMPSTAT)",
  hrs = "Usual weekly hours (all jobs)", hrs_main = "Usual weekly hours (main job)",
  fulltime = "Full-time (35+ hrs)")
# NOTE: every spec includes occupation FE — labels spell this out.
SPEC_LAB <- c("1" = "(1) occ+ym", "2" = "(2) occ+ind×ym", "4" = "(4) occ+ind×ym+st×ym")

# -----------------------------------------------------------------------------
# Occupation groups (OCC2010 codes; titles from occ_ai_exposure.csv)
# -----------------------------------------------------------------------------
# AI-CORE: the computer/tech family — the population of the motivating anecdote
AI_CORE <- c(
  110,   # Computer and information systems managers
  1000,  # Computer scientists and systems analysts
  1010,  # Computer programmers
  1020,  # Software developers, applications and systems software
  1030,  # Web developers
  1050,  # Computer support specialists
  1060,  # Database administrators
  1105,  # Network and computer systems administrators
  1106,  # Computer network architects
  1107)  # Computer occupations, all other

# COMPARATORS: teleworkable, professional, similar wage band, NOT AI-adjacent.
# Engineers deliberately excluded (semiconductor/hardware engineers plausibly
# share the AI dividend, which would contaminate the control group).
COMPARATOR <- c(
  50,    # Marketing and sales managers
  120,   # Financial managers
  630,   # Human resources workers
  710,   # Management analysts
  800,   # Accountants and auditors
  840,   # Financial analysts
  850,   # Personal financial advisors
  2100)  # Lawyers

# =============================================================================
# 1. Load & prep
# =============================================================================

d <- fread(file.path(PROC_DIR, "cps_couples.csv")) %>% as_tibble()
d <- d %>%
  filter(rel_month >= WINDOW_START) %>%
  filter((a_SEX == 1 & b_SEX == 2) | (a_SEX == 2 & b_SEX == 1))

d$husb_is_a <- d$a_SEX == 1
hw_vars <- c("genai_total_pctile_s", "wfh", "OCC2010", "IND1990", "AGE",
             "educ_cat", "white_nh", "hispan",
             "inlf", "emp", "hrs", "hrs_main", "fulltime",
             "WTFINL", "EARNWEEK2", "ELIGORG", "EARNWT")
for (v in hw_vars) {
  av <- d[[paste0("a_", v)]]; bv <- d[[paste0("b_", v)]]
  d[[paste0("h_", v)]] <- ifelse(d$husb_is_a, av, bv)
  d[[paste0("w_", v)]] <- ifelse(d$husb_is_a, bv, av)
}
d$h_tele <- ifelse(is.na(d$h_wfh), NA_integer_, as.integer(d$h_wfh > 0.5))
d$w_tele <- ifelse(is.na(d$w_wfh), NA_integer_, as.integer(d$w_wfh > 0.5))
d$rel_qtr <- as.integer(floor(d$rel_month / 3))

wage <- fread(WAGE_CSV) %>% as_tibble() %>% select(occ2010, log_wage)
d <- d %>%
  left_join(wage %>% rename(h_logwage = log_wage), by = c("h_OCC2010" = "occ2010")) %>%
  left_join(wage %>% rename(w_logwage = log_wage), by = c("w_OCC2010" = "occ2010"))

d$h_grp <- case_when(
  d$h_OCC2010 %in% AI_CORE    ~ "ai_core",
  d$h_OCC2010 %in% COMPARATOR ~ "comparator",
  TRUE                        ~ NA_character_)

# Group definition table: sizes, tele share, mean occ wage — verifies the
# "same tele, similar wage" premise of the within-teleworkable design.
gd <- d %>% filter(!is.na(h_grp)) %>%
  group_by(h_grp) %>%
  summarise(n_couple_months = n(),
            share_tele = mean(h_tele, na.rm = TRUE),
            mean_logwage = mean(h_logwage, na.rm = TRUE),
            n_occ = n_distinct(h_OCC2010), .groups = "drop")
fwrite(gd, file.path(TAB_DIR, "18_group_defs.csv"))
message("Group definitions (verify: both ~fully teleworkable, similar wages):")
print(gd %>% mutate(across(where(is.numeric), ~ round(.x, 3))))

# =============================================================================
# 2. PART 0 — first stage: earnings trajectories (ORG subsample)
# =============================================================================

# EARNWEEK2 cleaning (switched from legacy EARNWEEK, which TERMINATES Mar
# 2023 — the original run of this Part had only 5 post months; see
# doc/data_processing_notes.md). EARNWEEK2 covers the full window.
#   NIU sentinel = 999999.99. Topcode regime changes Apr 2023 (fixed $2,884.61
#   -> dynamic monthly top-3% mean), so we CENSOR uniformly at $2,884.61
#   across all months for a comparable series. Medians unaffected unless the
#   median hits the cap (it does not, verified by topcode shares << 50%).
TOPCODE_UNIFORM <- 2884.61
d$h_earn <- ifelse(d$h_ELIGORG == 1 & !is.na(d$h_EARNWEEK2) &
                     d$h_EARNWEEK2 > 0 & d$h_EARNWEEK2 < 999999,
                   pmin(d$h_EARNWEEK2, TOPCODE_UNIFORM), NA_real_)
w_pct <- d$w_genai_total_pctile_s
w_q1  <- quantile(w_pct, .25, na.rm = TRUE, type = 7)
d$w_exposed <- as.integer(w_pct > w_q1)
d$w_earn <- ifelse(d$w_ELIGORG == 1 & !is.na(d$w_EARNWEEK2) &
                     d$w_EARNWEEK2 > 0 & d$w_EARNWEEK2 < 999999,
                   pmin(d$w_EARNWEEK2, TOPCODE_UNIFORM), NA_real_)

topcode <- TOPCODE_UNIFORM
message(sprintf("Uniform censoring point: %.2f (topcode share = at-cap share)", topcode))

wmedian <- function(x, w) {
  ok <- !is.na(x) & !is.na(w); x <- x[ok]; w <- w[ok]
  if (!length(x)) return(NA_real_)
  o <- order(x); x <- x[o]; w <- w[o]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

# Panel A: husbands, AI-core vs comparator
fsA <- d %>%
  filter(!is.na(h_grp), !is.na(h_earn)) %>%
  group_by(grp = h_grp, qtr = rel_qtr) %>%
  summarise(med_earn = wmedian(h_earn, h_EARNWT),
            share_topcode = weighted.mean(h_earn >= topcode * 0.999, h_EARNWT),
            n = n(), .groups = "drop") %>%
  mutate(panel = "A: husbands, AI-core vs comparator")

# Panel B: wives, exposed vs unexposed (old insurance first stage; descriptive)
fsB <- d %>%
  filter(!is.na(d$w_exposed), !is.na(w_earn)) %>%
  group_by(grp = if_else(w_exposed == 1, "exposed", "unexposed"), qtr = rel_qtr) %>%
  summarise(med_earn = wmedian(w_earn, w_EARNWT),
            share_topcode = weighted.mean(w_earn >= topcode * 0.999, w_EARNWT),
            n = n(), .groups = "drop") %>%
  mutate(panel = "B: wives, exposed vs unexposed")

fs <- bind_rows(fsA, fsB) %>%
  group_by(panel, grp) %>%
  mutate(med_base = mean(med_earn[qtr < 0]),
         med_rel = med_earn / med_base) %>%   # indexed to own pre mean
  ungroup()
fwrite(fs, file.path(TAB_DIR, "18_firststage_earnings.csv"))

message("\n=== PART 0: post/pre earnings growth (median, indexed to own pre-mean) ===")
print(fs %>% group_by(panel, grp) %>%
        summarise(pre_med = mean(med_earn[qtr < 0]),
                  post_med = mean(med_earn[qtr >= 0]),
                  growth = post_med / pre_med - 1,
                  topcode_pre = mean(share_topcode[qtr < 0]),
                  topcode_post = mean(share_topcode[qtr >= 0]), .groups = "drop") %>%
        mutate(across(where(is.numeric), ~ round(.x, 3))))

for (pn in unique(fs$panel)) {
  gg <- fs %>% filter(panel == pn) %>%
    pivot_longer(c(med_rel, share_topcode), names_to = "meas", values_to = "val") %>%
    mutate(meas = recode(meas, med_rel = "Median weekly earnings (index, pre = 1)",
                         share_topcode = "Share at topcode"))
  g <- ggplot(gg, aes(qtr, val, colour = grp)) +
    geom_vline(xintercept = -0.5, linetype = "dotted", colour = "firebrick") +
    geom_line() + geom_point(size = .8) +
    facet_wrap(~ meas, scales = "free_y") +
    labs(title = paste("First stage —", pn),
         subtitle = "ORG subsample, EARNWT-weighted. EARNWEEK excludes equity compensation,\nso this measures only the WAGE-visible part of any AI dividend.",
         x = "Quarters relative to ChatGPT (2022Q4 = 0)", y = NULL, colour = NULL) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  ggsave(file.path(FIG_DIR, sprintf("18_firststage_%s.pdf",
                                    substr(pn, 1, 1))), g, width = 10, height = 5)
}

# =============================================================================
