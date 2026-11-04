# =============================================================================
# 26_ai_horserace.R — the HORSE-RACE identification of the AI effect:
# full-sample two-period skeleton with AI x period AND tele x period entered
# JOINTLY. The alternative to tele-subsetting (script 24): instead of switching
# the tele channel off by sample restriction, let the two exposures compete
# and read the AI coefficient net of the tele-period shift.
# =============================================================================
#   y = b1 AI x Post + b2 AI x Interlude + c1 tele x Post + c2 tele x Interlude
#       + X'd + occ FE + [ladder FE]           (tele main effect absorbed by occ FE;
#                                               AI main effect likewise)
# Compared against script 24's full-sample estimates WITHOUT tele controls
# (pulled from 24's caches — not re-estimated).
# Outcomes = the three findings' cells:
#   A-h own : hrs, hrs_main, log_earn, log_hrwage
#   A-w own : log_earn, log_hrwage
#   B-WtoH  : h_hrs, h_hrs_main, h_fulltime
# Specs: M3 and M4. Binary A = full-sample threshold (as in 24 full columns).
# Tele = treated side's D&N binary. Cluster: treated occ. Weights: outcome side.
# Output: output/ai_horserace/{tables,figures}/26_*
# =============================================================================

library(tidyverse)
library(fixest)
library(data.table)

ROOT <- path.expand(".")
PROC_DIR <- file.path(ROOT, "data", "processed")
DIR_T <- file.path(ROOT, "output", "ai_horserace", "tables")
DIR_F <- file.path(ROOT, "output", "ai_horserace", "figures")
for (p in c(DIR_T, DIR_F)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

WAGE_CSV <- file.path(ROOT, "output", "wage_hetero", "tables", "13_occ_wage.csv")
B24 <- file.path(ROOT, "output", "main_regressions", "B_spillover", "tables",
                 "24_B_ladder_raw.csv")
A24 <- file.path(ROOT, "output", "main_regressions", "A_own", "tables",
                 "24_A_ladder_raw.csv")
stopifnot(file.exists(WAGE_CSV), file.exists(A24), file.exists(B24))

# --- data prep (mirrors 24, incl. earnings construction) ---------------------
d <- fread(file.path(PROC_DIR, "cps_couples.csv")) %>% as_tibble()
d <- d %>% filter((a_SEX == 1 & b_SEX == 2) | (a_SEX == 2 & b_SEX == 1))
d$husb_is_a <- d$a_SEX == 1
hw_vars <- c("genai_total_pctile_s", "wfh", "OCC2010", "IND1990", "AGE",
             "educ_cat", "white_nh", "hispan",
             "hrs", "hrs_main", "fulltime", "WTFINL",
             "EARNWEEK2", "ELIGORG", "EARNWT")
for (v in hw_vars) {
  av <- d[[paste0("a_", v)]]; bv <- d[[paste0("b_", v)]]
  d[[paste0("h_", v)]] <- ifelse(d$husb_is_a, av, bv)
  d[[paste0("w_", v)]] <- ifelse(d$husb_is_a, bv, av)
}
wage <- fread(WAGE_CSV) %>% as_tibble() %>% select(occ2010, log_wage)
for (s in c("h", "w")) {
  d <- d %>% left_join(wage %>% rename(!!paste0(s, "_logwage") := log_wage),
                       by = setNames("occ2010", paste0(s, "_OCC2010")))
  d[[paste0(s, "_tele")]] <- ifelse(is.na(d[[paste0(s, "_wfh")]]), NA_integer_,
                                    as.integer(d[[paste0(s, "_wfh")]] > 0.5))
}
for (s in c("h", "w")) {
  e2 <- d[[paste0(s, "_EARNWEEK2")]]
  ok <- d[[paste0(s, "_ELIGORG")]] == 1 & !is.na(e2) & e2 > 0 & e2 < 999999
  earn <- ifelse(ok, pmin(e2, 2884.61), NA_real_)
  d[[paste0(s, "_log_earn")]] <- log(earn)
  hm <- d[[paste0(s, "_hrs_main")]]
  d[[paste0(s, "_log_hrwage")]] <- ifelse(!is.na(earn) & !is.na(hm) & hm > 0,
                                          log(earn / hm), NA_real_)
}
ymnum <- d$YEAR * 100 + d$MONTH
d$post      <- as.integer(d$rel_month >= 0)
d$interlude <- as.integer(ymnum >= 202003 & ymnum <= 202210)
for (s in c("h", "w")) {
  pct <- d[[paste0(s, "_genai_total_pctile_s")]]
  q1 <- quantile(pct, .25, na.rm = TRUE, type = 7)
  d[[paste0(s, "_expA")]] <- ifelse(is.na(pct), NA_integer_, as.integer(pct > q1))
}
CTRL <- paste(
  "h_AGE + I(h_AGE^2) + w_AGE + I(w_AGE^2)",
  "+ factor(h_educ_cat) + factor(w_educ_cat)",
  "+ h_white_nh + w_white_nh + h_hispan + w_hispan",
  "+ n_children")
EARN_OUT <- c("log_earn", "log_hrwage")

# --- cells -------------------------------------------------------------------
CELLS <- tribble(
  ~series, ~st, ~yside, ~oc,
  "A_h",  "h", "h", "hrs",
  "A_h",  "h", "h", "hrs_main",
  "A_h",  "h", "h", "log_earn",
  "A_h",  "h", "h", "log_hrwage",
  "A_w",  "w", "w", "log_earn",
  "A_w",  "w", "w", "log_hrwage",
  "WtoH", "w", "h", "hrs",
  "WtoH", "w", "h", "hrs_main",
  "WtoH", "w", "h", "fulltime")

fe_of <- function(m, st) if (m == "M3" || m == "M4")
  sprintf("%s_OCC2010 + %s_IND1990^ym + STATEFIP^ym", st, st) else
  stop("M3/M4 only")
run_hr <- function(row, m) {
  st <- row$st; ev <- paste0(st, "_expA"); tl <- paste0(st, "_tele")
  yv <- paste0(row$yside, "_", row$oc)
  wt <- paste0(row$yside, "_", if (row$oc %in% EARN_OUT) "EARNWT" else "WTFINL")
  rhs <- sprintf("%s:post + %s:interlude + %s:post + %s:interlude + %s",
                 ev, ev, tl, tl, CTRL)
  if (m == "M4")
    rhs <- sprintf("%s + %s_logwage:post + %s_logwage:interlude", rhs, st, st)
  dat <- d[!is.na(d[[tl]]) & !is.na(d[[ev]]), ]
  mod <- feols(as.formula(sprintf("%s ~ %s | %s", yv, rhs, fe_of(m, st))),
               data = dat, weights = as.formula(paste0("~", wt)),
               cluster = as.formula(sprintf("~%s_OCC2010", st)),
               lean = TRUE, mem.clean = TRUE, notes = FALSE)
  pc <- as.data.frame(coeftable(mod))
  g <- function(v, per) {
    i <- grep(sprintf("%s:%s|%s:%s", v, per, per, v), rownames(pc))[1]
    if (is.na(i)) c(NA, NA) else c(pc[i, 1], pc[i, 2])
  }
  a1 <- g(ev, "post"); a2 <- g(ev, "interlude")
  t1 <- g(tl, "post"); t2 <- g(tl, "interlude")
  data.frame(series = row$series, outcome = row$oc, model = m,
             ai_b1 = a1[1],  ai_b1_se = a1[2],
             ai_b2 = a2[1],  ai_b2_se = a2[2],
             tele_b1 = t1[1], tele_b1_se = t1[2],
             tele_b2 = t2[1], tele_b2_se = t2[2],
             n_obs = nobs(mod))
}

OUT <- file.path(DIR_T, "26_horserace_raw.csv")
done <- if (file.exists(OUT)) {
  x <- fread(OUT); unique(paste(x$series, x$outcome, x$model, sep = "|"))
} else character(0)
for (i in seq_len(nrow(CELLS))) for (m in c("M3", "M4")) {
  key <- paste(CELLS$series[i], CELLS$oc[i], m, sep = "|")
  if (key %in% done) next
  message("HR | ", key)
  r <- tryCatch(run_hr(CELLS[i, ], m),
                error = function(e) { message("  FAILED: ",
                                              conditionMessage(e)); NULL })
  if (!is.null(r)) fwrite(r, OUT, append = file.exists(OUT))
}
hr <- as_tibble(fread(OUT))

# --- baselines (no tele controls) from 24's caches ---------------------------
bA <- as_tibble(fread(A24)) %>% filter(sample == "full", model %in% c("M3", "M4")) %>%
  transmute(series = paste0("A_", side), outcome, model,
            base_b1 = b1, base_b1_se = b1_se)
bB <- as_tibble(fread(B24)) %>% filter(direction == "WtoH", sample == "full",
                                       model %in% c("M3", "M4")) %>%
  transmute(series = "WtoH", outcome, model, base_b1 = b1, base_b1_se = b1_se)
cmp <- hr %>% left_join(bind_rows(bA, bB), by = c("series", "outcome", "model"))

stars <- function(b, se) { z <- abs(b / se)
  ifelse(z > 2.576, "***", ifelse(z > 1.96, "**", ifelse(z > 1.645, "*", ""))) }
fmt <- cmp %>% transmute(
  series, outcome, model,
  `AI x Post (no tele ctrl, 24)` = sprintf("%.3f%s (%.3f)", base_b1,
                                           stars(base_b1, base_b1_se), base_b1_se),
  `AI x Post (horse race)` = sprintf("%.3f%s (%.3f)", ai_b1,
                                     stars(ai_b1, ai_b1_se), ai_b1_se),
  `Tele x Post (horse race)` = sprintf("%.3f%s (%.3f)", tele_b1,
                                       stars(tele_b1, tele_b1_se), tele_b1_se),
  `AI x Interlude` = sprintf("%.3f%s (%.3f)", ai_b2, stars(ai_b2, ai_b2_se),
                             ai_b2_se),
  Obs = format(n_obs, big.mark = ","))
write.csv(fmt %>% arrange(series, outcome, model),
          file.path(DIR_T, "26_horserace.csv"), row.names = FALSE)

# --- coefplot: AI x Post with vs without tele controls -----------------------
pd <- cmp %>%
  pivot_longer(c(base_b1, ai_b1), names_to = "variant", values_to = "b") %>%
  mutate(se = ifelse(variant == "base_b1", base_b1_se, ai_b1_se),
         variant = ifelse(variant == "base_b1", "No tele controls (24 full)",
                          "Horse race (+ tele x period)"),
         lab = paste(series, outcome))
g <- ggplot(pd, aes(b, lab, colour = variant)) +
  geom_vline(xintercept = 0, colour = "grey55") +
  geom_pointrange(aes(xmin = b - 1.96 * se, xmax = b + 1.96 * se),
                  position = position_dodge(width = .5), size = .3) +
  facet_wrap(~model) +
  scale_colour_manual(values = c(`No tele controls (24 full)` = "grey40",
                                 `Horse race (+ tele x period)` = "#d95f02")) +
  labs(title = "AI x Post, full sample: subset-free identification via the tele/AI horse race",
       subtitle = "Treated side's tele x Post + tele x Interlude added; tele and AI main effects absorbed by occupation FE.\nEarnings outcomes in log points; hours in hrs/week; fulltime in shares.",
       x = "AI x Post (b1)", y = NULL, colour = NULL) +
  theme_minimal(base_size = 10) +
  theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
ggsave(file.path(DIR_F, "26_horserace.pdf"), g, width = 10, height = 6)

message("=== 26_ai_horserace.R complete: tables/26_horserace.csv, figures/26_horserace.pdf ===")
