# =============================================================================
# 24_main_regressions.R — MAIN INFERENCE: AI exposure, own & spousal outcomes
# =============================================================================
# Design settled with user 2026-08-24/25; output format settled 2026-08-25:
#   * TABLES formatted like DiFilippo Table A.10 — one spec per table,
#     OUTCOMES as columns; rows = Exposed x Post (b1), Exposed x Interlude
#     (b2), pre-treatment mean, Obs, R2. Basic DiD only (no per-period rows).
#   * FIGURES = dynamic event studies, YEARLY coefficients (ref 2019, the
#     last full pre-COVID year), DiFilippo Fig-III style points + 95% CI.
#   * Series A and B in SEPARATE folders:
#       output/main_regressions/A_own/{tables,figures}
#       output/main_regressions/B_spillover/{tables,figures}
#   * Treatment: BINARY A only (quartile-intensity versions postponed).
#
# SKELETON (adapted from DiFilippo eq.1; During-role transplanted to excise
# COVID from OUR pre):
#   y = b1 (AI x Post) + b2 (AI x Interlude) + X'd + occ FE + time FE [+...]
#   Post = 1{t >= 2022-11}; Interlude = 1{2020-03..2022-10};
#   reference = 2015-01..2020-02 (pre-COVID normal times).
#   b1 = post-ChatGPT gap shift vs the pre-COVID anchor (THE number);
#   b2 = interlude deviation (quarantine + diagnostic).
#
# SERIES A — own exposure -> OWN outcomes (h, w separately).
#   Outcomes: hrs/hrs_main/fulltime + log_earn/log_hrwage (extensive margin
#   mechanically unavailable for own effects — occupation unobserved for
#   NILF; DiF fn.28. Earnings = EARNWEEK2, ORG subsample, EARNWT weights,
#   censored at $2,884.61; log_hrwage = log(earnings/main-job hours)).
#   Ladder: M1 demo+occ+ym; M2 +ind^ym; M3 +state^ym; M4 +occ-wage x periods.
#   Samples: full (M1-M4) + own-tele subsets (M4; within-subset binary A).
#   No tele x period control anywhere: subsetting IS the tele adjustment
#   (within subset the binary is collinear with time FE); actual-rate
#   control dropped per user (post-treatment measure, incomplete).
#
# SERIES B — treated spouse's exposure -> SPOUSE outcomes (both directions).
#   Five labor outcomes + log_earn/log_hrwage (earnings: ORG, EARNWT).
#   Ladder M1-M4 (treated side) + M5 = + outcome-spouse occupation FE:
#   isolates the spillover from CORRELATED DIRECT EFFECTS (assortative
#   matching; DiFilippo A.15). M5 = intensive margins only (conditions on
#   the spouse holding an occupation code); dual reading (response-margin
#   bad control) on record — empirically the two coincided in script 18.
#   Samples: full (M1-M5) + treated-tele subsets (M4).
#
# CLUSTERING: ONE-WAY at the treated spouse's occupation (the treatment
# assignment level; DiFilippo's convention). couple-level correlation is
# subsumed within occupation clusters (couples sit inside their treated
# spouse's occ cluster, absent switching); multiway dropped per user
# 2026-08-26. M5 likewise one-way (spouse occ enters only as FE).
# WEIGHTS: outcome person's WTFINL (EARNWT for earnings outcomes).
# PRE-STATED READING: an AI effect requires b1 to survive full-sample M4 AND
# the tele=1 subset, with a consistent sign in tele=0. Expected: anchored 0.
# =============================================================================

library(tidyverse)
library(fixest)
library(data.table)

ROOT <- path.expand(".")
PROC_DIR <- file.path(ROOT, "data", "processed")
DIR_A_T <- file.path(ROOT, "output", "main_regressions", "A_own", "tables")
DIR_A_F <- file.path(ROOT, "output", "main_regressions", "A_own", "figures")
DIR_B_T <- file.path(ROOT, "output", "main_regressions", "B_spillover", "tables")
DIR_B_F <- file.path(ROOT, "output", "main_regressions", "B_spillover", "figures")
for (p in c(DIR_A_T, DIR_A_F, DIR_B_T, DIR_B_F))
  dir.create(p, recursive = TRUE, showWarnings = FALSE)

WAGE_CSV <- file.path(ROOT, "output", "wage_hetero", "tables", "13_occ_wage.csv")
stopifnot(file.exists(WAGE_CSV))
ES_REF_YEAR <- 2019L

# =============================================================================
# 1. Load & prep
# =============================================================================

d <- fread(file.path(PROC_DIR, "cps_couples.csv")) %>% as_tibble()
d <- d %>% filter((a_SEX == 1 & b_SEX == 2) | (a_SEX == 2 & b_SEX == 1))

d$husb_is_a <- d$a_SEX == 1
hw_vars <- c("genai_total_pctile_s", "wfh", "OCC2010", "IND1990", "AGE",
             "educ_cat", "white_nh", "hispan",
             "inlf", "emp", "hrs", "hrs_main", "fulltime", "WTFINL",
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

# Earnings outcomes (ORG subsample; EARNWT weights). EARNWEEK2 censored
# uniformly at $2,884.61 (fixed topcode through Mar 2023; dynamic after —
# uniform censoring keeps the series comparable; see data notes).
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
d$precovid  <- as.integer(ymnum < 202003)

for (s in c("h", "w")) {
  pct <- d[[paste0(s, "_genai_total_pctile_s")]]
  q1 <- quantile(pct, .25, na.rm = TRUE, type = 7)
  d[[paste0(s, "_expA")]] <- ifelse(is.na(pct), NA_integer_, as.integer(pct > q1))
  tele <- d[[paste0(s, "_tele")]]
  for (tv in 0:1) {
    p <- ifelse(!is.na(tele) & tele == tv, pct, NA_real_)
    qs <- quantile(p, .25, na.rm = TRUE, type = 7)
    d[[sprintf("%s_expA_t%d", s, tv)]] <- ifelse(is.na(p), NA_integer_,
                                                 as.integer(p > qs))
  }
}

CTRL <- paste(
  "h_AGE + I(h_AGE^2) + w_AGE + I(w_AGE^2)",
  "+ factor(h_educ_cat) + factor(w_educ_cat)",
  "+ h_white_nh + w_white_nh + h_hispan + w_hispan",
  "+ n_children")

# --- RESUME GUARD (user 2026-08-25: interrupt-safe, no recomputation) --------
# Every estimated cell is appended to its raw CSV IMMEDIATELY with a spec_tag.
# On restart, cells whose key is already cached under the CURRENT tag are
# skipped. If the specification changes, the old cache is archived *_legacy.
spec_tag_of <- function(...) {
  s <- paste(unlist(list(...)), collapse = "|")
  if (requireNamespace("rlang", quietly = TRUE)) substr(rlang::hash(s), 1, 10)
  else sprintf("%010d", sum(utf8ToInt(s)))
}
SPEC_TAG <- spec_tag_of(CTRL, "post2022-11", "interlude2020-03:2022-10",
                        "binaryA", ES_REF_YEAR, "v3-cluster-occ-oneway")
message("Specification tag: ", SPEC_TAG)

resume_keys <- function(path, keycols, need = NULL) {
  if (!file.exists(path)) return(character(0))
  x <- as.data.frame(fread(path))
  if (!"spec_tag" %in% names(x) || !all(c(keycols, need) %in% names(x))) {
    file.rename(path, sub("\\.csv$", "_legacy.csv", path))
    message("Cache format/spec changed — archived: ", basename(path))
    return(character(0))
  }
  x <- x[x$spec_tag == SPEC_TAG, , drop = FALSE]
  if (!nrow(x)) {
    file.rename(path, sub("\\.csv$", "_legacy.csv", path))
    message("Cache from another spec — archived: ", basename(path))
    return(character(0))
  }
  unique(do.call(paste, c(x[, keycols, drop = FALSE], sep = "|")))
}
append_row <- function(row, path) {
  row$spec_tag <- SPEC_TAG
  fwrite(row, path, append = file.exists(path))
}
load_cache <- function(path) {
  if (!file.exists(path)) return(tibble())
  x <- as_tibble(fread(path))
  x %>% filter(spec_tag == SPEC_TAG)
}

fe_of <- function(m, st) {
  occ <- paste0(st, "_OCC2010"); ind <- paste0(st, "_IND1990")
  switch(m,
    M1 = sprintf("%s + ym", occ),
    M2 = sprintf("%s + %s^ym", occ, ind),
    sprintf("%s + %s^ym + STATEFIP^ym", occ, ind))   # M3, M4, M5
}
rhs_of <- function(m, st, expvar) {
  base <- sprintf("%s:post + %s:interlude + %s", expvar, expvar, CTRL)
  if (m %in% c("M4", "M5"))
    base <- sprintf("%s + %s_logwage:post + %s_logwage:interlude", base, st, st)
  base
}

run_cell <- function(dat, yv, wt, expvar, m, st, extra_fe = NULL, cl) {
  fe <- fe_of(m, st)
  if (!is.null(extra_fe)) fe <- paste(fe, "+", extra_fe)
  mod <- tryCatch(
    feols(as.formula(sprintf("%s ~ %s | %s", yv, rhs_of(m, st, expvar), fe)),
          data = dat, weights = as.formula(paste0("~", wt)),
          cluster = cl, lean = TRUE, mem.clean = TRUE, notes = FALSE),
    error = function(e) { message("  FAILED: ", conditionMessage(e)); NULL })
  if (is.null(mod)) return(NULL)
  pc <- as.data.frame(coeftable(mod))
  g <- function(pat) {
    r <- grep(pat, rownames(pc))[1]
    if (is.na(r)) c(NA_real_, NA_real_) else c(pc[r, 1], pc[r, 2])
  }
  b1 <- g(sprintf("%s:post|post:%s", expvar, expvar))
  b2 <- g(sprintf("%s:interlude|interlude:%s", expvar, expvar))
  # cov(b1, b2): needed for the Wald test of b1 - b2 = 0 (post-ChatGPT vs
  # interlude difference; the DiFilippo-A.16-equivalent row under our
  # non-nested parameterization, where their beta2 = our b1 - b2).
  cov12 <- tryCatch({
    V <- mod$cov.scaled            # clustered vcov computed at estimation;
    if (is.null(V)) V <- vcov(mod) # survives lean = TRUE (k x k, small)
    i1 <- grep(sprintf("%s:post|post:%s", expvar, expvar), rownames(V))[1]
    i2 <- grep(sprintf("%s:interlude|interlude:%s", expvar, expvar),
               rownames(V))[1]
    if (is.na(i1) || is.na(i2)) NA_real_ else V[i1, i2]
  }, error = function(e) NA_real_)
  r2v <- tryCatch(unname(fixest::r2(mod, type = "r2")),
                  error = function(e) NA_real_)
  ok <- dat$precovid == 1 & !is.na(dat[[yv]]) & !is.na(dat[[wt]])
  pm <- weighted.mean(dat[[yv]][ok], dat[[wt]][ok])
  data.frame(b1 = b1[1], b1_se = b1[2], b2 = b2[1], b2_se = b2[2],
             b12_cov = cov12, pre_mean = pm, n_obs = nobs(mod), r2 = r2v)
}

stars <- function(b, se) {
  if (is.na(b) || is.na(se)) return("")
  z <- abs(b / se)
  if (z > 2.576) "***" else if (z > 1.96) "**" else if (z > 1.645) "*" else ""
}

# LADDER comparison table: ONE file per side/direction. Columns = spec x
# sample combos; rows = outcome blocks (b1, se, b2, se, pre-mean, Obs).
# (Per user 2026-08-25: all specs side by side in one table for comparison.)
write_ladder <- function(res, out_csv, outcome_order, col_order) {
  res <- res %>%
    mutate(col = paste(model, sample),
           col = factor(col, levels = col_order)) %>%
    filter(!is.na(col), outcome %in% outcome_order)
  blocks <- lapply(outcome_order, function(oc) {
    r <- res %>% filter(outcome == oc) %>% arrange(col)
    cell <- function(f) sapply(levels(res$col), function(cn) {
      x <- r[as.character(r$col) == cn, ]
      if (nrow(x) == 0) "" else f(x)
    })
    rbind(
      c(paste0(oc, ": Exposed x Post"),
        cell(function(x) sprintf("%.3f%s", x$b1, stars(x$b1, x$b1_se)))),
      c("", cell(function(x) sprintf("(%.3f)", x$b1_se))),
      c(paste0(oc, ": Exposed x Interlude"),
        cell(function(x) sprintf("%.3f%s", x$b2, stars(x$b2, x$b2_se)))),
      c("", cell(function(x) sprintf("(%.3f)", x$b2_se))),
      c(paste0(oc, ": b1 - b2 = 0 (p-val)"),
        cell(function(x) {
          if (!"b12_cov" %in% names(x) || is.na(x$b12_cov)) return("")
          v <- x$b1_se^2 + x$b2_se^2 - 2 * x$b12_cov
          if (!is.finite(v) || v <= 0) return("")
          sprintf("%.3f", 2 * pnorm(-abs((x$b1 - x$b2) / sqrt(v))))
        })),
      c(paste0(oc, ": Mean pre-COVID"),
        cell(function(x) sprintf("%.3f", x$pre_mean))),
      c(paste0(oc, ": Obs"),
        cell(function(x) format(x$n_obs, big.mark = ","))))
  })
  tab <- do.call(rbind, blocks)
  colnames(tab) <- c("", levels(res$col))
  write.csv(tab, out_csv, row.names = FALSE)
}

# =============================================================================
# 2. SERIES A — ladders
# =============================================================================

EARN_OUT <- c("log_earn", "log_hrwage")
INTENSIVE <- c("hrs", "hrs_main", "fulltime", EARN_OUT)
wt_of <- function(side, oc)
  paste0(side, "_", if (oc %in% EARN_OUT) "EARNWT" else "WTFINL")
OUT_A <- c("hrs", "hrs_main", "fulltime", EARN_OUT)
A_CACHE <- file.path(DIR_A_T, "24_A_ladder_raw.csv")
A_KEY <- c("side", "outcome", "model", "sample")
doneA <- resume_keys(A_CACHE, A_KEY, need = "b12_cov")
if (length(doneA)) message(sprintf("A: resuming, %d cells cached", length(doneA)))
for (s in c("h", "w")) {
  cl2 <- as.formula(sprintf("~%s_OCC2010", s))
  for (oc in OUT_A) {
    yv <- paste0(s, "_", oc); wt <- wt_of(s, oc)
    for (sv in c("full", "tele1", "tele0")) {
      dat <- if (sv == "full") d else
        d[!is.na(d[[paste0(s, "_tele")]]) &
          d[[paste0(s, "_tele")]] == as.integer(sub("tele", "", sv)), ]
      ev <- if (sv == "full") paste0(s, "_expA") else
        sprintf("%s_expA_t%s", s, sub("tele", "", sv))
      for (m in c("M1", "M2", "M3", "M4")) {
        key <- paste(s, oc, m, sv, sep = "|")
        if (key %in% doneA) next
        message(sprintf("A | %s | %s | %s | %s", s, oc, m, sv))
        r <- run_cell(dat, yv, wt, ev, m, s, cl = cl2)
        if (!is.null(r)) append_row(
          cbind(data.frame(side = s, outcome = oc, model = m, sample = sv), r),
          A_CACHE)
      }
    }
  }
}
A <- load_cache(A_CACHE)
A_COLS <- c(paste(c("M1", "M2", "M3", "M4"), "full"),
            paste(c("M1", "M2", "M3", "M4"), "tele1"),
            paste(c("M1", "M2", "M3", "M4"), "tele0"))
for (s in c("h", "w"))
  write_ladder(A %>% filter(side == s),
               file.path(DIR_A_T, sprintf("24_A_table_%s.csv", s)),
               OUT_A, A_COLS)

# =============================================================================
# 3. SERIES B — ladders
# =============================================================================

OUT_B <- c("inlf", "emp", "hrs", "hrs_main", "fulltime", EARN_OUT)
DIRS <- list(HtoW = list(st = "h", so = "w"), WtoH = list(st = "w", so = "h"))
B_CACHE <- file.path(DIR_B_T, "24_B_ladder_raw.csv")
B_KEY <- c("direction", "outcome", "model", "sample")
doneB <- resume_keys(B_CACHE, B_KEY, need = "b12_cov")
if (length(doneB)) message(sprintf("B: resuming, %d cells cached", length(doneB)))
for (dn in names(DIRS)) {
  st <- DIRS[[dn]]$st; so <- DIRS[[dn]]$so
  cl2 <- as.formula(sprintf("~%s_OCC2010", st))
    for (oc in OUT_B) {
    yv <- paste0(so, "_", oc); wt <- wt_of(so, oc)
    for (sv in c("full", "tele1", "tele0")) {
      dat <- if (sv == "full") d else
        d[!is.na(d[[paste0(st, "_tele")]]) &
          d[[paste0(st, "_tele")]] == as.integer(sub("tele", "", sv)), ]
      ev <- if (sv == "full") paste0(st, "_expA") else
        sprintf("%s_expA_t%s", st, sub("tele", "", sv))
      specs <- c("M1", "M2", "M3", "M4",
                 if (oc %in% INTENSIVE) "M5")
      for (m in specs) {
        key <- paste(dn, oc, m, sv, sep = "|")
        if (key %in% doneB) next
        message(sprintf("B | %s | %s | %s | %s", dn, oc, m, sv))
        r <- run_cell(dat, yv, wt, ev, m, st,
                      extra_fe = if (m == "M5") paste0(so, "_OCC2010") else NULL,
                      cl = cl2)
        if (!is.null(r)) append_row(
          cbind(data.frame(direction = dn, outcome = oc, model = m,
                           sample = sv), r),
          B_CACHE)
      }
    }
  }
}
B <- load_cache(B_CACHE)
# NOTE: "M5 tele1" = maximal isolation column (tele channel off + correlated
# direct effects absorbed) — but it conditions on BOTH spouses' occupational
# attributes; read with that double-conditioning caveat.
B_COLS <- c(paste(c("M1", "M2", "M3", "M4", "M5"), "full"),
            paste(c("M1", "M2", "M3", "M4", "M5"), "tele1"),
            paste(c("M1", "M2", "M3", "M4", "M5"), "tele0"))
for (dn in names(DIRS))
  write_ladder(B %>% filter(direction == dn),
               file.path(DIR_B_T, sprintf("24_B_table_%s.csv", dn)),
               OUT_B, B_COLS)

# =============================================================================
# 4. DYNAMIC EVENT STUDIES — yearly coefficients, ref 2019 (M2 spec, full)
# =============================================================================
# Yearly (not monthly/quarterly): DiFilippo Fig-III style, readable, and the
# ym FE inside M2 still absorb monthly shocks. 2026 covers Jan-May (noted).

# ES for ALL specs (per user 2026-08-25): overlaid colored series, one
# pointrange set per spec, dodged within year. M4/M5 add occupation-wage x
# YEAR interactions (the wage control in event-study form).
run_es <- function(dat, yv, wt, expvar, m, st, extra_fe = NULL, cl) {
  rhs <- sprintf("i(YEAR, %s, ref = %d) + %s", expvar, ES_REF_YEAR, CTRL)
  if (m %in% c("M4", "M5"))
    rhs <- sprintf("%s + i(YEAR, %s_logwage, ref = %d)", rhs, st, ES_REF_YEAR)
  fe <- fe_of(m, st)
  if (!is.null(extra_fe)) fe <- paste(fe, "+", extra_fe)
  mod <- tryCatch(
    feols(as.formula(sprintf("%s ~ %s | %s", yv, rhs, fe)),
          data = dat, weights = as.formula(paste0("~", wt)),
          cluster = cl, lean = TRUE, mem.clean = TRUE, notes = FALSE),
    error = function(e) { message("  ES FAILED: ", conditionMessage(e)); NULL })
  if (is.null(mod)) return(NULL)
  pc <- as.data.frame(coeftable(mod))
  idx <- grep(sprintf("^YEAR::\\d+:%s$", expvar), rownames(pc))
  data.frame(year = as.integer(sub("^YEAR::(\\d+):.*$", "\\1",
                                   rownames(pc)[idx])),
             est = pc[idx, 1], se = pc[idx, 2])
}

SPEC_PAL <- c(M1 = "#bdd7e7", M2 = "#6baed6", M3 = "#3182bd",
              M4 = "#08519c", M5 = "#e6550d")
plot_es <- function(es, title_str, ylab_str) {
  ref <- es %>% distinct(outcome, model) %>%
    mutate(year = ES_REF_YEAR, est = 0, se = 0)
  es <- bind_rows(es, ref)
  ggplot(es, aes(year, est, colour = model)) +
    geom_hline(yintercept = 0, colour = "grey55") +
    geom_vline(xintercept = 2019.6, linetype = "dashed", colour = "grey40") +
    geom_vline(xintercept = 2022.8, linetype = "dashed", colour = "red3") +
    geom_line(position = position_dodge(width = .65),
              linewidth = .35, alpha = .7) +
    geom_pointrange(aes(ymin = est - 1.96 * se, ymax = est + 1.96 * se),
                    position = position_dodge(width = .65), size = .22) +
    facet_wrap(~ outcome, scales = "free_y", ncol = 2) +
    scale_colour_manual(values = SPEC_PAL) +
    scale_x_continuous(breaks = seq(2015, 2026, 2)) +
    labs(title = title_str,
         subtitle = sprintf("Yearly Exposed x Year coefficients, ref %d, all specifications overlaid.\nM1 demo+occ+ym | M2 +ind x ym | M3 +state x ym | M4 +occ-wage x year | M5 +spouse occ FE (B, intensive outcomes).\nDashed lines: COVID (2020) and ChatGPT (Nov 2022). 2026 = Jan-May only.",
                            ES_REF_YEAR),
         x = NULL, y = ylab_str, colour = NULL) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
}

# A-series ES: M1-M4, samples full / tele1 / tele0; resumable per
# (outcome, model, sample) cell; figures rebuilt from cache at the end
for (s in c("h", "w")) {
  cl2 <- as.formula(sprintf("~%s_OCC2010", s))
  ES_CACHE <- file.path(DIR_A_T, sprintf("24_A_es_%s.csv", s))
  doneE <- resume_keys(ES_CACHE, c("sample", "outcome", "model"))
  if (length(doneE)) message(sprintf("ES A %s: resuming, %d cells", s, length(doneE)))
  for (sv in c("full", "tele1", "tele0")) {
    dat <- if (sv == "full") d else
      d[!is.na(d[[paste0(s, "_tele")]]) &
        d[[paste0(s, "_tele")]] == as.integer(sub("tele", "", sv)), ]
    ev <- if (sv == "full") paste0(s, "_expA") else
      sprintf("%s_expA_t%s", s, sub("tele", "", sv))
    for (oc in OUT_A) for (m in c("M1", "M2", "M3", "M4")) {
      key <- paste(sv, oc, m, sep = "|")
      if (key %in% doneE) next
      message(sprintf("ES A | %s | %s | %s | %s", s, oc, m, sv))
      r <- run_es(dat, paste0(s, "_", oc), wt_of(s, oc), ev, m, s,
                  cl = cl2)
      if (!is.null(r)) {
        r$sample <- sv; r$outcome <- oc; r$model <- m
        append_row(r, ES_CACHE)
      }
    }
  }
  esc <- load_cache(ES_CACHE)
  for (sv in unique(esc$sample)) {
    g <- plot_es(esc %>% filter(sample == sv),
                 sprintf("Own AI exposure and own outcomes — %s [%s]",
                         ifelse(s == "h", "husbands", "wives"), sv),
                 "Exposed - Q1 differential vs 2019")
    ggsave(file.path(DIR_A_F, sprintf("24_A_es_%s_%s.pdf", s, sv)), g,
           width = 10, height = 7)
  }
}

# B-series ES: M1-M4 all outcomes (M5 intensive, full sample only);
# resumable per cell; figures rebuilt from cache
for (dn in names(DIRS)) {
  st <- DIRS[[dn]]$st; so <- DIRS[[dn]]$so
  cl2 <- as.formula(sprintf("~%s_OCC2010", st))
    ES_CACHE <- file.path(DIR_B_T, sprintf("24_B_es_%s.csv", dn))
  doneE <- resume_keys(ES_CACHE, c("sample", "outcome", "model"))
  if (length(doneE)) message(sprintf("ES B %s: resuming, %d cells", dn, length(doneE)))
  for (sv in c("full", "tele1", "tele0")) {
    dat <- if (sv == "full") d else
      d[!is.na(d[[paste0(st, "_tele")]]) &
        d[[paste0(st, "_tele")]] == as.integer(sub("tele", "", sv)), ]
    ev <- if (sv == "full") paste0(st, "_expA") else
      sprintf("%s_expA_t%s", st, sub("tele", "", sv))
    for (oc in OUT_B) {
      specs <- c("M1", "M2", "M3", "M4",
                 if (oc %in% INTENSIVE) "M5")
      for (m in specs) {
        key <- paste(sv, oc, m, sep = "|")
        if (key %in% doneE) next
        message(sprintf("ES B | %s | %s | %s | %s", dn, oc, m, sv))
        r <- run_es(dat, paste0(so, "_", oc), wt_of(so, oc), ev, m, st,
                    extra_fe = if (m == "M5") paste0(so, "_OCC2010") else NULL,
                    cl = cl2)
        if (!is.null(r)) {
          r$sample <- sv; r$outcome <- oc; r$model <- m
          append_row(r, ES_CACHE)
        }
      }
    }
  }
  esc <- load_cache(ES_CACHE)
  for (sv in unique(esc$sample)) {
    g <- plot_es(esc %>% filter(sample == sv),
                 sprintf("Spouse's AI exposure and own outcomes — %s [%s]",
                         ifelse(dn == "HtoW",
                                "husband's exposure, wife's outcomes",
                                "wife's exposure, husband's outcomes"), sv),
                 "Exposed - Q1 differential vs 2019")
    ggsave(file.path(DIR_B_F, sprintf("24_B_es_%s_%s.pdf", dn, sv)), g,
           width = 10, height = 9)
  }
}

message("\n=== 24_main_regressions.R complete ===")
message("A_own/tables:      ladder raw + formatted DiFilippo-style tables + ES csv")
message("A_own/figures:     yearly event studies (h, w)")
message("B_spillover/*:     same for both directions (+ M5 tables)")
message("Reading order: full M4 tables -> tele1/tele0 M4 tables -> ES figures.")
