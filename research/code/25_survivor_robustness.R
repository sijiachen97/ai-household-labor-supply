# =============================================================================
# 25_survivor_robustness.R (v2) — LEAVE-ONE-OUT robustness for the two
# significant hours findings:
#   TARGET 1 (B-WtoH, tele1): wife-exposed -> husband hours/fulltime UP.
#   TARGET 2 (A-own,  tele0): exposed husbands' own hours DOWN (new in v2).
# =============================================================================
# TARGET 1 design (unchanged from v1):
#   Part 1  Composite teacher drop (all teaching occs out of tele1), M1-M5
#           ladder, hrs/hrs_main/fulltime. Reading (A) teacher-reference
#           artifact predicts collapse; (B) added-worker predicts survival.
#   Part 2  One-at-a-time LOO, top-5 REFERENCE occs, M3/M4/M5 (v2: +M5,
#           +fulltime).
#   Part 3  Same on top-5 TREATED occs.
# TARGET 2 design (v2):
#   Part 4  A-series, husbands, tele0 subset (where the own-hours decline
#           lives; post-specific per the b1=b2 test). One-at-a-time LOO on
#           top-5 treated and top-5 reference occupations, hrs/hrs_main,
#           M3/M4. Candidate carriers: long-hours occupations (retail/
#           production supervisors, physicians) -> if one drop kills the
#           effect, the post-COVID-normalization reading gains.
# Binary thresholds are KEPT from the full subset samples (as in 24); only
# group composition changes under drops. Baselines read from 24's caches.
# Cluster one-way on treated occupation; weights = outcome side's WTFINL.
# Output: output/survivor_robustness/{tables,figures}
# =============================================================================

library(tidyverse)
library(fixest)
library(data.table)

ROOT <- path.expand(".")
PROC_DIR <- file.path(ROOT, "data", "processed")
DIR_T <- file.path(ROOT, "output", "survivor_robustness", "tables")
DIR_F <- file.path(ROOT, "output", "survivor_robustness", "figures")
for (p in c(DIR_T, DIR_F)) dir.create(p, recursive = TRUE, showWarnings = FALSE)

WAGE_CSV <- file.path(ROOT, "output", "wage_hetero", "tables", "13_occ_wage.csv")
B24_CACHE <- file.path(ROOT, "output", "main_regressions", "B_spillover",
                       "tables", "24_B_ladder_raw.csv")
A24_CACHE <- file.path(ROOT, "output", "main_regressions", "A_own",
                       "tables", "24_A_ladder_raw.csv")
stopifnot(file.exists(WAGE_CSV), file.exists(B24_CACHE), file.exists(A24_CACHE))

# =============================================================================
# 1. Load & prep (mirrors 24; earnings vars not needed here)
# =============================================================================

d <- fread(file.path(PROC_DIR, "cps_couples.csv")) %>% as_tibble()
d <- d %>% filter((a_SEX == 1 & b_SEX == 2) | (a_SEX == 2 & b_SEX == 1))
d$husb_is_a <- d$a_SEX == 1
hw_vars <- c("genai_total_pctile_s", "wfh", "OCC2010", "IND1990", "AGE",
             "educ_cat", "white_nh", "hispan",
             "hrs", "hrs_main", "fulltime", "WTFINL")
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
ymnum <- d$YEAR * 100 + d$MONTH
d$post      <- as.integer(d$rel_month >= 0)
d$interlude <- as.integer(ymnum >= 202003 & ymnum <= 202210)
d$precovid  <- as.integer(ymnum < 202003)

# Within-subset binaries — SAME construction as 24 (threshold from the FULL
# subset sample; kept fixed under all drops below).
p1  <- ifelse(!is.na(d$w_tele) & d$w_tele == 1, d$w_genai_total_pctile_s, NA_real_)
d$w_expA_t1 <- ifelse(is.na(p1), NA_integer_,
                      as.integer(p1 > quantile(p1, .25, na.rm = TRUE, type = 7)))
p0  <- ifelse(!is.na(d$h_tele) & d$h_tele == 0, d$h_genai_total_pctile_s, NA_real_)
d$h_expA_t0 <- ifelse(is.na(p0), NA_integer_,
                      as.integer(p0 > quantile(p0, .25, na.rm = TRUE, type = 7)))

CTRL <- paste(
  "h_AGE + I(h_AGE^2) + w_AGE + I(w_AGE^2)",
  "+ factor(h_educ_cat) + factor(w_educ_cat)",
  "+ h_white_nh + w_white_nh + h_hispan + w_hispan",
  "+ n_children")

# Analysis samples
dt1  <- d %>% filter(!is.na(w_tele), w_tele == 1, !is.na(w_expA_t1))  # target 1
dt0h <- d %>% filter(!is.na(h_tele), h_tele == 0, !is.na(h_expA_t0))  # target 2

titles <- fread(file.path(PROC_DIR, "occ_ai_exposure.csv")) %>%
  as_tibble() %>% transmute(occ = as.integer(occ2010), occ_title)

# =============================================================================
# 2. Compositions (weighted shares of the treated side's occupation)
# =============================================================================

comp <- function(dat, side, lab) {
  dat %>%
    count(occ = .data[[paste0(side, "_OCC2010")]],
          wt = .data[[paste0(side, "_WTFINL")]], name = "wt") %>%
    mutate(share = wt / sum(wt), group = lab) %>%
    arrange(desc(share)) %>%
    left_join(titles, by = "occ") %>%
    select(group, occ, occ_title, share) %>% slice_head(n = 10)
}
composition <- bind_rows(
  comp(dt1  %>% filter(w_expA_t1 == 0), "w", "B ref (w tele1, exposed=0)"),
  comp(dt1  %>% filter(w_expA_t1 == 1), "w", "B treated (w tele1, exposed=1)"),
  comp(dt0h %>% filter(h_expA_t0 == 0), "h", "A ref (h tele0, exposed=0)"),
  comp(dt0h %>% filter(h_expA_t0 == 1), "h", "A treated (h tele0, exposed=1)"))
write.csv(composition, file.path(DIR_T, "25_composition_top10.csv"),
          row.names = FALSE)
print(composition, n = 40)

top5 <- function(lab) composition %>% filter(group == lab) %>% slice_head(n = 5)
B_REF <- top5("B ref (w tele1, exposed=0)");  B_TRT <- top5("B treated (w tele1, exposed=1)")
A_REF <- top5("A ref (h tele0, exposed=0)");  A_TRT <- top5("A treated (h tele0, exposed=1)")

TEACH <- c(2300, 2310, 2320, 2330, 2340, 2540)
message(sprintf("Teacher share of B reference (weighted): %.1f%%",
  100 * with(dt1 %>% filter(w_expA_t1 == 0),
             weighted.mean(w_OCC2010 %in% TEACH, w_WTFINL))))

# =============================================================================
# 3. Estimation machinery (generalized over treated side st / outcome side so)
# =============================================================================

fe_of <- function(m, st, so) switch(m,
  M1 = sprintf("%s_OCC2010 + ym", st),
  M2 = sprintf("%s_OCC2010 + %s_IND1990^ym", st, st),
  M5 = sprintf("%s_OCC2010 + %s_IND1990^ym + STATEFIP^ym + %s_OCC2010",
               st, st, so),
  sprintf("%s_OCC2010 + %s_IND1990^ym + STATEFIP^ym", st, st))   # M3, M4
rhs_of <- function(m, st, ev) {
  base <- sprintf("%s:post + %s:interlude + %s", ev, ev, CTRL)
  if (m %in% c("M4", "M5"))
    base <- sprintf("%s + %s_logwage:post + %s_logwage:interlude", base, st, st)
  base
}
run_cell <- function(dat, yv, m, st, so, ev, wt) {
  mod <- tryCatch(
    feols(as.formula(sprintf("%s ~ %s | %s", yv, rhs_of(m, st, ev),
                             fe_of(m, st, so))),
          data = dat, weights = as.formula(paste0("~", wt)),
          cluster = as.formula(sprintf("~%s_OCC2010", st)),
          lean = TRUE, mem.clean = TRUE, notes = FALSE),
    error = function(e) { message("  FAILED: ", conditionMessage(e)); NULL })
  if (is.null(mod)) return(NULL)
  pc <- as.data.frame(coeftable(mod))
  g <- function(pat) {
    r <- grep(pat, rownames(pc))[1]
    if (is.na(r)) c(NA_real_, NA_real_) else c(pc[r, 1], pc[r, 2])
  }
  b1 <- g(sprintf("%s:post|post:%s", ev, ev))
  b2 <- g(sprintf("%s:interlude|interlude:%s", ev, ev))
  cov12 <- tryCatch({
    V <- mod$cov.scaled; if (is.null(V)) V <- vcov(mod)
    i1 <- grep(sprintf("%s:post|post:%s", ev, ev), rownames(V))[1]
    i2 <- grep(sprintf("%s:interlude|interlude:%s", ev, ev), rownames(V))[1]
    if (is.na(i1) || is.na(i2)) NA_real_ else V[i1, i2]
  }, error = function(e) NA_real_)
  data.frame(b1 = b1[1], b1_se = b1[2], b2 = b2[1], b2_se = b2[2],
             b12_cov = cov12, n_obs = nobs(mod))
}
p_diff <- function(x) {
  v <- x$b1_se^2 + x$b2_se^2 - 2 * x$b12_cov
  ifelse(is.finite(v) & v > 0, 2 * pnorm(-abs((x$b1 - x$b2) / sqrt(v))),
         NA_real_)
}
stars <- function(b, se) {
  z <- abs(b / se)
  ifelse(z > 2.576, "***", ifelse(z > 1.96, "**", ifelse(z > 1.645, "*", "")))
}

# Resume cache (v1-compatible: same spec tag, so v1 cells are reused; the new
# M5 / fulltime / Part-4 cells have new keys and are the only ones estimated)
spec_tag_of <- function(...) {
  s <- paste(unlist(list(...)), collapse = "|")
  if (requireNamespace("rlang", quietly = TRUE)) substr(rlang::hash(s), 1, 10)
  else sprintf("%010d", sum(utf8ToInt(s)))
}
SPEC_TAG <- spec_tag_of(CTRL, "WtoH-tele1-loo", "binaryA-fixed-threshold",
                        "v1-cluster-occ-oneway")
CACHE <- file.path(DIR_T, "25_loo_raw.csv")
KEY <- c("part", "drop", "outcome", "model")
done <- if (file.exists(CACHE)) {
  x <- as.data.frame(fread(CACHE))
  if (!"spec_tag" %in% names(x) || !all(c(KEY, "b12_cov") %in% names(x))) {
    file.rename(CACHE, sub("\\.csv$", "_legacy.csv", CACHE)); character(0)
  } else {
    x <- x[x$spec_tag == SPEC_TAG, , drop = FALSE]
    if (!nrow(x)) { file.rename(CACHE, sub("\\.csv$", "_legacy.csv", CACHE))
                    character(0) }
    else unique(do.call(paste, c(x[, KEY, drop = FALSE], sep = "|")))
  }
} else character(0)
if (length(done)) message(sprintf("Resuming: %d cells cached", length(done)))
add <- function(part, drop, oc, m, r) {
  row <- cbind(data.frame(part = part, drop = drop, outcome = oc, model = m), r)
  row$spec_tag <- SPEC_TAG
  fwrite(row, CACHE, append = file.exists(CACHE))
}

# --- build run list ----------------------------------------------------------
# each: part, drop label, occs to drop, outcome, model, and series config
B_CFG <- list(dat = "dt1",  st = "w", so = "h", ev = "w_expA_t1", wt = "h_WTFINL",
              yside = "h")
A_CFG <- list(dat = "dt0h", st = "h", so = "w", ev = "h_expA_t0", wt = "h_WTFINL",
              yside = "h")
runs <- list()
rq <- function(part, drop, occs, oc, m, cfg)
  runs[[length(runs) + 1]] <<- c(list(part = part, drop = drop, occs = occs,
                                      oc = oc, m = m), cfg)
# Part 1: composite teacher drop (target 1), full ladder
for (oc in c("hrs", "hrs_main", "fulltime")) for (m in paste0("M", 1:5))
  rq("teacher_drop", "teachers", TEACH, oc, m, B_CFG)
# Parts 2-3: B-side LOO, M3/M4/M5, hrs + hrs_main + fulltime
for (i in seq_len(nrow(B_REF))) for (oc in c("hrs", "hrs_main", "fulltime"))
  for (m in c("M3", "M4", "M5"))
    rq("loo_reference", as.character(B_REF$occ[i]), B_REF$occ[i], oc, m, B_CFG)
for (i in seq_len(nrow(B_TRT))) for (oc in c("hrs", "hrs_main", "fulltime"))
  for (m in c("M3", "M4", "M5"))
    rq("loo_treated", as.character(B_TRT$occ[i]), B_TRT$occ[i], oc, m, B_CFG)
# Part 4: A-side LOO (husband own, tele0), M3/M4, hrs + hrs_main
for (i in seq_len(nrow(A_REF))) for (oc in c("hrs", "hrs_main"))
  for (m in c("M3", "M4"))
    rq("A_loo_reference", as.character(A_REF$occ[i]), A_REF$occ[i], oc, m, A_CFG)
for (i in seq_len(nrow(A_TRT))) for (oc in c("hrs", "hrs_main"))
  for (m in c("M3", "M4"))
    rq("A_loo_treated", as.character(A_TRT$occ[i]), A_TRT$occ[i], oc, m, A_CFG)

for (r in runs) {
  key <- paste(r$part, r$drop, r$oc, r$m, sep = "|")
  if (key %in% done) next
  message(sprintf("%s | drop %s | %s | %s", r$part, r$drop, r$oc, r$m))
  base_dat <- get(r$dat)
  dat <- base_dat[!(base_dat[[paste0(r$st, "_OCC2010")]] %in% r$occs), ]
  res <- run_cell(dat, paste0(r$yside, "_", r$oc), r$m, r$st, r$so, r$ev, r$wt)
  if (!is.null(res)) add(r$part, r$drop, r$oc, r$m, res)
}

# =============================================================================
# 4. Baselines from 24's caches + formatted tables
# =============================================================================

baseB <- as_tibble(fread(B24_CACHE)) %>%
  filter(direction == "WtoH", sample == "tele1",
         outcome %in% c("hrs", "hrs_main", "fulltime")) %>%
  transmute(part = "baseline", drop = "none (baseline, from 24)",
            outcome, model, b1, b1_se, b2, b2_se, b12_cov, n_obs)
baseA <- as_tibble(fread(A24_CACHE)) %>%
  filter(side == "h", sample == "tele0",
         outcome %in% c("hrs", "hrs_main")) %>%
  transmute(part = "A_baseline", drop = "none (baseline, from 24)",
            outcome, model, b1, b1_se, b2, b2_se, b12_cov, n_obs)

loo <- as_tibble(fread(CACHE)) %>% filter(spec_tag == SPEC_TAG) %>%
  select(part, drop, outcome, model, b1, b1_se, b2, b2_se, b12_cov, n_obs)
all_res <- bind_rows(baseB, baseA, loo)
all_res$p_b1b2 <- p_diff(all_res)
all_res <- all_res %>%
  left_join(titles %>% mutate(drop = as.character(occ)) %>%
              select(drop, occ_title), by = "drop") %>%
  mutate(label = case_when(
    part %in% c("baseline", "A_baseline") ~ "Baseline (no drop)",
    part == "teacher_drop"                ~ "Drop ALL teaching occs",
    TRUE ~ sprintf("Drop %s %s", drop, coalesce(occ_title, ""))))

fmt <- all_res %>%
  transmute(part, label, outcome, model,
            b1 = sprintf("%.3f%s (%.3f)", b1, stars(b1, b1_se), b1_se),
            b2 = sprintf("%.3f%s (%.3f)", b2, stars(b2, b2_se), b2_se),
            `p(b1=b2)` = sprintf("%.3f", p_b1b2),
            Obs = format(n_obs, big.mark = ","))
wcsv <- function(x, f) write.csv(x, file.path(DIR_T, f), row.names = FALSE)
wcsv(fmt %>% filter(part %in% c("baseline", "teacher_drop")) %>%
       arrange(outcome, model, part), "25_teacher_drop_ladder.csv")
wcsv(fmt %>% filter(part %in% c("baseline", "loo_reference")) %>%
       arrange(outcome, model, part, label), "25_loo_reference.csv")
wcsv(fmt %>% filter(part %in% c("baseline", "loo_treated")) %>%
       arrange(outcome, model, part, label), "25_loo_treated.csv")
wcsv(fmt %>% filter(part %in% c("A_baseline", "A_loo_reference",
                                "A_loo_treated")) %>%
       arrange(outcome, model, part, label), "25_loo_A_htele0.csv")

# =============================================================================
# 5. Coefficient plots
# =============================================================================

loo_plot <- function(dat, ocs, mods, fname, title_str) {
  pd <- dat %>%
    filter(outcome %in% ocs, model %in% mods) %>%
    mutate(label = str_trunc(label, 45),
           label = fct_rev(fct_inorder(label)),
           grp = case_when(grepl("baseline", part) ~ "Baseline",
                           part == "teacher_drop" ~ "Teacher drop",
                           grepl("reference", part) ~ "LOO: reference occ",
                           TRUE ~ "LOO: treated occ"))
  g <- ggplot(pd, aes(b1, label, colour = grp)) +
    geom_vline(xintercept = 0, colour = "grey55") +
    geom_pointrange(aes(xmin = b1 - 1.96 * b1_se, xmax = b1 + 1.96 * b1_se),
                    size = .3) +
    facet_grid(outcome ~ model, scales = "free_x") +
    scale_colour_manual(values = c(Baseline = "black",
                                   `Teacher drop` = "#d62728",
                                   `LOO: reference occ` = "#1f77b4",
                                   `LOO: treated occ` = "#2ca02c")) +
    labs(title = title_str,
         subtitle = "Exposed x Post (b1) with 95% CI. Within-subset binary threshold fixed under all drops.",
         x = "b1 (Exposed x Post)", y = NULL, colour = NULL) +
    theme_minimal(base_size = 10) +
    theme(plot.title = element_text(face = "bold"), legend.position = "bottom")
  ggsave(file.path(DIR_F, fname), g,
         width = 10 + 1.5 * (length(mods) > 2),
         height = if (length(ocs) > 1) 7 else 4.5)
}
Bres <- all_res %>% filter(part %in% c("baseline", "teacher_drop",
                                       "loo_reference", "loo_treated"))
Ares <- all_res %>% filter(grepl("^A_", part))
loo_plot(Bres, c("hrs", "hrs_main"), c("M3", "M4", "M5"), "25_loo_coefplot.pdf",
         "Result 3 under leave-one-out: B-WtoH, tele1, husbands' hours")
loo_plot(Bres, "fulltime", c("M3", "M4", "M5"), "25_loo_coefplot_fulltime.pdf",
         "Leave-one-out: B-WtoH, tele1, husbands' full-time share")
loo_plot(Ares, c("hrs", "hrs_main"), c("M3", "M4"), "25_loo_A_htele0.pdf",
         "Result 1 under leave-one-out: A-own, tele0, husbands' hours")

message("\n=== 25_survivor_robustness.R (v2) complete ===")
message("tables/: 25_composition_top10, 25_teacher_drop_ladder, ",
        "25_loo_reference, 25_loo_treated, 25_loo_A_htele0 (+ raw cache)")
message("figures/: 25_loo_coefplot(.pdf/_fulltime.pdf), 25_loo_A_htele0.pdf")
