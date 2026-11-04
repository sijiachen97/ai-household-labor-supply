# Occupation wage input used by main models; extracted from script 13.
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

