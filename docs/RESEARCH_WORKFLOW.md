# Research workflow: AI and household labor supply

## Working with the research source

`research/` contains the original research code, including data construction and estimation. `code/` retains the smaller, separately documented portfolio utilities. Run research scripts with **`research/` as the working directory**. Original project-root paths have been changed to `.` in these copies; configure any remaining legacy/external input paths before execution. Do not point the copies at the original research folder if you want to keep its outputs unchanged.

Notebook outputs, execution counts and cell metadata have been cleared. Notebook cell order and analysis specifications are retained; the notebooks contain successive specifications and are not advertised as a clean, unattended pipeline. No respondent-level data or model caches are included. `docs/SOURCE_MANIFEST.json` records original relative filenames, source hashes and adaptations.

## Order of analysis

| Order | File under `research/code/` | Role |
| --- | --- | --- |
| 1 | `01_merge_ai_exposure.R` | Harmonize occupation codes, generative-AI exposure and teleworkability inputs; write occupation exposure lookup |
| 2 | `02_clean_cps.R` | Read IPUMS CPS extract, clean variables, construct spouse links and the repeated-cross-section couples analysis sample |
| 3 | `13_wage_control_heterogeneity.R` | Construct the occupation wage measure (`output/wage_hetero/tables/13_occ_wage.csv`) used by the main specifications; also contains earlier heterogeneity analyses |
| 4 | `24_main_regressions.R` | Main own-outcome and spousal-outcome regressions, model variants M1–M5, tables and event-study figures |
| 5 | `25_survivor_robustness.R` | Occupation/reference-group and teacher sensitivity analyses using main-regression outputs |
| 6 | `26_ai_horserace.R` | Joint AI-exposure/teleworkability comparison specifications |

```sh
cd research
Rscript code/01_merge_ai_exposure.R
Rscript code/02_clean_cps.R
Rscript code/13_wage_control_heterogeneity.R
Rscript code/24_main_regressions.R
Rscript code/25_survivor_robustness.R
Rscript code/26_ai_horserace.R
```

Download and place the raw inputs before running. The cleaning code expects the paired IPUMS extract `cps_00004.xml` and `cps_00004.dat.gz`; inspect the raw-input paths in the script. The current extract spans January 2015–May 2026, despite some older comments mentioning a narrower sample. Use the actual filters in the main regression script to identify the estimation sample.

The September slides' main analysis is script 24; later common-threshold variants are not silently substituted for it. These are repeated cross-sections, not a longitudinal panel of couples. Occupation fixed effects absorb level differences and do not by themselves remove occupation-specific post-period shocks; interpret any stronger shorthand in original code comments accordingly.

## Execution status

The source files have been checked for R syntax. The full CPS cleaning and regressions have not been rerun in this update. Exact package versions and all required extract variables should be verified before a numerical replication. See [CODE_GUIDE.md](CODE_GUIDE.md) for official download locations and exposure-data inputs.

## Software referenced by the source

R packages: `data.table`, `fixest`, `ipumsr`, `readxl`, `tidyverse`.

These are detected imports, not a pinned or tested environment lockfile. The requirements for the smaller `code/` utilities do not necessarily cover the complete research notebooks.
