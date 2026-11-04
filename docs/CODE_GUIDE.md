# AI and household labor supply: code guide

All executable research code is under `code/`. `code/data_process/` constructs inputs; `code/analysis/` estimates models and produces tables/figures. Run commands from the repository root. The original research folders are unchanged.

The code is selected against the supplied final paper/slides and their production dependencies, rather than copied as an archive of every experiment. [Source manifest](SOURCE_MANIFEST.json) records original filenames, selected notebook cells, hashes and adaptations. [Source review](SOURCE_REVIEW.json) lists the reviewed source inventory and inclusion/exclusion decisions. No microdata, model caches or saved notebook outputs are shared.

## Final slides and run order

Reference: `doc/presentation_advisor/advisor_slides.tex` and the supplied `AI_labor_slides.pdf`. The slide source directly includes figures from scripts 24, 25, 26 and the first-stage section of 18. The later common-Q3 scripts 29–34 are excluded because they do not generate this version of the slides.

| Order | Included file | Role / final output |
| --- | --- | --- |
| 1 | `code/data_process/01_merge_ai_exposure.R` | Occupation crosswalk, exposure and teleworkability lookup |
| 2 | `code/data_process/02_clean_cps.R` | CPS cleaning, spouse links and couples sample |
| 3 | `code/data_process/03_build_occupation_wages.R` | Production wage-lookup block extracted from original script 13; excludes its earlier exploratory models |
| 4 | `code/analysis/24_main_regressions.R` | Main own/spouse results, M1–M5 and event studies, including `24_B_es_WtoH_tele1.pdf` |
| 5 | `code/analysis/25_survivor_robustness.R` | Leave-one-occupation-out and teacher exclusions used in the slides |
| 6 | `code/analysis/26_ai_horserace.R` | Teleworkability/AI joint comparison, `26_horserace.pdf` |
| 7 | `code/analysis/18_earnings_first_stage.R` | Original first-stage section generating the slides' backup `18_firststage_A.pdf`; later exploratory models omitted |

After preparing the raw inputs:

```sh
bash code/run_analysis.sh
```

The full analysis uses repeated cross-sections of married couples, not a longitudinal household panel. Raw inputs belong in `data/raw/`, processed data in `data/processed/`, and results in `output/`. The code expects the paired IPUMS files `cps_00004.xml` and `cps_00004.dat.gz`. If your account gives a different extract number, update both paths together.

## CPS extract recipe

Request basic monthly samples for January 2015–May 2026 from IPUMS CPS. Use the variable names listed below, taken from `KEEP_VARS` in the original cleaning script. Download the DDI XML and corresponding data file together. Some variables are available only for subsets of months or respondents; the code handles the intended universes.

`YEAR`, `MONTH`, `SERIAL`, `CPSID`, `CPSIDP`, `CPSIDV`, `PERNUM`, `SPLOC`, `SPRULE`, `MISH`, `ASECFLAG`, `EMPSTAT`, `LABFORCE`, `OCC2010`, `IND1990`, `CLASSWKR`, `UHRSWORKT`, `UHRSWORK1`, `WKSTAT`, `DURUNEMP`, `TELWRKPAY`, `TELWRKHR`, `TELWRKBFCVD`, `TELWRKDIFFCVD`, `QTELWRKPAY`, `QTELWRKHR`, `QTELWRKBFCVD`, `QTELWRKDIFFCVD`, `EARNWEEK`, `EARNWEEK2`, `HOURWAGE2`, `PAIDHOUR`, `ELIGORG`, `AGE`, `SEX`, `RACE`, `HISPAN`, `EDUC`, `MARST`, `NCHILD`, `NCHLT5`, `RELATE`, `STATEFIP`, `METAREA`, `CBSASZ`, `COUNTY`, `WTFINL`, `EARNWT`

Source: [IPUMS CPS](https://cps.ipums.org/cps/). Registration and an authorized extract are required.

The main scripts contain the original threshold and sample definitions. A newer exposure threshold or extraction vintage can change estimates and should not be silently substituted. All scripts were syntax-checked, but full CPS cleaning and regression estimation were not rerun here.

## Source data: where to download

| Expected input | Source and instructions |
| --- | --- |
| `genaiexp_estz_occscores.csv` | [Authors' Generative AI and Firm Values data site](https://artificalminushuman.com/), Occupation-Level Generative AI Exposure → Download CSV. Use SOC2010 scores, not firm exposure. |
| `2010-occ-codes-with-crosswalk-from-2002-2011.xls` | [Census occupation crosswalk directory](https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/). Select this exact workbook; the code reads `2010summ` with two header rows skipped. |
| `onet_teleworkable_blscodes.csv` | [Dingel–Neiman authors' repository](https://github.com/jdingel/DingelNeiman-workathome), `onet_to_BLS_crosswalk/output/onet_teleworkable_blscodes.csv`. Use the raw file download. |
| `AIOE_DataAppendix.xlsx` | [Felten–Raj–Seamans AIOE repository](https://github.com/AIOE-Data/AIOE). Download the workbook; the code reads `Appendix A`. |
| `national_M2018_dl.xlsx` | [BLS May 2018 national OES archive](https://www.bls.gov/oes/special.requests/oesm18nat.zip). Extract the national workbook. This supplies employment weights. |

## Software

R packages referenced: `data.table`, `fixest`, `ipumsr`, `readxl`, `tidyverse`.

Dependencies are not a fully pinned environment. See [Validation](VALIDATION.md) for what was and was not executed.
