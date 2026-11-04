# Occupational AI exposure and household labor supply

## Source data: where to download

Place the following source files in `RAW_DIR`. Links checked November 4, 2026; retain original filenames and record download dates and release/commit identifiers.

| Expected input | Source and instructions |
| --- | --- |
| `genaiexp_estz_occscores.csv` | [Authors' Generative AI and Firm Values data site](https://artificalminushuman.com/), Occupation-Level Generative AI Exposure → Download CSV. Use SOC2010 scores, not firm exposure. |
| `2010-occ-codes-with-crosswalk-from-2002-2011.xls` | [Census occupation crosswalk directory](https://www2.census.gov/programs-surveys/demo/guidance/industry-occupation/). Select this exact workbook; the code reads `2010summ` with two header rows skipped. |
| `onet_teleworkable_blscodes.csv` | [Dingel–Neiman authors' repository](https://github.com/jdingel/DingelNeiman-workathome), `onet_to_BLS_crosswalk/output/onet_teleworkable_blscodes.csv`. Use the raw file download. |
| `AIOE_DataAppendix.xlsx` | [Felten–Raj–Seamans AIOE repository](https://github.com/AIOE-Data/AIOE). Download the workbook; the code reads `Appendix A`. |
| `national_M2018_dl.xlsx` | [BLS May 2018 national OES archive](https://www.bls.gov/oes/special.requests/oesm18nat.zip). Extract the national workbook. This supplies employment weights. |

For the full household analysis, use [IPUMS CPS](https://cps.ipums.org/cps/) to register/sign in and request basic monthly samples from January 2015 through May 2026, matching the supplied slides. Download the extract with its codebook and import syntax. The selected occupation-score script does **not** read CPS microdata; CPS preparation and estimation are separate stages not yet included here. The exact variable manifest should be exported from the original project before providing a reproducible extract recipe.

## Included research code

The repository now includes data-construction and main-analysis source in `research/`, in addition to the focused utilities described below. See [Research workflow](RESEARCH_WORKFLOW.md) for the file-by-file roles, execution order, and unresolved input handoffs. The earlier selection-only completeness inventory has been superseded by that workflow; full numerical replication has not been certified.

## Featured code: occupation harmonization

`build_occupation_exposure.R` creates an OCC2010-keyed exposure file for merging into CPS records. It normalizes SOC codes, handles exact, grouped and wildcard crosswalk entries, integrates generative AI exposure and teleworkability, and exports coverage diagnostics. Where available, BLS employment counts weight aggregation across occupations; the script includes an equal-weight fallback when that input is absent.

## Run

```r
install.packages(c("tidyverse", "readxl"))
```

```sh
PROJECT_ROOT=/path/to/project RAW_DIR=/path/to/source-files Rscript code/build_occupation_exposure.R
```

Required source filenames (not distributed here):

| Input | Purpose |
| --- | --- |
| `2010-occ-codes-with-crosswalk-from-2002-2011.xls` | Census OCC2010–SOC crosswalk, `2010summ` sheet |
| `genaiexp_estz_occscores.csv` | Eisfeldt–Schubert–Taska–Zhang generative AI exposure |
| `onet_teleworkable_blscodes.csv` | Occupational teleworkability |
| `AIOE_DataAppendix.xlsx` | Alternative AI occupational exposure, Appendix A |
| `national_M2018_dl.xlsx` | BLS employment weights; recommended |

Outputs are written to `data/processed/` and `output/data_prep/tables/` below `PROJECT_ROOT`. Review match coverage and unmatched codes before merging scores into individual data. The research's household regressions and CPS microdata are not included in this selected sample.
