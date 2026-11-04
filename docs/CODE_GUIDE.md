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

## Completeness review

| Stage | Included? | What is still needed |
| --- | --- | --- |
| Occupational exposure/crosswalk integration | Yes | Five source files above; date/version manifest |
| Coverage and unmatched occupation diagnostics | Yes | Review actual coverage before applying scores to CPS |
| CPS import, spouse linkage, sample restrictions, outcomes/weights | No | Port and validate the original `02_clean_cps.R` and extract specification |
| Exposure thresholds, household regressions, event studies, robustness figures | No | Select the specification corresponding to the September 2026 slides and include its dependencies |

The local project contains both `24_main_regressions.R` and `29_main_regressions_common_q3.R`. They use different threshold constructions; they should not be mixed into one claimed replication of the slides. Additional wage-trend inputs also need their generating scripts.

The crosswalk sample ran successfully on the existing inputs. That validates execution of this component, not the household findings. BLS employment weights are optional in the current script, which falls back to equal weights if the file is absent; to match the employment-weighted description, provide the BLS file and inspect the log. External downloads are not version-pinned, and the code does not yet fail on an explicitly defined minimum match-coverage threshold.

This work in progress studies whether occupational exposure to generative AI is associated with labor supply adjustments within couples. It combines occupational measures with CPS household data. Exposure measures potential task overlap with AI, not observed adoption or an individual job-loss event.

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
