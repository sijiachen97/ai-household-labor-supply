# Occupational AI exposure and household labor supply

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
