#!/bin/bash
set -euo pipefail
cd "$(dirname "$0")/.."
for script in 01_merge_ai_exposure 02_clean_cps 03_build_occupation_wages; do
  Rscript "code/data_process/$script.R"
done
for script in 24_main_regressions 25_survivor_robustness 26_ai_horserace 18_earnings_first_stage; do
  Rscript "code/analysis/$script.R"
done
