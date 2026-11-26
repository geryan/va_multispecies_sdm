#!/bin/sh
# Run the count-misfit analysis on the fit now in the store (see README.md).
# From the project root:
#   sh extras/count_misfit/run_all.sh outputs/count_misfit/fit_<YYYYMMDD>
# Each script's printed output goes to <outdir>/logs/<script>.log. Reads the
# store and outputs/draws/coef_draws.rds; writes only to <outdir>. About 40 s.
set -e
out="$1"
if [ -z "$out" ]; then
  echo "usage: sh extras/count_misfit/run_all.sh <outdir>"
  exit 1
fi
dir=extras/count_misfit
mkdir -p "$out/logs"
for s in 01_count_predictive 02_overview 03_structure 04_distributions 05_information 06_checks 07_effort_fields 08_profile_ranges; do
  echo "== $s"
  Rscript "$dir/$s.R" "$out" > "$out/logs/$s.log" 2>&1
done
echo "== 04b_check_pln"
Rscript "$dir/04b_check_pln.R" "$dir/04_distributions.R" > "$out/logs/04b_check_pln.log" 2>&1
echo "done: $out/logs"
