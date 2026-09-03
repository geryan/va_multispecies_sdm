#!/usr/bin/env bash
# Run the 50-chain `keepre` fit (the 4 sound fixes; sampling_re kept in count) as
# 5 memory-safe batches of 10 chains (2000 warmup + 2000 samples each), then merge
# into one 50-chain draws object. Self-protected against idle/system sleep via
# caffeinate (a 10h run will otherwise be killed when the Mac idle-sleeps).
#
# Usage (from anywhere in the repo, ON AC POWER):
#   bash extras/run_keepre_caffeinated.sh
# Outputs: extras/sre_keepre_draws.rds (+ *_param_diag.csv, *_results.csv, plots).
set -u
cd "$(git rev-parse --show-toplevel 2>/dev/null || echo .)" || exit 1
LOG=extras/sre_keepre_full.log

# don't double-run
if pgrep -f 'sre_mixing_exploration' >/dev/null; then
  echo "a keepre run is already active -- aborting" ; exit 1
fi

# keep the Mac awake for the life of THIS script (system + idle sleep). NOTE: -s only
# holds on AC power -- if the laptop goes onto battery, macOS will sleep and kill this,
# hence the resumability below. KEEP IT PLUGGED IN.
caffeinate -s -i -w "$$" &

# resumable: append (don't truncate) and skip any batch whose draws already exist, so a
# kill only costs the in-flight batch -- re-running this script continues where it stopped.
echo "########## DRIVER (RE)START $(date) (caffeinated, resumable) ##########" >> "$LOG"
for b in 1 2 3 4 5; do
  if [ -f "extras/sre_keepre_b${b}_draws.rds" ]; then
    echo "########## BATCH $b SKIP -- draws already exist $(date) ##########" >> "$LOG"
    continue
  fi
  echo "########## BATCH $b START $(date) ##########" >> "$LOG"
  SRE_MODE=keepre SRE_TAG=keepre_b"$b" SRE_CHAINS=10 SRE_WARMUP=2000 SRE_SAMPLES=2000 \
    Rscript extras/sre_mixing_exploration.R >> "$LOG" 2>&1
  echo "########## BATCH $b END $(date) exit=$? ##########" >> "$LOG"
done
echo "########## MERGE START $(date) ##########" >> "$LOG"
SRE_MODE=merge_keepre SRE_TAG=keepre \
  SRE_BATCHES=keepre_b1,keepre_b2,keepre_b3,keepre_b4,keepre_b5 \
  Rscript extras/sre_mixing_exploration.R >> "$LOG" 2>&1
echo "########## MERGE END $(date) exit=$? ##########" >> "$LOG"
echo "########## DRIVER DONE $(date) ##########" >> "$LOG"
