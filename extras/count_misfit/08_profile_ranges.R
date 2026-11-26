# The ranges README.md quotes from the profiles of 04 (no refitting): the NB1
# dispersion the counts alone would choose against the posterior, the count
# level factors, and the Poisson-lognormal spread.
#
#   Rscript extras/count_misfit/08_profile_ranges.R <outdir>
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})
options(
  width = 200,
  pillar.sigfig = 3,
  dplyr.summarise.inform = FALSE
)

outdir <- commandArgs(trailingOnly = TRUE)[1]
res <- readRDS(
  file.path(
    outdir,
    "distribution_profiles.rds"
  )
)

cat("== NB1 at the fitted means: the dispersion the counts alone choose / posterior median ==\n")
r <- res |>
  filter(scenario == "NB1, fitted means") |>
  mutate(ratio = disp / gamma_nb_med) |>
  select(
    species,
    n_pos,
    gamma_nb_med,
    gamma_profile = disp,
    ratio
  ) |>
  arrange(ratio)
print(
  r,
  n = 30
)
cat("ratio range:", round(range(r$ratio), 2), "\n")

cat("\n== count level factor (multiplier on the fitted mean), scenarios with a free level ==\n")
print(
  res |>
    filter(grepl("level", scenario)) |>
    group_by(scenario) |>
    summarise(
      min = min(level),
      median = median(level),
      max = max(level)
    )
)

cat("\n== Poisson-lognormal: SD of the log-scale scatter (sigma) by species ==\n")
pln <- res |>
  filter(grepl("Poisson-lognormal|PLN", scenario)) |>
  select(
    species,
    n_pos,
    scenario,
    sigma = disp
  ) |>
  pivot_wider(
    names_from = scenario,
    values_from = sigma
  )
print(
  pln,
  n = 30
)
pln_range <- res |>
  filter(grepl("Poisson-lognormal|PLN", scenario)) |>
  group_by(scenario) |>
  summarise(
    min_sigma = min(disp[species != "quadriannulatus"]),
    max_sigma = max(disp[species != "quadriannulatus"]),
    quadriannulatus = disp[species == "quadriannulatus"]
  ) |>
  mutate(
    min_factor = exp(min_sigma),
    max_factor = exp(max_sigma)
  )
cat("ranges without quadriannulatus (factor = exp(sigma), the multiplier for one SD):\n")
print(pln_range)

cat("\nzeta prior, +-2 SD (0.1) as a multiplier on a source's counts:", round(exp(c(-0.2, 0.2)), 2), "\n")

cat("\n== scenario totals with more digits; gain = log-likelihood over 'NB1, fitted means' ==\n")
cp <- readRDS(
  file.path(
    outdir,
    "count_predictive.rds"
  )
)
n_rec <- table(cp$species)
tot <- res |>
  group_by(scenario) |>
  summarise(
    loglik = sum(loglik),
    pred_share_pos = sum(pred_p_pos * as.numeric(n_rec[species])) / nrow(cp),
    pos_tail_lt_01 = sum(n_pos_tail_lt_01) / sum(n_pos)
  ) |>
  mutate(gain = loglik - loglik[scenario == "NB1, fitted means"]) |>
  arrange(desc(loglik))
print(
  as.data.frame(tot),
  digits = 6
)

cat("\n== count records ==\n")
cat(
  "records:",
  nrow(cp),
  "; inferred (zeros added where a species was not recorded):",
  sum(cp$inferred, na.rm = TRUE),
  "; positive:",
  sum(cp$n > 0),
  "\n"
)

info <- readRDS(
  file.path(
    outdir,
    "information.rds"
  )
)
cat(
  "information about the species' levels, all species: counts",
  round(sum(info$info_count_nb1) / sum(info$info_count_nb1 + info$info_pa), 4),
  "; PA",
  round(sum(info$info_pa) / sum(info$info_count_nb1 + info$info_pa), 4),
  "\n"
)
