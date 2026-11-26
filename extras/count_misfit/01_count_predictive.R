# Read-only diagnostics of the count underprediction (current fit, 2026-11-22).
# Rebuilds each count record's predictive distribution from coef_draws.rds with
# os_record_loglik() (kappa = 1, i.e. the current model), then saves per-record
# summaries for the analysis script. Nothing in the store is written.
#
# Run from the project root:
#   Rscript extras/count_misfit/01_count_predictive.R outputs/count_misfit/fit_<YYYYMMDD>
# or run every script with extras/count_misfit/run_all.sh (see README.md)

suppressPackageStartupMessages({
  library(targets)
  library(dplyr)
  library(tidyr)
  library(tibble)
})

args <- commandArgs(trailingOnly = TRUE)
outdir <- args[1]
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

source("R/make_designmat_interactions.R")
source("R/cv_designmat.R")
source("R/os_record_loglik.R")

meta_before <- file.info("_targets/meta/meta")$mtime

dat <- tar_read(model_data_spatial_cx)
draws_file <- "outputs/draws/coef_draws.rds"
fit <- readRDS(draws_file)
cat(
  "draws:",
  draws_file,
  format(file.info(draws_file)$mtime),
  unname(tools::md5sum(draws_file)),
  "\n"
)

ll <- os_record_loglik(
  draws_file = draws_file,
  model_data_spatial = dat,
  data_types = "count",
  return_expected = TRUE
)

recs <- ll$count$records
mu <- ll$count$expected            # [records x draws]
D <- ncol(mu)
n_rec <- nrow(mu)

# the records as the fit orders them, with every column, for joining on
count_rows <- dat |>
  filter(data_type == "count")
stopifnot(
  nrow(count_rows) == n_rec,
  all(count_rows$n == recs$n),
  all(count_rows$species == recs$species)
)

sp_id <- match(recs$species, fit$target_species)
gnb <- t(fit$gamma_nb[, sp_id, drop = FALSE])   # [records x draws]
size <- mu / gnb
prob <- 1 / (1 + gnb)
y <- recs$n

# predictive probabilities, averaged over the posterior draws
p0_nb1 <- rowMeans(
  matrix(
    dnbinom(0, size = size, prob = prob),
    nrow = n_rec
  )
)
cdf_y <- rowMeans(
  matrix(
    pnbinom(rep(y, times = D), size = size, prob = prob),
    nrow = n_rec
  )
)
cdf_ym1 <- rowMeans(
  matrix(
    ifelse(
      rep(y, times = D) > 0,
      pnbinom(pmax(rep(y, times = D) - 1, 0), size = size, prob = prob),
      0
    ),
    nrow = n_rec
  )
)

# the presence probability the presence-absence likelihood would give for the
# same linear predictor: 1 - exp(-lambda e^sre). Count records also carry zeta,
# so remove it for this comparison
sid <- fit$sid_lookup$sid[match(recs$source_id, fit$sid_lookup$source_id)]
zeta_col <- match(sid, fit$count_sids)
zeta_rec <- t(fit$zeta[, zeta_col, drop = FALSE])
lambda_pa <- mu / exp(zeta_rec)
p_pos_poisson <- rowMeans(-expm1(-lambda_pa))

set.seed(20261124)
u <- runif(n_rec)
pit <- cdf_ym1 + u * (cdf_y - cdf_ym1)

out <- count_rows |>
  select(
    species,
    latitude,
    longitude,
    model_date,
    start_date,
    end_date,
    study_months,
    sampling_method,
    sampling_method_detailed,
    source_id,
    inferred,
    n
  ) |>
  mutate(
    rec = row_number(),
    mu_mean = rowMeans(mu),
    mu_med = apply(mu, 1, median),
    log_mu_mean = rowMeans(log(mu)),
    zeta_mean = rowMeans(zeta_rec),
    gamma_nb_med = apply(gnb, 1, median),
    p0_nb1 = p0_nb1,
    p_pos_nb1 = 1 - p0_nb1,
    p_pos_poisson = p_pos_poisson,
    cdf_ym1 = cdf_ym1,
    cdf_y = cdf_y,
    upper_tail = 1 - cdf_ym1,       # P(Y >= y)
    pit = pit,
    z = qnorm(pmin(pmax(pit, 1e-12), 1 - 1e-12))
  )

saveRDS(
  out,
  file.path(outdir, "count_predictive.rds")
)

gamma_summary <- tibble(
  species = fit$target_species,
  gamma_nb_med = apply(fit$gamma_nb, 2, median),
  gamma_nb_q05 = apply(fit$gamma_nb, 2, quantile, 0.05),
  gamma_nb_q95 = apply(fit$gamma_nb, 2, quantile, 0.95)
)
saveRDS(
  gamma_summary,
  file.path(outdir, "gamma_nb.rds")
)

zeta_summary <- tibble(
  sid = fit$count_sids,
  zeta_mean = colMeans(fit$zeta),
  zeta_sd = apply(fit$zeta, 2, sd)
) |>
  left_join(
    fit$sid_lookup,
    by = "sid"
  )
saveRDS(
  zeta_summary,
  file.path(outdir, "zeta.rds")
)

sre <- tibble(
  method = fit$sampling_methods,
  sampling_re_mean = colMeans(fit$sampling_re),
  share = as.numeric(fit$sm_prop[fit$sampling_methods])
)
saveRDS(
  sre,
  file.path(outdir, "sampling_re.rds")
)

cat("records:", n_rec, " draws:", D, "\n")
cat("meta untouched:", identical(meta_before, file.info("_targets/meta/meta")$mtime), "\n")
