# How much do the counts and the presence-absence records each tell the fit
# about a species' overall level (alpha)? Expected Fisher information for
# log lambda, summed over records, at the posterior means (plug-in). The
# presence-only records carry none: their level is absorbed by the bias
# intercept gamma, which enters additively with alpha.
suppressPackageStartupMessages({
  library(targets)
  library(dplyr)
  library(tidyr)
  library(tibble)
})
options(width = 200, pillar.sigfig = 3, dplyr.summarise.inform = FALSE)

args <- commandArgs(trailingOnly = TRUE)
outdir <- args[1]

source("R/make_designmat_interactions.R")
source("R/cv_designmat.R")
source("R/os_record_loglik.R")

dat <- tar_read(model_data_spatial_cx)
ll <- os_record_loglik(
  draws_file = "outputs/draws/coef_draws.rds",
  model_data_spatial = dat,
  data_types = "pa",
  return_expected = TRUE
)
pa <- ll$pa$records |>
  mutate(
    lambda = rowMeans(-log1p(-pmin(ll$pa$expected, 1 - 1e-15)))
  )
rm(ll)
invisible(gc())

# Bernoulli with p = 1 - exp(-lambda): information for log lambda
pa <- pa |>
  mutate(info = ifelse(lambda > 700, 0, lambda^2 / expm1(lambda)))

cp <- readRDS(file.path(outdir, "count_predictive.rds"))
gnb <- readRDS(file.path(outdir, "gamma_nb.rds"))
cp <- cp |>
  left_join(gnb |> select(species, g = gamma_nb_med), by = "species")

# NB1 (size r = mu / g, prob p = 1 / (1 + g)): information for log mu, holding
# g fixed, is sum_{j >= 0} P(Y > j) r^2 / (r + j)^2. For r < 0.01 the j >= 1
# terms are below r^2 * pi^2 / 6 of the first, so it is P(Y > 0)
info_nb1 <- function(mu, g){
  r <- mu / g
  p <- 1 / (1 + g)
  out <- 1 - dnbinom(0, size = r, prob = p)
  big <- which(r >= 0.01)
  for (i in big) {
    J <- qnbinom(1 - 1e-12, size = r[i], prob = p[i])
    j <- 0:J
    out[i] <- sum(pnbinom(j, size = r[i], prob = p[i], lower.tail = FALSE) * r[i]^2 / (r[i] + j)^2)
  }
  out
}
cp <- cp |>
  mutate(
    info_nb1 = info_nb1(mu_mean, g),
    info_poisson = mu_mean
  )

# check the NB1 information formula against a numerical second derivative
chk <- tibble(mu = c(0.05, 1, 30, 400), g = c(300, 173, 98, 50))
chk$formula <- info_nb1(chk$mu, chk$g)
chk$numeric <- mapply(function(mu, g){
  y <- 0:qnbinom(1 - 1e-13, size = mu / g, prob = 1 / (1 + g))
  py <- dnbinom(y, size = mu / g, prob = 1 / (1 + g))
  h <- 1e-4
  l <- function(lm) dnbinom(y, size = exp(lm) / g, prob = 1 / (1 + g), log = TRUE)
  d2 <- (l(log(mu) + h) - 2 * l(log(mu)) + l(log(mu) - h)) / h^2
  -sum(py * d2)
}, chk$mu, chk$g)
cat("== NB1 information: formula against numerical second derivative ==\n")
print(chk)

info <- full_join(
  cp |>
    group_by(species) |>
    summarise(
      n_count = n(),
      info_count_nb1 = sum(info_nb1),
      info_count_if_poisson = sum(info_poisson)
    ),
  pa |>
    group_by(species) |>
    summarise(
      n_pa = n(),
      info_pa = sum(info)
    ),
  by = "species"
) |>
  mutate(
    count_share = info_count_nb1 / (info_count_nb1 + info_pa),
    count_share_if_poisson = info_count_if_poisson / (info_count_if_poisson + info_pa)
  ) |>
  arrange(desc(n_count))

cat("\n== information about each species' level, by data type ==\n")
print(info, n = 30, width = 200)
cat("\nall species: count share", round(sum(info$info_count_nb1) / sum(info$info_count_nb1 + info$info_pa), 3),
    "; if the counts were Poisson:", round(sum(info$info_count_if_poisson) / sum(info$info_count_if_poisson + info$info_pa), 3), "\n")
cat("per record, median information: count (NB1)", signif(median(cp$info_nb1), 3),
    "; PA", signif(median(pa$info), 3), "\n")
saveRDS(info, file.path(outdir, "information.rds"))
