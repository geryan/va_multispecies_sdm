# Overview of the count misfit from the per-record predictive summaries.
suppressPackageStartupMessages({
  library(targets)
  library(dplyr)
  library(tidyr)
  library(tibble)
})
options(width = 200, pillar.sigfig = 3, dplyr.summarise.inform = FALSE)

args <- commandArgs(trailingOnly = TRUE)
outdir <- args[1]

cp <- readRDS(file.path(outdir, "count_predictive.rds"))
gnb <- readRDS(file.path(outdir, "gamma_nb.rds"))
zeta <- readRDS(file.path(outdir, "zeta.rds"))
sre <- readRDS(file.path(outdir, "sampling_re.rds"))

# check against the pipeline's DHARMa residuals (100 simulations)
dh <- tar_read(resids_and_rhats)$resids |>
  filter(data_type_plot == "count")
stopifnot(nrow(dh) == nrow(cp), all(dh$n == cp$n), all(dh$species == cp$species))
cat("\n== check against the stored DHARMa residuals ==\n")
cat("records:", nrow(cp), "\n")
cat("DHARMa residual > 0.95:", sum(dh$residual > 0.95), "; saturated (residual_norm > 4.7):", sum(dh$residual_norm > 4.7), "\n")
cat("of the saturated, observed count > 0:", sum(dh$residual_norm > 4.7 & dh$n > 0), "\n")
cat("exact PIT > 0.95:", sum(cp$pit > 0.95), "\n")
cat("cor(DHARMa residual, exact PIT):", round(cor(dh$residual, cp$pit), 3), "\n")

cat("\n== gamma_nb (posterior median, 90% interval) ==\n")
print(gnb |> arrange(desc(gamma_nb_med)), n = 30)

cat("\n== zeta (per count source; prior N(0, 0.1^2)) ==\n")
cat("sources:", nrow(zeta), "; posterior means range", round(range(zeta$zeta_mean), 3),
    "; SD of the means", round(sd(zeta$zeta_mean), 3), "\n")

cat("\n== sampling_re ==\n")
print(sre)

cat("\n== count records by method ==\n")
print(cp |> count(sampling_method, positive = n > 0) |> pivot_wider(names_from = positive, values_from = n, names_prefix = "pos_"))

pos <- cp |> filter(n > 0)
cat("\n== positive counts: how far into the upper tail ==\n")
cat("positive counts:", nrow(pos), "\n")
cat("P(Y >= y) < 0.025:", sum(pos$upper_tail < 0.025), sprintf("(%.0f%%)", 100 * mean(pos$upper_tail < 0.025)), "\n")
cat("P(Y >= y) < 0.01:", sum(pos$upper_tail < 0.01), sprintf("(%.0f%%)", 100 * mean(pos$upper_tail < 0.01)), "\n")
cat("P(Y >= y) < 0.001:", sum(pos$upper_tail < 0.001), sprintf("(%.0f%%)", 100 * mean(pos$upper_tail < 0.001)), "\n")
cat("P(Y >= 1) i.e. predicted P(count > 0) at the positive records: quantiles\n")
print(round(quantile(pos$p_pos_nb1, c(0.05, 0.25, 0.5, 0.75, 0.95)), 4))
cat("observed count at positive records with P(Y >= y) < 0.01: quantiles\n")
print(quantile(pos$n[pos$upper_tail < 0.01], c(0.05, 0.25, 0.5, 0.75, 0.95, 1)))
cat("share of those with count <= 5:", round(mean(pos$n[pos$upper_tail < 0.01] <= 5), 3), "\n")
cat("count = 1 records: n =", sum(pos$n == 1), "; P(Y>=1) < 0.01:", sum(pos$n == 1 & pos$upper_tail < 0.01), "\n")

cat("\n== by species ==\n")
by_sp <- cp |>
  group_by(species) |>
  summarise(
    n_rec = n(),
    n_pos = sum(n > 0),
    obs_p_pos = mean(n > 0),
    pred_p_pos_nb1 = mean(p_pos_nb1),
    pred_p_pos_poisson = mean(p_pos_poisson),
    sum_obs = sum(n),
    sum_mu = sum(mu_mean),
    med_pos_count = median(n[n > 0]),
    med_mu_at_pos = median(mu_mean[n > 0]),
    pos_tail_lt_01 = mean(upper_tail[n > 0] < 0.01)
  ) |>
  left_join(gnb |> select(species, gamma_nb_med), by = "species") |>
  arrange(desc(n_pos))
print(by_sp, n = 30, width = 200)

cat("\n== all species ==\n")
cat("observed share > 0:", round(mean(cp$n > 0), 4),
    "; NB1 predicted:", round(mean(cp$p_pos_nb1), 4),
    "; 1 - exp(-lambda) (the PA likelihood's link):", round(mean(cp$p_pos_poisson), 4), "\n")
cat("sum observed / sum predicted mean:", round(sum(cp$n) / sum(cp$mu_mean), 2), "\n")
cat("same without the 1% largest counts:",
    round(with(cp |> filter(n <= quantile(n, 0.99)), sum(n) / sum(mu_mean)), 2), "\n")
cat("median of predicted mean, all records:", signif(median(cp$mu_mean), 3),
    "; at positive records:", signif(median(pos$mu_mean), 3),
    "; median positive count:", median(pos$n), "\n")
cat("geometric mean of count / predicted mean at positive records:",
    signif(exp(mean(log(pos$n / pos$mu_mean))), 3), "\n")

saveRDS(by_sp, file.path(outdir, "by_species.rds"))
