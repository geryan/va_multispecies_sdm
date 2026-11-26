# Where the count gap sits: location-dates, sources, duration, single records.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(lme4)
})
options(width = 200, pillar.sigfig = 3, dplyr.summarise.inform = FALSE)

args <- commandArgs(trailingOnly = TRUE)
outdir <- args[1]

cp <- readRDS(file.path(outdir, "count_predictive.rds")) |>
  mutate(
    # location-date: source x method x detailed method x location x start date x duration
    loc_date = paste(
      source_id,
      sampling_method,
      sampling_method_detailed,
      latitude,
      longitude,
      start_date,
      study_months
    ),
    log_ratio = log(n / mu_mean)
  )
zeta <- readRDS(file.path(outdir, "zeta.rds"))

cat("== structure of the count records ==\n")
cat("count location-dates:", n_distinct(cp$loc_date),
    "; sources:", n_distinct(cp$source_id), "\n")
ld <- cp |>
  group_by(loc_date) |>
  summarise(
    n_sp = n(),
    n_pos = sum(n > 0),
    total = sum(n),
    months = first(study_months),
    method = first(sampling_method)
  )
cat("records per location-date (quantiles):\n")
print(quantile(ld$n_sp, c(0, 0.25, 0.5, 0.75, 1)))
cat("location-dates with no positive count:", sum(ld$n_pos == 0),
    sprintf("(%.0f%%)", 100 * mean(ld$n_pos == 0)), "\n")
cat("study_months over count records (quantiles):\n")
print(quantile(cp$study_months, c(0, 0.1, 0.25, 0.5, 0.75, 0.9, 0.99, 1), na.rm = TRUE))

pos <- cp |> filter(n > 0)

cat("\n== variance decomposition of log(count / predicted mean), positive counts ==\n")
m <- lmer(
  log_ratio ~ 1 + (1 | source_id) + (1 | loc_date),
  data = pos |> mutate(source_id = factor(source_id))
)
vc <- as.data.frame(VarCorr(m))
vc$share <- vc$vcov / sum(vc$vcov)
print(vc[, c("grp", "vcov", "sdcor", "share")], digits = 3)
cat("intercept (mean log ratio):", round(fixef(m)[1], 2),
    "-> x", signif(exp(fixef(m)[1]), 3), "\n")
cat("raw SD of log ratio:", round(sd(pos$log_ratio), 2), "\n")

cat("\n== the same for all records on the PIT normal score (zeros included) ==\n")
m2 <- lmer(
  z ~ 1 + (1 | source_id) + (1 | loc_date),
  data = cp |> mutate(source_id = factor(source_id))
)
vc2 <- as.data.frame(VarCorr(m2))
vc2$share <- vc2$vcov / sum(vc2$vcov)
print(vc2[, c("grp", "vcov", "sdcor", "share")], digits = 3)

cat("\n== per source: log(sum count / sum predicted mean) ==\n")
src <- cp |>
  group_by(source_id) |>
  summarise(
    n_rec = n(),
    n_pos = sum(n > 0),
    sum_n = sum(n),
    sum_mu = sum(mu_mean),
    months_med = median(study_months)
  ) |>
  mutate(log_gap = log(sum_n / sum_mu)) |>
  left_join(zeta |> select(source_id, zeta_mean), by = "source_id")
s5 <- src |> filter(n_pos >= 1, n_rec >= 5)
cat("sources with >= 5 records and a positive count:", nrow(s5), "\n")
cat("log gap quantiles:\n")
print(round(quantile(s5$log_gap, c(0.05, 0.25, 0.5, 0.75, 0.95)), 2))
cat("SD of log gap:", round(sd(s5$log_gap), 2),
    "; SD of fitted zeta means:", round(sd(s5$zeta_mean), 3),
    "; zeta prior SD 0.1\n")
cat("cor(log gap, fitted zeta):", round(cor(s5$log_gap, s5$zeta_mean), 2), "\n")
cat("sources with every count 0:", sum(src$n_pos == 0), "of", nrow(src), "\n")

cat("\n== duration: does the gap grow with study_months? (positive counts) ==\n")
pos_d <- pos |>
  filter(!is.na(study_months), study_months > 0)
cat("records:", nrow(pos_d), "\n")
fit_d <- lm(log_ratio ~ log(study_months), data = pos_d)
print(round(coef(summary(fit_d)), 3))
cat("Spearman cor(z, study_months), all records:",
    round(cor(cp$z, cp$study_months, method = "spearman", use = "complete.obs"), 3), "\n")
print(
  pos |>
    mutate(
      dur = cut(
        study_months,
        c(0, 1, 3, 12, Inf),
        right = TRUE,
        include.lowest = TRUE
      )
    ) |>
    group_by(dur) |>
    summarise(
      n_pos = n(),
      med_count = median(n),
      med_mu = median(mu_mean),
      gm_ratio = exp(mean(log_ratio)),
      tail_lt_01 = mean(upper_tail < 0.01)
    )
)

cat("\n== by method (positive counts) ==\n")
print(
  pos |>
    group_by(sampling_method) |>
    summarise(
      n_pos = n(),
      med_count = median(n),
      med_mu = median(mu_mean),
      gm_ratio = exp(mean(log_ratio)),
      tail_lt_01 = mean(upper_tail < 0.01)
    ) |>
    arrange(desc(n_pos))
)

cat("\n== the most extreme positive counts ==\n")
print(
  pos |>
    arrange(upper_tail) |>
    select(species, source_id, sampling_method_detailed, start_date, study_months, n, mu_mean, p_pos_nb1, upper_tail) |>
    head(20),
  width = 200
)

cat("\n== sources holding the extreme positives (P(Y >= y) < 0.01) ==\n")
ext_src <- pos |>
  filter(upper_tail < 0.01) |>
  count(source_id, name = "n_extreme") |>
  arrange(desc(n_extreme)) |>
  mutate(cum_share = cumsum(n_extreme) / sum(n_extreme))
cat("sources:", nrow(ext_src), "; top 10 hold", round(100 * ext_src$cum_share[10]), "% ; top 25 hold",
    round(100 * ext_src$cum_share[25]), "%\n")

saveRDS(src, file.path(outdir, "by_source.rds"))
