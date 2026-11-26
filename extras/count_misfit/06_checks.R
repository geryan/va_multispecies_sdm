# (a) Is the size of a catch right once there is one, and only its frequency
#     wrong? Predictive distribution given Y > 0 at each positive record.
# (b) Duplicated records among the positives.
# (c) Does the duration effect hold within sources?
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
gnb <- readRDS(file.path(outdir, "gamma_nb.rds"))
set.seed(20261125)
pos <- cp |>
  filter(n > 0) |>
  left_join(gnb |> select(species, g = gamma_nb_med), by = "species") |>
  mutate(
    # predictive given a catch: P(Y >= y | Y > 0), and the conditional PIT
    cond_upper = upper_tail / p_pos_nb1,
    cond_pit = (cdf_ym1 - p0_nb1 + runif(n()) * (cdf_y - cdf_ym1)) / p_pos_nb1,
    cond_mean = mu_mean / p_pos_nb1
  )

cat("== (a) size of a catch, given one: predictive given Y > 0 ==\n")
cat("positives with P(Y >= y | Y > 0) < 0.01:", sum(pos$cond_upper < 0.01),
    sprintf("(%.1f%%)", 100 * mean(pos$cond_upper < 0.01)),
    "; < 0.025:", sprintf("%.1f%%", 100 * mean(pos$cond_upper < 0.025)),
    "; unconditional < 0.01:", sprintf("%.1f%%", 100 * mean(pos$upper_tail < 0.01)), "\n")
cat("conditional PIT, share in each tenth (uniform = 0.10 each):\n")
print(round(table(cut(pos$cond_pit, seq(0, 1, 0.1), include.lowest = TRUE)) / nrow(pos), 3))

# conditional median of the predictive given Y > 0, per record, by inversion
# on a grid (draws averaged into p0, so use the plug-in gamma and mean as a guide)
cond_median <- function(mu, g){
  r <- mu / g
  p <- 1 / (1 + g)
  p0 <- dnbinom(0, size = r, prob = p)
  target <- p0 + 0.5 * (1 - p0)
  qnbinom(target, size = r, prob = p)
}
pos <- pos |>
  mutate(cond_med_plugin = mapply(cond_median, mu_mean, g))

cat("\nby species: observed positives against the model's predictive given a catch\n")
print(
  pos |>
    group_by(species) |>
    summarise(
      n_pos = n(),
      gamma = first(g),
      approx_mean_given_catch = first(g) / log1p(first(g)),
      model_mean_given_catch = median(cond_mean),
      model_median_given_catch = median(cond_med_plugin),
      obs_median = median(n),
      cond_tail_lt_01 = mean(cond_upper < 0.01),
      obs_p_pos_all = NA_real_
    ) |>
    select(-obs_p_pos_all) |>
    arrange(desc(n_pos)),
  n = 30
)

cat("\n== (b) duplicated positive count records ==\n")
dup <- pos |>
  group_by(species, loc_date, n) |>
  mutate(copies = n()) |>
  ungroup()
cat("positive records in a species x location-date x count group of 2 or more:",
    sum(dup$copies > 1), sprintf("(%.1f%%)", 100 * mean(dup$copies > 1)), "\n")
dedup <- dup |>
  group_by(species, loc_date, n) |>
  slice(1) |>
  ungroup()
cat("after keeping one of each: positives", nrow(dedup),
    "; P(Y >= y) < 0.01:", sprintf("%.1f%%", 100 * mean(dedup$upper_tail < 0.01)),
    "(all positives:", sprintf("%.1f%%)", 100 * mean(pos$upper_tail < 0.01)), "\n")
all_dup <- cp |>
  group_by(species, loc_date, n) |>
  mutate(copies = n()) |>
  ungroup()
cat("all count records in such groups:", sum(all_dup$copies > 1), "of", nrow(cp), "\n")

cat("\n== (c) duration within source (positive counts) ==\n")
pd <- pos |>
  filter(!is.na(study_months), study_months > 0) |>
  mutate(source_id = factor(source_id))
m_marg <- lm(log_ratio ~ log(study_months), data = pd)
m_within <- lmer(log_ratio ~ log(study_months) + (1 | source_id), data = pd)
cat("marginal slope:", round(coef(m_marg)[2], 3), "(SE", round(coef(summary(m_marg))[2, 2], 3), ")\n")
cat("with a source intercept:", round(fixef(m_within)[2], 3),
    "(SE", round(coef(summary(m_within))[2, 2], 3), ")\n")
# purely within-source: centre duration on each source's mean
pd <- pd |>
  group_by(source_id) |>
  mutate(
    lm_c = log(study_months) - mean(log(study_months)),
    lm_bar = mean(log(study_months))
  ) |>
  ungroup()
m_wb <- lmer(log_ratio ~ lm_c + lm_bar + (1 | source_id), data = pd)
cat("within-source slope:", round(fixef(m_wb)["lm_c"], 3),
    "(SE", round(coef(summary(m_wb))["lm_c", 2], 3), ")",
    "; between-source slope:", round(fixef(m_wb)["lm_bar"], 3),
    "(SE", round(coef(summary(m_wb))["lm_bar", 2], 3), ")\n")
cat("sources whose positives span more than one duration:",
    pd |> group_by(source_id) |> summarise(k = n_distinct(study_months)) |> filter(k > 1) |> nrow(),
    "of", n_distinct(pd$source_id), "\n")

# added 2026-11-25: most count records are inferred zeros (a species not
# recorded at a location-date where others were counted), so the share of
# records with a catch mixes two kinds of record
cat("\n== (d) how often a catch happens, reported against inferred records ==\n")
catch_by <- function(d){
  d |>
    summarise(
      n_rec = n(),
      obs_p_pos = mean(n > 0),
      pred_p_pos_nb1 = mean(p_pos_nb1),
      pred_p_pos_pa_link = mean(p_pos_poisson),
      median_mu = median(mu_mean)
    )
}
print(
  cp |>
    group_by(inferred) |>
    catch_by()
)
print(
  cp |>
    filter(species %in% c("arabiensis", "funestus", "gambiae", "pharoensis", "coluzzii")) |>
    group_by(
      species,
      inferred
    ) |>
    catch_by(),
  n = 20
)
cat("location-dates with an inferred zero:",
    n_distinct(cp$loc_date[cp$inferred]),
    "of", n_distinct(cp$loc_date), "\n")

# do the inferred zeros drive the NB1 dispersion? Profile gamma per species at
# the fitted means (as in 04) on all records and on the reported records only
cat("\n== (e) NB1 dispersion chosen by the counts (means held fixed): all records against reported only ==\n")
prof_g <- function(d){
  f <- function(lg){
    -sum(
      dnbinom(
        d$n,
        size = d$mu_mean / exp(lg),
        prob = 1 / (1 + exp(lg)),
        log = TRUE
      )
    )
  }
  exp(optimize(f, c(log(0.5), log(1e5)))$minimum)
}
tail_nb1 <- function(y, mu, g){
  pnbinom(
    y - 1,
    size = mu / g,
    prob = 1 / (1 + g),
    lower.tail = FALSE
  )
}
g_cmp <- cp |>
  group_by(species) |>
  group_modify(function(d, key){
    rep <- d |> filter(!inferred)
    pos <- rep |> filter(n > 0)
    g_all <- prof_g(d)
    g_rep <- prof_g(rep)
    tibble(
      n_reported = nrow(rep),
      n_pos = nrow(pos),
      gamma_all = g_all,
      gamma_reported = g_rep,
      obs_p_pos_reported = mean(rep$n > 0),
      pred_p_pos_reported_g_all = mean(1 - dnbinom(0, size = rep$mu_mean / g_all, prob = 1 / (1 + g_all))),
      pred_p_pos_reported_g_rep = mean(1 - dnbinom(0, size = rep$mu_mean / g_rep, prob = 1 / (1 + g_rep))),
      n_tail_g_all = sum(tail_nb1(pos$n, pos$mu_mean, g_all) < 0.01),
      n_tail_g_rep = sum(tail_nb1(pos$n, pos$mu_mean, g_rep) < 0.01)
    )
  }) |>
  ungroup() |>
  mutate(ratio = gamma_reported / gamma_all) |>
  arrange(desc(n_pos))
print(
  g_cmp |>
    select(species, n_reported, n_pos, gamma_all, gamma_reported, ratio, obs_p_pos_reported, pred_p_pos_reported_g_all, pred_p_pos_reported_g_rep),
  n = 30,
  width = 200
)
cat(
  "positives beyond the 99th percentile: gamma from all records",
  sprintf("%.1f%%", 100 * sum(g_cmp$n_tail_g_all) / sum(g_cmp$n_pos)),
  "; gamma from reported records only",
  sprintf("%.1f%%", 100 * sum(g_cmp$n_tail_g_rep) / sum(g_cmp$n_pos)),
  "\n"
)
cat("reported records, share with a catch: observed", round(mean(cp$n[!cp$inferred] > 0), 3),
    "; NB1 at gamma from all records", round(sum(g_cmp$pred_p_pos_reported_g_all * g_cmp$n_reported) / sum(g_cmp$n_reported), 3),
    "; at gamma from reported only", round(sum(g_cmp$pred_p_pos_reported_g_rep * g_cmp$n_reported) / sum(g_cmp$n_reported), 3), "\n")
