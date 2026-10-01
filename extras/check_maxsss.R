# Does maxsss_thresholds() (R/maxsss_thresholds.R) choose the same maxSSS
# threshold as PresenceAbsence::optimal.thresholds(opt.methods = "MaxSens+Spec")?
# And does binarise_by_threshold() apply it as intended?
#
# Run from the project root:
#   Rscript extras/check_maxsss.R
#
# A. 200 synthetic data sets, some with ties in the predictions
# B. the _rep predictions and model data from the targets store (read only)
#
# PresenceAbsence is handed the same candidate thresholds -- 0 and every
# distinct predicted value -- so the two should agree exactly. Against its
# default 101-point grid (0, 0.01, ..., 1) they can only agree to within the
# grid spacing.

suppressPackageStartupMessages({
  library(targets)
  library(terra)
  library(dplyr)
})

source("R/maxsss_thresholds.R")
source("R/binarise_by_threshold.R")

set.seed(20260930)

pa_threshold <- function(pred, observed, threshold){
  PresenceAbsence::optimal.thresholds(
    data.frame(
      id = seq_along(pred),
      observed = observed,
      pred = pred
    ),
    threshold = threshold,
    opt.methods = "MaxSens+Spec"
  )[1, 2]
}


############
# A. synthetic
############

res_a <- t(
  replicate(
    200,
    {
      n <- sample(20:400, 1)
      observed <- rbinom(n, 1, runif(1, 0.1, 0.9))
      pred <- plogis(rnorm(n, observed * runif(1, 0, 3) - 1, 1))
      # coarse rounding in some data sets, to make ties
      pred <- round(pred, sample(c(2, 8), 1))
      c(
        ours = maxsss(pred, observed)[["threshold"]],
        pa_same = pa_threshold(pred, observed, sort(unique(c(0, pred)))),
        pa_grid = pa_threshold(pred, observed, 101)
      )
    }
  )
)

cat("\n== A. 200 synthetic data sets ==\n")
cat(
  "identical to PresenceAbsence given the same candidates:",
  sum(res_a[, "ours"] == res_a[, "pa_same"]), "of 200\n"
)
cat(
  "largest difference from PresenceAbsence's default 0.01 grid:",
  signif(max(abs(res_a[, "ours"] - res_a[, "pa_grid"])), 3), "\n"
)


############
# B. the _rep predictions
############

pred_p <- tar_read(pred_p_rep)
model_data_spatial <- tar_read(model_data_spatial)

th <- maxsss_thresholds(
  pred_p,
  model_data_spatial
)

cell <- cellFromXY(
  pred_p[[1]],
  as.matrix(model_data_spatial[, c("longitude", "latitude")])
)

obs <- model_data_spatial |>
  mutate(cell = cell) |>
  filter(!is.na(species), !is.na(cell)) |>
  group_by(species, cell) |>
  summarise(observed = as.integer(any(presence == 1)), .groups = "drop")

vals <- terra::extract(pred_p, obs$cell)

res_b <- bind_rows(
  lapply(
    names(pred_p),
    function(sp){
      k <- obs$species == sp
      pr <- vals[k, sp]
      ok <- !is.na(pr)
      pa <- pa_threshold(pr[ok], obs$observed[k][ok], sort(unique(c(0, pr[ok]))))
      tibble(
        species = sp,
        ours = th$threshold[th$species == sp],
        presence_absence = pa,
        identical = identical(ours, pa)
      )
    }
  )
)

cat("\n== B. _rep predictions: thresholds ==\n")
print(as.data.frame(res_b), row.names = FALSE, digits = 4)

# every binary layer is the species' own threshold rule applied to its layer
b <- binarise_by_threshold(pred_p, th)

layer_ok <- vapply(
  seq_len(nlyr(pred_p)),
  function(i){
    v <- values(pred_p[[i]], mat = FALSE)
    bv <- values(b[[i]], mat = FALSE)
    expect <- as.numeric(predicted_present(v, th$threshold[i]))
    identical(is.na(expect), is.na(bv)) && all(expect[!is.na(expect)] == bv[!is.na(bv)])
  },
  logical(1)
)

cat("\nbinary layers matching their threshold rule:", sum(layer_ok), "of", length(layer_ok), "\n")

verdict <- all(res_a[, "ours"] == res_a[, "pa_same"]) && all(res_b$identical) && all(layer_ok)

cat(
  "\n",
  if (verdict) "ALL IDENTICAL to PresenceAbsence, and binarising is correct" else "DIFFERENCES: see above",
  "\n",
  sep = ""
)
