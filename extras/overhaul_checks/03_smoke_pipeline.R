# Smoke test of the overhauled model core, end to end, on a tiny fit.
#
# Runs 03_smoke_targets.R: the real fit -> image -> predictions -> rasters ->
# cubes targets, with the real model data, but 2 chains of 20 warm-up and 20
# samples. Everything is written to a scratch folder and a scratch store; the
# project's store and outputs/ are only read.
#
# It checks three things.
#   1. The renamed functions and targets run, and their outputs have the
#      right shape.
#   2. A second run with nothing changed builds nothing.
#   3. A refit that writes a different image to the SAME path rebuilds
#      everything below it. This is the refit cascade: before the fix, the
#      unchanged path meant nothing downstream reran.
#
# The fit is far too short to mean anything: this tests the plumbing only.
# About 15 minutes and 3 GB of scratch space (a copy of the monthly offsets).
# Run from the repo root:
#   Rscript extras/overhaul_checks/03_smoke_pipeline.R <scratch folder>

library(targets)
library(dplyr)

arguments <- commandArgs(trailingOnly = TRUE)

smoke_dir <- if (length(arguments)) {
  arguments[1]
} else {
  file.path(tempdir(), "overhaul_smoke")
}

dir.create(
  smoke_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

smoke_store <- file.path(smoke_dir, "store")
smoke_script <- "extras/overhaul_checks/03_smoke_targets.R"

Sys.setenv(
  OVERHAUL_MAIN_STORE = "_targets",
  OVERHAUL_SMOKE_DIR = smoke_dir
)

failures <- character()

check <- function(ok, label, detail = NULL) {

  cat(
    sprintf(
      "%s  %s\n",
      if (ok) "ok  " else "FAIL",
      label
    )
  )

  if (!ok) {
    failures <<- c(failures, label)
    if (!is.null(detail)) {
      print(detail)
    }
  }

  invisible(ok)

}

run_smoke <- function(samples) {

  Sys.setenv(OVERHAUL_SMOKE_SAMPLES = samples)

  tar_make(
    script = smoke_script,
    store = smoke_store
  )

  tar_progress(store = smoke_store)

}

read_smoke <- function(name) {
  tar_read_raw(
    name,
    store = smoke_store
  )
}

input_names <- c(
  "model_data_spatial_cx",
  "target_covariate_names",
  "target_species",
  "bioregion_names",
  "covariate_rast_10",
  "offsets_avg_10",
  "offsets_5",
  "expert_offset_maps_10",
  "landcover_bare_10",
  "expert_maps"
)

# not below the fit: paths, inputs, and the expert-range boundary
not_below_fit <- c(
  input_names,
  "fit_image",
  "pred_prefix",
  "plot_dir",
  "expert_maps_buffer_1000"
)

###################
# 1. everything runs
###################

progress_1 <- run_smoke(samples = 20)

cat("\n1. first run\n")

check(
  all(progress_1$progress == "completed"),
  sprintf(
    "all %s targets built",
    nrow(progress_1)
  ),
  progress_1 |>
    filter(progress != "completed")
)

species <- read_smoke("target_species")
preds_1 <- read_smoke("preds")

check(
  all(file.exists(unlist(preds_1[c("lambda_no_offset", "p", "p_cv")]))) &&
    length(preds_1$md5) == 3,
  "preds holds the three prediction files and their md5s"
)

pred_p <- read_smoke("pred_p")
pred_lambda <- read_smoke("pred_lambda")

check(
  identical(names(pred_p), species) && identical(names(pred_lambda), species),
  "masked rasters have one layer per species, in order"
)

image_md5_1 <- unname(tools::md5sum(read_smoke("model_fit_image")))

draws <- readRDS(read_smoke("coef_draws"))

check(
  identical(draws$target_species, species) &&
    dim(draws$beta)[2] == 198,
  "coefficient draws: 18 species and 198 design columns"
)

cube_files <- read_smoke("abundance_cubes")

check(
  length(cube_files) == length(species) && all(file.exists(cube_files)),
  "one abundance cube per species, tracked as files"
)

# the cubes are pred_lambda by month: the mean of the last 12 layers of a
# species' cube is its pred_lambda layer
cube_agreement <- vapply(
  species,
  function(sp) {

    cube <- terra::rast(
      grep(
        sprintf("/%s_abundance.tif$", sp),
        cube_files,
        value = TRUE
      )
    )

    annual <- terra::values(terra::mean(cube[[13:24]]))[, 1]
    lambda <- terra::values(pred_lambda[[sp]])[, 1]

    c(
      layers = terra::nlyr(cube),
      na_mismatch = sum(is.na(annual) != is.na(lambda)),
      max_relative_difference = max(abs(annual - lambda), na.rm = TRUE) /
        max(abs(lambda), na.rm = TRUE)
    )

  },
  numeric(3)
) |>
  t()

check(
  all(cube_agreement[, "layers"] == 24) &&
    all(cube_agreement[, "na_mismatch"] == 0) &&
    all(cube_agreement[, "max_relative_difference"] < 1e-4),
  sprintf(
    "cubes: 24 months each, and their last-12-month mean equals pred_lambda (largest relative difference %.1e)",
    max(cube_agreement[, "max_relative_difference"])
  ),
  cube_agreement
)

dominant_expert <- read_smoke("pred_dominant_expert")
dominant <- read_smoke("pred_dominant")

check(
  sum(!is.na(terra::values(dominant_expert))) < sum(!is.na(terra::values(dominant))) &&
    all(file.exists(read_smoke("plot_pred_dominant_expert"))),
  "expert-range dominant maps: masked to the buffered expert ranges, and plotted"
)

###################
# 2. nothing changed, nothing builds
###################

progress_2 <- run_smoke(samples = 20)

cat("\n2. second run, nothing changed\n")

check(
  all(progress_2$progress == "skipped"),
  "every target skipped",
  progress_2 |>
    filter(progress != "skipped")
)

###################
# 3. a refit to the same path cascades
###################

progress_3 <- run_smoke(samples = 22)

cat("\n3. refit with a different result, same image path\n")

image_md5_3 <- unname(tools::md5sum(read_smoke("model_fit_image")))

check(
  identical(read_smoke("model_fit"), file.path(smoke_dir, "model_fit.RData")) &&
    image_md5_1 != image_md5_3,
  "model_fit returned the same path; the image behind it changed"
)

rebuilt <- progress_3$name[progress_3$progress == "completed"]

below_fit <- setdiff(
  progress_3$name,
  not_below_fit
)

check(
  setequal(rebuilt, below_fit),
  sprintf(
    "all %s targets below the fit rebuilt; the %s inputs and paths did not",
    length(below_fit),
    length(not_below_fit)
  ),
  list(
    not_rebuilt = setdiff(below_fit, rebuilt),
    rebuilt_unexpectedly = setdiff(rebuilt, below_fit)
  )
)

preds_3 <- read_smoke("preds")

check(
  identical(preds_1[1:3], preds_3[1:3]) &&
    !identical(preds_1$md5, preds_3$md5),
  "preds returned the same three paths; its md5s changed, which is what carries the refit to the maps"
)

###################
# summary
###################

if (length(failures)) {
  cat(
    sprintf(
      "\n%s CHECK(S) FAILED\n",
      length(failures)
    )
  )
  quit(status = 1)
}

cat("\nall smoke checks passed\n")
