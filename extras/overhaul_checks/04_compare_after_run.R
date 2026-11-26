# After the overhauled pipeline has been run: are the data going through it the
# same as before, in the _rep_cx set? Compares against the snapshot taken by
# 01_snapshot_before.R.
#
# Read-only, apart from its report in outputs/overhaul_check/. Run from the
# repo root once tar_make() has built the model targets:
#   Rscript extras/overhaul_checks/04_compare_after_run.R
#
# Loads the new fit image, and the old one if it is still on disk, one at a
# time: allow about 5 minutes and ~10 GB of memory.
#
#   A. The stored inputs to the fit are the snapshot's, hash for hash.
#   B. The data inside the new fit image are the data inside the old one.
#   C. The cubes agree with pred_lambda.
#   D. Old against new results. The refit is a new MCMC run with a longer
#      warm-up, so these differ; this is a report to read, not a pass or fail.
#
# The old fit image, prediction rasters and coefficient draws are needed for
# the second half of B and for D: run this before deleting them.
#
# A store other than the project's can be given as an argument. That is how
# this script was itself tried out, on the smoke test's scratch store, where A
# is expected to fail.

library(targets)
library(dplyr)

source("extras/overhaul_checks/helpers.R")

outdir <- "outputs/overhaul_check"

arguments <- commandArgs(trailingOnly = TRUE)

store <- if (length(arguments)) {
  arguments[1]
} else {
  "_targets"
}

read_target <- function(name) {
  tar_read_raw(
    name,
    store = store
  )
}

before <- readRDS(
  file.path(
    outdir,
    "snapshot_before.rds"
  )
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

old_file <- function(pattern) {
  grep(
    pattern,
    before$files$path,
    value = TRUE
  )
}

###################
# A. the stored inputs to the fit
###################

edges <- tar_network(
  targets_only = TRUE,
  outdated = FALSE
)$edges

fit_inputs <- ancestors(
  "model_fit",
  edges
) |>
  setdiff("fit_image")

inputs <- before$meta |>
  filter(name %in% fit_inputs) |>
  select(
    name,
    data_before = data
  ) |>
  full_join(
    tar_meta(
      fields = "data",
      store = store
    ) |>
      filter(name %in% fit_inputs) |>
      select(
        name,
        data_now = data
      ),
    by = "name"
  ) |>
  mutate(
    same = !is.na(data_before) & !is.na(data_now) & data_before == data_now
  )

outdated <- tar_outdated(store = store)

cat("\nA. the stored inputs to the fit\n")

check(
  nrow(inputs) == length(fit_inputs) && all(inputs$same),
  sprintf(
    "all %s targets upstream of the fit hold the same data as in the snapshot",
    length(fit_inputs)
  ),
  inputs |>
    filter(!same)
)

check(
  inputs$same[inputs$name == "model_data_spatial_cx"],
  "model_data_spatial_cx is unchanged"
)

model_targets <- c(
  "model_fit",
  "model_fit_image",
  "resids_and_rhats",
  "preds",
  "pred_p",
  "pred_lambda",
  "pred_cv",
  "coef_draws",
  "abundance_cubes"
)

check(
  !any(model_targets %in% outdated),
  "the fit, its image, the predictions, the draws and the cubes are built and up to date",
  intersect(model_targets, outdated)
)

###################
# B. the data inside the fit image
###################

new_image <- read_target("model_fit_image")

image <- compare_image_to_snapshot(
  new_image,
  before$image
)

# run settings, which the function saves along with everything else
settings <- c(
  "image_name",
  "n_burnin",
  "n_samples",
  "n_chains",
  "n_cores"
)

# the fit gives each background point a sampling method at random, from the
# target's seed, and the target's name -- hence its seed -- changed. These four
# tables carry that draw
imputed <- c(
  "model_data_spatial_bg",
  "bg_data",
  "model_data",
  "pobg_data_index"
)

compared <- image$objects |>
  filter(!is.na(same))

unexpected <- compared |>
  filter(
    !same,
    !name %in% c(settings, imputed)
  )

cat("\nB. the data inside the fit image\n")

check(
  nrow(unexpected) == 0,
  sprintf(
    "%s of %s data objects in the new image are identical to the old image's; the rest are run settings and the random background draw",
    sum(compared$same),
    nrow(compared)
  ),
  unexpected |>
    select(name, class)
)

check(
  image$model_data_dim_same && length(image$model_data_columns_differing) == 0,
  "the model data the fit received: same size, every column identical",
  image$model_data_columns_differing
)

must_match <- c(
  "model_data_spatial",
  "distinct_idx",
  "distinct_coords",
  "x",
  "x_bioregion",
  "x_interactions",
  "x_all",
  "count_data_index",
  "pa_data_index",
  "target_covariate_names",
  "target_species",
  "bioregion_names",
  "sampling_methods"
)

check(
  all(compared$same[compared$name %in% must_match]) &&
    all(must_match %in% compared$name),
  "design matrix, design rows, and the count and presence-absence tables are identical"
)

old_image <- before$image_path

if (file.exists(old_image)) {

  old_tables <- image_tables(
    old_image,
    imputed
  )

  new_tables <- image_tables(
    new_image,
    imputed
  )

  imputation <- lapply(
    setNames(imputed, imputed),
    function(name) {
      table_differences(
        old_tables[[name]],
        new_tables[[name]]
      )
    }
  ) |>
    bind_rows(.id = "table")

  rm(old_tables, new_tables)

  print(imputation)

  check(
    all(imputation$column %in% c("sampling_method", "sampling_method_id")) &&
      all(imputation$data_types %in% c("bg", NA)),
    "the four tables that differ do so only in the sampling method drawn for background points"
  )

} else {

  cat(
    "skip  the old fit image is gone, so the four background tables were not compared column by column\n"
  )

}

###################
# C. cubes against pred_lambda
###################

species <- read_target("target_species")
pred_lambda <- read_target("pred_lambda")
cube_files <- read_target("abundance_cubes")

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

cat("\nC. abundance cubes\n")

check(
  all(cube_agreement[, "layers"] == 24) &&
    all(cube_agreement[, "na_mismatch"] == 0) &&
    all(cube_agreement[, "max_relative_difference"] < 1e-4),
  sprintf(
    "one cube per species, 24 months each; the last-12-month mean equals pred_lambda (largest relative difference %.1e)",
    max(cube_agreement[, "max_relative_difference"])
  ),
  cube_agreement
)

###################
# D. old against new results (a report, not a test)
###################

cat("\nD. old against new results: expected to differ by MCMC noise and the longer warm-up\n")

preds <- read_target("preds")

old_p_file <- old_file("rep_cx_p\\.tif$")
old_draws_file <- old_file("coef_draws_rep_cx\\.rds$")

results <- list()

if (file.exists(old_p_file)) {

  old_p <- terra::values(terra::rast(old_p_file))
  new_p <- terra::values(terra::rast(preds$p))

  results$probability <- tibble(
    species = species,
    correlation = vapply(
      species,
      function(sp) {
        cor(
          old_p[, sp],
          new_p[, sp],
          use = "complete.obs"
        )
      },
      numeric(1)
    ),
    mean_abs_difference = colMeans(
      abs(old_p[, species] - new_p[, species]),
      na.rm = TRUE
    ),
    max_abs_difference = apply(
      abs(old_p[, species] - new_p[, species]),
      2,
      max,
      na.rm = TRUE
    )
  )

  cat("\nprobability of occurrence, unmasked, old fit against new, by species:\n")

  results$probability |>
    mutate(
      across(
        where(is.numeric),
        \(x) round(x, 4)
      )
    ) |>
    print(n = 18)

} else {

  cat("skip  the old prediction rasters are gone\n")

}

if (file.exists(old_draws_file)) {

  old_draws <- readRDS(old_draws_file)
  new_draws <- readRDS(read_target("coef_draws"))

  posterior_mean <- function(draws, parameter) {
    as.vector(
      apply(
        draws[[parameter]],
        seq_along(dim(draws[[parameter]]))[-1],
        mean
      )
    )
  }

  results$coefficients <- tibble(
    parameter = c("alpha", "beta"),
    n = c(
      length(posterior_mean(new_draws, "alpha")),
      length(posterior_mean(new_draws, "beta"))
    ),
    correlation_of_posterior_means = c(
      cor(
        posterior_mean(old_draws, "alpha"),
        posterior_mean(new_draws, "alpha")
      ),
      cor(
        posterior_mean(old_draws, "beta"),
        posterior_mean(new_draws, "beta")
      )
    )
  )

  cat("\nposterior means of the coefficients, old fit against new:\n")

  print(results$coefficients)

  convergence_row <- function(draws, fit) {
    tibble(
      fit = fit,
      chains = draws$convergence$n_chains,
      samples = draws$convergence$n_iter,
      max_rhat_gate = draws$convergence$max_rhat_gate,
      max_rhat_beta = draws$convergence$beta_max_rhat,
      min_ess_gate = draws$convergence$min_ess_gate,
      min_ess_beta = draws$convergence$beta_min_ess
    )
  }

  results$convergence <- bind_rows(
    convergence_row(old_draws, "old"),
    convergence_row(new_draws, "new")
  )

  cat("\nconvergence recorded with the draws:\n")

  print(results$convergence)

} else {

  cat("skip  the old coefficient draws are gone\n")

}

###################
# save, and refresh the duplicates list
###################

if (store == "_targets") {

  saveRDS(
    list(
      checked = Sys.time(),
      inputs = inputs,
      image_objects = image$objects,
      cube_agreement = cube_agreement,
      results = results,
      failures = failures
    ),
    file.path(
      outdir,
      "compare_after_run.rds"
    )
  )

  source("extras/overhaul_checks/list_duplicate_outputs.R")

}

if (length(failures)) {
  cat(
    sprintf(
      "\n%s CHECK(S) FAILED\n",
      length(failures)
    )
  )
  quit(status = 1)
}

cat("\nall checks passed (section D is a report: read it)\n")
