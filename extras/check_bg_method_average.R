# Smoke test of the background sampling-method fix (2026-10-19).
#
#   Rscript extras/check_bg_method_average.R fit <seed> <image>   tiny fit (2 chains, 20 + 20)
#   Rscript extras/check_bg_method_average.R check <imageA> <imageB> <outdir>
#
# Run `fit` twice with different seeds, then `check` on the two images. About 5 min in
# all. Passed on 2026-10-19 (23 checks).
#
# Run from the project root. Reads the real store read-only; writes only to the scratch
# paths given.

args <- commandArgs(trailingOnly = TRUE)
mode <- args[1]

pkgs <- c(
  "dplyr", "sdmtools", "readr", "tidyr", "terra", "ggplot2", "geotargets",
  "idpalette", "tidyterra", "geodata", "coda", "greta", "DHARMa", "lubridate",
  "magrittr", "stringr", "bayesplot", "patchwork", "MCMCvis", "bssdm"
)

suppressPackageStartupMessages({
  library(targets)
  for (p in pkgs) library(p, character.only = TRUE)
})

tar_source(files = "R")

ok <- function(cond, msg) {
  cat(sprintf("%-4s %s\n", if (isTRUE(cond)) "ok" else "FAIL", msg))
  if (!isTRUE(cond)) failures <<- c(failures, msg)
}
failures <- character()

if (mode == "fit") {

  seed <- as.integer(args[2])
  image <- args[3]

  set.seed(seed)

  t0 <- Sys.time()

  fit_model_multispecies_pp_count(
    model_data_spatial = tar_read(model_data_spatial_cx),
    target_covariate_names = tar_read(target_covariate_names),
    target_species = tar_read(target_species),
    bioregion_names = tar_read(bioregion_names),
    image_name = image,
    n_burnin = 20,
    n_samples = 20,
    n_chains = 2,
    n_cores = 2
  )

  cat(
    sprintf(
      "fit, seed %i: %.1f min -> %s\n",
      seed,
      as.numeric(difftime(Sys.time(), t0, units = "mins")),
      image
    )
  )

}

if (mode == "check") {

  image_a <- args[2]
  image_b <- args[3]
  outdir <- args[4]

  a <- new.env()
  b <- new.env()
  load(image_a, envir = a)
  load(image_b, envir = b)

  cat("\n1. the data the model sees no longer depend on the seed\n")

  for (obj in c("model_data_spatial_bg", "bg_data", "model_data", "pobg_data_index", "pobg_sampling_idx")) {
    ok(
      identical(a[[obj]], b[[obj]]),
      sprintf("%s identical across seeds", obj)
    )
  }

  cat("\n2. background rows have no method and index the averaged effect\n")

  bg_rows <- a$model_data_spatial_bg$data_type == "bg"
  ok(all(is.na(a$model_data_spatial_bg$sampling_method[bg_rows])), "bg rows: sampling_method is NA")
  ok(
    all(!is.na(a$model_data_spatial_bg$sampling_method_id[!bg_rows])),
    "every other record has a method"
  )
  ok(
    all(a$pobg_sampling_idx[a$pobg_is_bg] == a$n_sampling_methods + 1),
    sprintf("bg rows (%i) index element %i", sum(a$pobg_is_bg), a$n_sampling_methods + 1)
  )
  ok(
    identical(
      as.integer(a$pobg_sampling_idx[!a$pobg_is_bg]),
      as.integer(a$pobg_data_index$sampling_method_id[!a$pobg_is_bg])
    ),
    sprintf("PO rows (%i) keep their own method", sum(!a$pobg_is_bg))
  )
  ok(
    sum(a$pobg_is_bg) == 249 * length(a$target_species),
    "bg rows = 249 points x 18 species"
  )
  ok(isTRUE(all.equal(sum(a$sm_prop), 1)), "method shares sum to 1")

  cat("\n3. the built model computes the averaged effect exactly\n")

  # the method term on each PO/bg row, as the model computes it, at a few posterior draws
  term <- with(
    a,
    log_lambda_obs_pobg - log_lambda[pobg_data_loc_sp_idx]
  )

  vals <- calculate(
    term,
    a$sampling_re,
    a$sampling_re_bg,
    values = a$draws,
    nsim = 5
  )

  p <- as.numeric(a$sm_prop)
  max_err_bg <- 0
  max_err_po <- 0
  max_err_scalar <- 0

  for (i in 1:5) {
    sre <- vals[[2]][i, , 1]
    t_i <- vals[[1]][i, , 1]
    expect_bg <- log(sum(p * exp(sre)))
    max_err_scalar <- max(max_err_scalar, abs(vals[[3]][i, , 1] - expect_bg))
    max_err_bg <- max(max_err_bg, abs(t_i[a$pobg_is_bg] - expect_bg))
    max_err_po <- max(
      max_err_po,
      abs(t_i[!a$pobg_is_bg] - sre[a$pobg_data_index$sampling_method_id[!a$pobg_is_bg]])
    )
  }

  ok(max_err_scalar < 1e-8, sprintf("sampling_re_bg = log(sum(p * exp(sampling_re))), max error %.2g", max_err_scalar))
  ok(max_err_bg < 1e-8, sprintf("every bg row gets it, max error %.2g", max_err_bg))
  ok(max_err_po < 1e-8, sprintf("every PO row gets its own method's effect, max error %.2g", max_err_po))

  # the same as copying each bg point once per method with weight w * p_m: the bg rows'
  # Poisson log-likelihood (n = 0, so -mu) summed both ways at one draw
  mu_bg <- calculate(
    a$po_data_response_expected,
    a$sampling_re,
    values = a$draws,
    nsim = 1
  )
  mu <- mu_bg[[1]][1, , 1][a$pobg_is_bg]
  sre1 <- mu_bg[[2]][1, , 1]
  base <- mu / sum(p * exp(sre1)) # w * lambda * bias without the method term
  replicated <- sum(outer(base, p * exp(sre1)))
  ok(
    isTRUE(all.equal(sum(mu), replicated, tolerance = 1e-10)),
    sprintf("bg log-likelihood equals the one-row-per-method version (%.6g vs %.6g)", -sum(mu), -replicated)
  )

  cat("\n4. downstream functions on the tiny fit\n")

  draws_file <- file.path(outdir, "draws_a.rds")
  extract_cv_draws(
    image_name = image_a,
    output_file = draws_file,
    n_keep = 40
  )
  d <- readRDS(draws_file)
  ok(
    identical(names(d$sm_prop), d$sampling_methods) && isTRUE(all.equal(sum(d$sm_prop), 1)),
    "extract_cv_draws() stores sm_prop, named by method"
  )

  # a fake 5-fold split by longitude band, on the real coordinates
  dat <- tar_read(model_data_spatial_cx)
  coords <- dat |>
    distinct(latitude, longitude) |>
    mutate(
      coord_key = cv_coord_key(latitude, longitude),
      fold = as.integer(cut(longitude, 5))
    )

  pr <- predict_cv_heldout(
    draws_file = draws_file,
    model_data_spatial = dat,
    cv_folds = coords,
    fold = 3,
    nsims_ppc = 10
  )
  n_bg_test <- sum(dat$data_type == "bg" & cut(dat$longitude, 5, labels = FALSE) == 3)
  ok(pr$n_unseen_methods == 0, "predict_cv_heldout(): no unseen methods reported")
  ok(pr$n_records_unseen_method == 0, "predict_cv_heldout(): bg rows not counted as unseen-method records")
  ok(
    !is.null(pr$preds$po_pred) && ncol(pr$preds$po_pred) == length(pr$dat$po_dat),
    sprintf(
      "predict_cv_heldout(): PO/bg predictions for %i rows (%i held-out bg points x 18)",
      length(pr$dat$po_dat),
      n_bg_test
    )
  )
  ok(all(is.finite(pr$preds$po_pred)), "predict_cv_heldout(): PO/bg predictions finite")

  vdir <- file.path(outdir, "validation")
  v <- validation_and_checking(
    model_fit_image_multisp_pp_count_sm = image_a,
    nsims = 10,
    plotdir = vdir
  )
  ok(
    is.list(v) && nrow(v$rhats) == nrow(tar_read(resids_and_rhats)$rhats),
    sprintf("validation_and_checking() runs; %i R-hats, as in production", nrow(v$rhats))
  )
  ok(
    any(is.na(v$resids$sampling_method[v$resids$data_type == "bg"])),
    "validation residuals: bg rows have no method (an NA panel in the by-method plot)"
  )

  cat(
    if (length(failures)) sprintf("\n%i CHECK(S) FAILED\n", length(failures)) else "\nall checks passed\n"
  )

}
