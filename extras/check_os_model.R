# Checks of the offset-strength ("os") model and its comparison functions, on tiny fits
# of the real data (2026-10-19).
#
#   Rscript extras/check_os_model.R fit <seed> <image>
#       tiny os fit (2 chains, 20 + 20) on tar_read(model_data_spatial_cx)
#   Rscript extras/check_os_model.R check <image_os> <image_current> <outdir>
#       <image_current> is a tiny fit of the current model, e.g. from
#       `Rscript extras/check_bg_method_average.R fit 101 <image>`
#
# Run from the project root. Reads the store read-only; writes only to the paths given.

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

failures <- character()
ok <- function(cond, msg) {
  cat(sprintf("%-4s %s\n", if (isTRUE(cond)) "ok" else "FAIL", msg))
  if (!isTRUE(cond)) failures <<- c(failures, msg)
}

rel_err <- function(a, b) {
  max(abs(a - b) / pmax(abs(b), 1e-12))
}

if (mode == "fit") {

  seed <- as.integer(args[2])
  image <- args[3]

  set.seed(seed)
  t0 <- Sys.time()

  os_fit_model_multispecies_pp_count(
    model_data_spatial = tar_read(model_data_spatial_cx),
    target_covariate_names = tar_read(target_covariate_names),
    target_species = tar_read(target_species),
    bioregion_names = tar_read(bioregion_names),
    image_name = image,
    n_burnin = 20,
    n_samples = 20,
    n_chains = 2,
    n_cores = 2,
    kappa_scale = 0.05
  )

  cat(
    sprintf(
      "os fit, seed %i: %.1f min -> %s\n",
      seed,
      as.numeric(difftime(Sys.time(), t0, units = "mins")),
      image
    )
  )

}

if (mode == "check") {

  image_os <- args[2]
  image_current <- args[3]
  outdir <- args[4]

  dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

  dat <- tar_read(model_data_spatial_cx)
  target_species <- tar_read(target_species)

  os <- new.env()
  load(image_os, envir = os)

  ##########
  cat("\n1. kappa enters log lambda as kappa[s] * log(offset)\n")

  log_offset_r <- log(os$model_data_spatial$offset[os$distinct_idx])

  v <- calculate(
    os$log_lambda,
    os$log_lambda_larval_habitat,
    os$kappa,
    values = os$draws,
    nsim = 3
  )

  err <- 0
  for (i in 1:3) {
    diff_i <- v[[1]][i, , ] - v[[2]][i, , ]
    expect_i <- outer(log_offset_r, v[[3]][i, , 1])
    err <- max(err, abs(diff_i - expect_i))
  }
  ok(err < 1e-10, sprintf("log_lambda - habitat = log(offset) x kappa, max error %.2g", err))

  # at log_kappa = 0 the model is the current one: habitat + log(offset), as
  # fit_model_multispecies_pp_count() builds it with sweep()
  d1 <- as.matrix(os$draws[[1]])[1, ]
  pick_draw <- function(name, dims) {
    array(d1[grepl(sprintf("^%s\\[", name), names(d1))], dim = dims)
  }
  alpha_1 <- pick_draw("alpha", os$n_species)
  beta_raw_1 <- pick_draw("beta_raw", c(os$n_cov_abund_all, os$n_species))

  # calculate() looks the names in `values` up where it is called, so call it inside
  # the image's environment
  os$values_test <- list(
    alpha = alpha_1,
    beta_raw = beta_raw_1,
    log_kappa = rep(0, os$n_species)
  )
  ll0 <- evalq(
    calculate(
      log_lambda,
      values = values_test
    )[[1]],
    envir = os
  )

  habitat_1 <- os$x_all %*% sweep(beta_raw_1, 1, os$beta_scale, "*")
  habitat_1 <- sweep(habitat_1, 2, alpha_1, "+")
  ll_current <- sweep(habitat_1, 1, log_offset_r, "+")
  ok(
    max(abs(ll0 - ll_current)) < 1e-8,
    sprintf("at log_kappa = 0, log_lambda is the current model's, max error %.2g", max(abs(ll0 - ll_current)))
  )

  ok(
    any(grepl("^log_kappa\\[", colnames(as.matrix(os$draws[[1]])))),
    "log_kappa is in the draws"
  )

  ##########
  cat("\n2. os_extract_cv_draws()\n")

  draws_os <- file.path(outdir, "draws_os.rds")
  draws_current <- file.path(outdir, "draws_current.rds")

  os_extract_cv_draws(
    image_name = image_os,
    output_file = draws_os,
    n_keep = 40
  )
  extract_cv_draws(
    image_name = image_current,
    output_file = draws_current,
    n_keep = 40
  )

  d_os <- readRDS(draws_os)
  d_cur <- readRDS(draws_current)
  ok(
    identical(dim(d_os$kappa), c(40L, 18L)) && isTRUE(all.equal(d_os$kappa, exp(d_os$log_kappa))),
    "kappa is [40 x 18] and equals exp(log_kappa)"
  )
  ok(
    setequal(setdiff(names(d_os), names(d_cur)), c("log_kappa", "kappa", "kappa_scale")),
    "the os draws add only log_kappa, kappa and kappa_scale"
  )
  ok(identical(d_os$kappa_scale, 0.05), "kappa_scale stored as 0.05")

  ##########
  cat("\n3. os_record_loglik() reproduces greta's expected values\n")

  for (which_fit in c("os", "current")) {

    image <- if (which_fit == "os") image_os else image_current
    draws_file <- if (which_fit == "os") draws_os else draws_current

    e <- new.env()
    load(image, envir = e)

    rl <- os_record_loglik(
      draws_file = draws_file,
      model_data_spatial = dat,
      return_expected = TRUE
    )

    # greta's values at every posterior sample, in chain order. One target per call:
    # given an mcmc.list, calculate() binds several targets' columns into one
    g <- lapply(
      list(
        e$count_data_response_expected,
        e$pa_data_response_expected,
        e$po_data_response_expected
      ),
      function(target) calculate(target, values = e$draws)
    )

    to_mat <- function(x) do.call(rbind, lapply(x, as.matrix))

    # the draws file keeps all 20 x 2 samples here, in the same order
    stopifnot(length(rl$chain) == 40)

    errs <- c(
      count = rel_err(t(rl$count$expected), to_mat(g[[1]])),
      pobg = rel_err(t(rl$pobg$expected), to_mat(g[[3]]))
    )

    for (dt in names(errs)) {
      ok(
        errs[[dt]] < 1e-8,
        sprintf("%s fit, %s: max relative error %.2g", which_fit, dt, errs[[dt]])
      )
    }

    # presence-absence: the fit computes p as ilogit(log1p(-exp(-lambda)) + lambda)
    # (R/logit_icloglog_safe.R), which loses precision as lambda -> 0 (relative error
    # ~ 1e-16 / lambda); os_record_loglik() uses -expm1(-lambda), which does not. So
    # compare relatively where p > 1e-8, and absolutely everywhere
    pa_mine <- t(rl$pa$expected)
    pa_greta <- to_mat(g[[2]])
    big <- pa_greta > 1e-8
    err_big <- max(abs(pa_mine[big] - pa_greta[big]) / pa_greta[big])
    err_abs <- max(abs(pa_mine - pa_greta))
    ok(
      err_big < 1e-8 && err_abs < 1e-13,
      sprintf(
        "%s fit, pa: max relative error %.2g where p > 1e-8 (%.1f%% of values); max absolute error %.2g overall",
        which_fit,
        err_big,
        100 * mean(big),
        err_abs
      )
    )

    ok(
      all(is.finite(unlist(lapply(rl[c("count", "pa", "pobg")], `[[`, "loglik")))),
      sprintf("%s fit: every log-likelihood finite", which_fit)
    )

    ok(
      nrow(rl$pobg$records) == sum(dat$data_type == "po") + 18 * sum(dat$data_type == "bg"),
      sprintf("%s fit: PO + bg records = PO + 18 x bg (%i)", which_fit, nrow(rl$pobg$records))
    )

    rm(e)
    gc(verbose = FALSE)

  }

  ##########
  cat("\n4. predictions\n")

  offsets_5 <- tar_read(offsets_5)
  offsets_avg_10 <- tar_read(offsets_avg_10)
  offsets_2024_10 <- aggregate(
    offsets_5[[289:300]],
    fact = 2,
    fun = "mean",
    cores = 4,
    na.rm = TRUE
  )
  m <- values(mean(offsets_2024_10))[, 1]
  a <- values(offsets_avg_10)[, 1]
  both <- !is.na(m) & !is.na(a)
  ok(
    max(abs(m[both] - a[both]) / pmax(a[both], 1e-12)) < 1e-6 && all(is.na(m) == is.na(a)),
    sprintf(
      "the 2024 monthly offsets average to offsets_avg_10 (max relative error %.2g, NA cells %s)",
      max(abs(m[both] - a[both]) / pmax(a[both], 1e-12)),
      if (all(is.na(m) == is.na(a))) "identical" else "differ"
    )
  )

  covariate_rast_10 <- tar_read(covariate_rast_10)

  set.seed(7)
  pr <- os_predict_lambda(
    image_name = image_os,
    prediction_layer = covariate_rast_10,
    target_species = target_species,
    output_file_prefix = file.path(outdir, "pred_os"),
    offset = offsets_2024_10,
    sm = TRUE,
    nsims = 10
  )
  ok(identical(names(pr), c("lambda", "p", "p_cv")), "os_predict_lambda() returns lambda, p, p_cv")

  # an independent reconstruction at 200 cells, from the same parameter draws
  set.seed(7)
  pd <- calculate(
    os$beta,
    os$alpha,
    os$sampling_re,
    os$log_kappa,
    values = os$draws,
    nsim = 10
  )
  cv <- values(covariate_rast_10)
  cells <- sample(which(!is.na(cv[, 1]) & !is.na(values(offsets_2024_10)[, 1])), 200)
  x <- cv[cells, os$target_covariate_names]
  X <- cbind(x, make_designmat_interactions(x, cv[cells, os$bioregion_names]))
  lo <- log(values(offsets_2024_10)[cells, ])
  sm_idx <- match("human_landing_catch_ind", os$sampling_methods)
  p_r <- rast(pr$p)
  l_r <- rast(pr$lambda)
  err_p <- 0
  err_l <- 0
  # calculate() names its output after the expressions passed, so index by position
  names(pd) <- c("beta", "alpha", "sampling_re", "log_kappa")
  for (s in c(1, 5, 12, 18)) {
    eta <- X %*% t(pd$beta[, , s])
    eta <- sweep(eta, 2, pd$alpha[, s, 1] + pd$sampling_re[, sm_idx, 1], "+")
    k <- exp(pd$log_kappa[, s, 1])
    eff <- Reduce(`+`, lapply(1:12, function(mm) exp(outer(lo[, mm], k)))) / 12
    lam <- exp(eta) * eff
    p_expect <- rowMeans(1 - exp(-lam))
    l_expect <- apply(lam, 1, median)
    err_p <- max(err_p, rel_err(values(p_r)[cells, s], p_expect))
    err_l <- max(err_l, rel_err(values(l_r)[cells, s], l_expect))
  }
  ok(err_p < 1e-5, sprintf("p matches an independent reconstruction (max relative error %.2g)", err_p))
  ok(err_l < 1e-5, sprintf("lambda (with offset) matches it too (max relative error %.2g)", err_l))

  ##########
  cat("\n5. the comparison functions, end to end\n")

  set.seed(7)
  pr_cur <- predict_lambda(
    image_name = image_current,
    prediction_layer = covariate_rast_10,
    target_species = target_species,
    output_file_prefix = file.path(outdir, "pred_current"),
    offset = offsets_avg_10,
    sm = TRUE,
    nsims = 10
  )

  expert <- tar_read(expert_offset_maps_10)
  bare <- tar_read(landcover_bare_10)

  pred_p_cur <- mask_landcover_and_expert_offset(
    p = rast(pr_cur$p),
    expert = expert,
    bare = bare
  )
  pred_p_os <- mask_landcover_and_expert_offset(
    p = rast(pr$p),
    expert = expert,
    bare = bare
  )
  pred_lambda_os <- mask_landcover_and_expert_offset(
    p = rast(pr$lambda),
    expert = expert,
    bare = bare
  )
  ok(nlyr(pred_lambda_os) == 18, "pred_lambda_os masks to 18 layers")

  thr_cur <- maxsss_thresholds(pred_p = pred_p_cur, model_data_spatial = dat)
  thr_os <- maxsss_thresholds(pred_p = pred_p_os, model_data_spatial = dat)

  cmp_dir <- file.path(outdir, "model_comparison")

  t0 <- Sys.time()
  loo_res <- os_compare_loo(
    draws_file_current = draws_current,
    draws_file_os = draws_os,
    model_data_spatial = dat,
    output_dir = cmp_dir
  )
  cat(sprintf("     os_compare_loo(): %.1f min\n", as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  print(loo_res$by_type)
  ok(
    nrow(loo_res$pointwise) == nrow(dat) - sum(dat$data_type == "bg") + 18 * sum(dat$data_type == "bg"),
    sprintf("LOO covers every record (%i)", nrow(loo_res$pointwise))
  )
  ok(all(is.finite(loo_res$pointwise$diff)), "every pointwise difference finite")

  maps_res <- os_compare_maps(
    pred_p = pred_p_cur,
    pred_p_os = pred_p_os,
    p_unmasked = rast(pr_cur$p),
    p_unmasked_os = rast(pr$p),
    thresholds = thr_cur,
    thresholds_os = thr_os,
    offset = offsets_avg_10,
    model_data_records = tar_read(model_data_records),
    target_species = target_species,
    output_dir = cmp_dir
  )
  print(maps_res$presence_cells, n = 5)
  ok(
    all(c("presence_cells.csv", "range_by_offset.csv", "unmasked_low_offset.csv") %in% list.files(cmp_dir)),
    "os_compare_maps() writes its three CSVs"
  )

  kfiles <- os_plot_kappa(
    draws_file = draws_os,
    output_dir = cmp_dir
  )
  ok(all(file.exists(kfiles)), "os_plot_kappa() writes its table and figures")

  rfiles <- os_plot_range_change(
    pred_p = pred_p_cur,
    pred_p_os = pred_p_os,
    thresholds = thr_cur,
    thresholds_os = thr_os,
    model_data_records = tar_read(model_data_records),
    target_species = c("arabiensis", "ziemanni"),
    output_dir = cmp_dir
  )
  ok(all(file.exists(rfiles)), "os_plot_range_change() writes one figure per species")

  cat(
    if (length(failures)) sprintf("\n%i CHECK(S) FAILED\n", length(failures)) else "\nall checks passed\n"
  )

}
