# Fit one cross-validation fold, in its own process, and return a compact draws file.
#
# WHY A SUBPROCESS. greta holds TensorFlow state for the life of the session and does not
# give it back between fits. Five sequential fits in one R process will not survive 16 GB;
# this is the same reason extras/sre_lmax_sweep.sh runs each configuration through a fresh
# `Rscript` rather than looping in-session. targets runs the whole pipeline in one callr
# process, so branches share it -- the isolation has to come from here.
#
# WHY THE SEED IS PASSED IN. A callr child starts with a fresh RNG, and targets' per-target
# seed does not reach it. Two things in the fit are random and unseeded from outside: the
# background sampling-method imputation (fit_model_multispecies_pp_count.R:134) and greta's MCMC
# initialisation. `set.seed()` is therefore the first statement in the child.
#
# WHY THE PRODUCTION FIT FUNCTION IS CALLED UNMODIFIED. Folds are assigned by coordinate,
# so a fold's training frame is a row subset in which every retained coordinate keeps all
# of its records. `distinct_idx` then picks the same physical row per coordinate and
# `model_date` as it does on the full data, and the design matrix and offsets are
# bit-identical. See cv_designmat().
#
# Returns the path to the draws .rds, NOT to the fit image. `format = "file"` hashes what
# it is given, and the image is ~1.6 GB per fold at these settings; the image path is
# recorded inside the .rds instead.
fit_cv_fold <- function(
    model_data_spatial,
    cv_folds,
    fold,
    target_covariate_names,
    target_species,
    bioregion_names,
    n_burnin = 1000,
    n_samples = 1000,
    n_chains = 10,
    n_cores = 8,
    n_keep = 1000,
    seed = 20260909,
    output_dir = "outputs/cv/folds",
    keep_image = TRUE,
    overwrite = FALSE
){

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  out_path <- file.path(
    output_dir,
    sprintf("cv_fold_%02i_draws.rds", fold)
  )

  image_path <- file.path(
    output_dir,
    sprintf("cv_fold_%02i.RData", fold)
  )

  cv_folds_path <- if (is.character(cv_folds)) {
    grep("cv_folds\\.csv$", cv_folds, value = TRUE)
  } else {
    cv_folds
  }

  fingerprint <- cv_fold_fingerprint(
    model_data_spatial = model_data_spatial,
    cv_folds_path = cv_folds_path,
    fold = fold,
    target_covariate_names = target_covariate_names,
    target_species = target_species,
    bioregion_names = bioregion_names,
    n_burnin = n_burnin,
    n_samples = n_samples,
    n_chains = n_chains,
    n_keep = n_keep,
    seed = seed
  )

  # resumable: an interrupted five-fold run picks up at the fold it stopped on rather than
  # refitting hours of completed work, matching download_mu_hfp() / prepare_esa_landcover().
  # A file is reused only if it was fitted to these same inputs. Without that check a
  # change in the data would rerun this target, hand back the old file unchanged, and
  # leave every score downstream of it stale.
  if (file.exists(out_path) && !overwrite) {

    existing_fingerprint <- readRDS(out_path)$fingerprint

    if (identical(existing_fingerprint, fingerprint)) {
      return(out_path)
    }

  }

  fold_seed <- seed + fold

  started <- Sys.time()

  callr::r(
    func = function(
      model_data_spatial,
      cv_folds_path,
      fold,
      target_covariate_names,
      target_species,
      bioregion_names,
      n_burnin,
      n_samples,
      n_chains,
      n_cores,
      n_keep,
      fold_seed,
      image_path,
      out_path,
      keep_image
    ){

      # FIRST statement: covers the bg sampling-method imputation and greta's inits
      set.seed(fold_seed)

      suppressMessages({
        library(dplyr)
        library(tidyr)
        library(greta)
      })

      targets::tar_source(files = "R")

      split <- split_cv_fold(
        model_data_spatial = model_data_spatial,
        cv_folds = cv_folds_path,
        fold = fold,
        target_species = target_species
      )

      fit_model_multispecies_pp_count(
        model_data_spatial = split$train,
        target_covariate_names = target_covariate_names,
        target_species = target_species,
        bioregion_names = bioregion_names,
        image_name = image_path,
        n_burnin = n_burnin,
        n_samples = n_samples,
        n_chains = n_chains,
        n_cores = n_cores
      )

      extract_cv_draws(
        image_name = image_path,
        output_file = out_path,
        n_keep = n_keep,
        fold = fold,
        seed = fold_seed,
        delete_image = !keep_image
      )

    },
    args = list(
      model_data_spatial = model_data_spatial,
      cv_folds_path = cv_folds_path,
      fold = fold,
      target_covariate_names = target_covariate_names,
      target_species = target_species,
      bioregion_names = bioregion_names,
      n_burnin = n_burnin,
      n_samples = n_samples,
      n_chains = n_chains,
      n_cores = n_cores,
      n_keep = n_keep,
      fold_seed = fold_seed,
      image_path = image_path,
      out_path = out_path,
      keep_image = keep_image
    ),
    libpath = .libPaths(),
    wd = getwd(),
    show = TRUE
  )

  if (!file.exists(out_path)) {
    stop(
      sprintf(
        "fit_cv_fold(): fold %s returned without writing %s",
        fold,
        out_path
      ),
      call. = FALSE
    )
  }

  # runtime is worth keeping next to the convergence numbers -- a fold that finished
  # suspiciously fast usually failed to adapt rather than mixed well
  draws <- readRDS(out_path)

  draws$runtime_min <- as.numeric(
    difftime(
      Sys.time(),
      started,
      units = "mins"
    )
  )

  draws$n_train <- nrow(model_data_spatial)

  draws$fingerprint <- fingerprint

  saveRDS(
    draws,
    out_path
  )

  out_path

}

# Everything a fold's draws depend on, as one hash: the data, the folds, the fold, the
# fit settings, the seed and the fit function's code. fit_cv_fold() stores it in the draws
# file and reuses an existing file only when it matches. `n_cores` is left out: it changes
# how long the fit takes, not what it is fitted to.
cv_fold_fingerprint <- function(
    model_data_spatial,
    cv_folds_path,
    fold,
    target_covariate_names,
    target_species,
    bioregion_names,
    n_burnin,
    n_samples,
    n_chains,
    n_keep,
    seed
){

  rlang::hash(
    list(
      model_data_spatial = model_data_spatial,
      cv_folds = if (is.character(cv_folds_path)) {
        unname(tools::md5sum(cv_folds_path))
      } else {
        cv_folds_path
      },
      fold = fold,
      target_covariate_names = target_covariate_names,
      target_species = target_species,
      bioregion_names = bioregion_names,
      n_burnin = n_burnin,
      n_samples = n_samples,
      n_chains = n_chains,
      n_keep = n_keep,
      seed = seed,
      fit_function = deparse(
        body(fit_model_multispecies_pp_count)
      )
    )
  )

}
