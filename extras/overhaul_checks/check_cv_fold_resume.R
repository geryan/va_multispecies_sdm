# Does fit_cv_fold() reuse an existing fold file only when it was fitted to the
# same inputs? Tests the resume guard without fitting anything.
#
# A fold file is faked in a temporary folder with the fingerprint of one set of
# inputs. Asking for that fold with the same inputs must hand the file back
# untouched. Asking with anything changed must go on to refit: here the refit
# is made to fail at once, on a folds file that is not a real one, so an error
# is the sign that the old file was not reused. The fit settings are one chain
# of one iteration, so that nothing here could turn into a real fit.
#
# Reads the stored model data; writes only to a temporary folder. Run from the
# repo root:
#   Rscript extras/overhaul_checks/check_cv_fold_resume.R

library(targets)
library(dplyr)

tar_source(files = "R")

failures <- character()

check <- function(ok, label) {

  cat(
    sprintf(
      "%s  %s\n",
      if (ok) "ok  " else "FAIL",
      label
    )
  )

  if (!ok) {
    failures <<- c(failures, label)
  }

  invisible(ok)

}

fold_dir <- file.path(tempdir(), "cv_resume_check")

dir.create(
  fold_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

folds_file <- file.path(fold_dir, "cv_folds.csv")

writeLines(
  "not,a,real,folds,file",
  folds_file
)

inputs <- list(
  model_data_spatial = tar_read(model_data_spatial_cx),
  cv_folds = folds_file,
  fold = 1L,
  target_covariate_names = tar_read(target_covariate_names),
  target_species = tar_read(target_species),
  bioregion_names = tar_read(bioregion_names),
  n_burnin = 1,
  n_samples = 1,
  n_chains = 1,
  n_keep = 1,
  seed = tar_read(seed),
  output_dir = fold_dir
)

fold_file <- file.path(fold_dir, "cv_fold_01_draws.rds")

fake_fold_file <- function(fingerprint) {
  saveRDS(
    list(
      placeholder = "not real draws",
      fingerprint = fingerprint
    ),
    fold_file
  )
}

# TRUE if the existing file was handed back, FALSE if a refit was attempted
reused <- function(inputs) {

  written <- file.mtime(fold_file)

  result <- tryCatch(
    do.call(fit_cv_fold, inputs),
    error = function(e) NULL
  )

  identical(result, fold_file) && identical(file.mtime(fold_file), written)

}

fake_fold_file(
  cv_fold_fingerprint(
    model_data_spatial = inputs$model_data_spatial,
    cv_folds_path = inputs$cv_folds,
    fold = inputs$fold,
    target_covariate_names = inputs$target_covariate_names,
    target_species = inputs$target_species,
    bioregion_names = inputs$bioregion_names,
    n_burnin = inputs$n_burnin,
    n_samples = inputs$n_samples,
    n_chains = inputs$n_chains,
    n_keep = inputs$n_keep,
    seed = inputs$seed
  )
)

check(
  reused(inputs),
  "same inputs: the existing fold file is reused"
)

more_cores <- inputs
more_cores$n_cores <- 2

check(
  reused(more_cores),
  "a different number of cores: still reused"
)

one_row_fewer <- inputs
one_row_fewer$model_data_spatial <- inputs$model_data_spatial[-1, ]

check(
  !reused(one_row_fewer),
  "one row dropped from the data: not reused"
)

more_chains <- inputs
more_chains$n_chains <- 2

check(
  !reused(more_chains),
  "a different number of chains: not reused"
)

writeLines(
  "a,different,folds,file",
  folds_file
)

check(
  !reused(inputs),
  "the folds file changed: not reused"
)

writeLines(
  "not,a,real,folds,file",
  folds_file
)

check(
  reused(inputs),
  "the folds file restored: reused again"
)

fake_fold_file(NULL)

check(
  !reused(inputs),
  "a fold file with no fingerprint, as written before 2026-10-14: not reused"
)

unlink(
  fold_dir,
  recursive = TRUE
)

if (length(failures)) {
  cat(
    sprintf(
      "\n%s CHECK(S) FAILED\n",
      length(failures)
    )
  )
  quit(status = 1)
}

cat("\nall resume checks passed\n")
