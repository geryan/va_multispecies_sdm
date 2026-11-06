# Snapshot of the pipeline and of the _cx data, taken BEFORE the 2026-10-14
# overhaul of _targets.R (one model set, plain names). The later checks in this
# folder compare against it.
#
# Read-only: nothing in the store or in outputs/ changes, apart from the
# snapshot itself in outputs/overhaul_check/. Run from the repo root, on the
# pre-overhaul _targets.R (commit 18d5acd):
#   Rscript extras/overhaul_checks/01_snapshot_before.R
#
# Loads the 6.8 GB fit image once, so allow a few minutes and ~10 GB of memory.

library(targets)
library(dplyr)

source("extras/overhaul_checks/helpers.R")

outdir <- "outputs/overhaul_check"

dir.create(
  outdir,
  recursive = TRUE,
  showWarnings = FALSE
)

###################
# the pipeline as defined and as stored
###################

manifest <- tar_manifest(
  fields = tidyselect::any_of(
    c("name", "command", "pattern", "format")
  )
)

network <- tar_network(
  targets_only = TRUE,
  outdated = FALSE
)

meta <- tar_meta()

outdated <- tar_outdated()

###################
# the _cx output files
###################

cx_files <- c(
  "outputs/images/model_fit_test_source_re_rep_cx.RData",
  "outputs/rasters/reparam_multispecies_pp_rep_cx.tif",
  "outputs/rasters/reparam_multispecies_pp_rep_cx_p.tif",
  "outputs/rasters/reparam_multispecies_pp_rep_cx_p_cv.tif",
  "outputs/draws/coef_draws_rep_cx.rds",
  list.files(
    "outputs/rasters/countries_rep_cx",
    recursive = TRUE,
    full.names = TRUE
  )
)

files <- tibble(
  path = cx_files,
  bytes = file.size(cx_files),
  modified = file.mtime(cx_files),
  md5 = unname(tools::md5sum(cx_files))
)

###################
# the data that reached the model, from inside the fit image
###################

image_path <- cx_files[1]

fit <- new.env(parent = emptyenv())

load(
  image_path,
  envir = fit
)

image <- digest_image(fit)

# the model data the fit saw against the stored target it was built from
stored_model_data <- tar_read(model_data_spatial_cx)

image$model_data_matches_store <- identical(
  fit$model_data_spatial,
  stored_model_data
)

image$stored_model_data_hash <- rlang::hash(stored_model_data)

rm(fit)

###################
# save
###################

snapshot <- list(
  taken = Sys.time(),
  git_head = system(
    "git rev-parse HEAD",
    intern = TRUE
  ),
  manifest = manifest,
  edges = network$edges,
  vertices = network$vertices,
  meta = meta,
  outdated = outdated,
  files = files,
  image_path = image_path,
  image = image
)

saveRDS(
  snapshot,
  file.path(
    outdir,
    "snapshot_before.rds"
  )
)

cat(
  sprintf(
    "targets: %s | edges: %s | outdated: %s | files hashed: %s\n",
    nrow(manifest),
    nrow(network$edges),
    length(outdated),
    nrow(files)
  )
)

cat(
  sprintf(
    "image objects: %s, of which hashed: %s\n",
    nrow(image$objects),
    sum(!is.na(image$objects$hash))
  )
)

cat(
  "model data in the image identical to stored model_data_spatial_cx:",
  image$model_data_matches_store,
  "\n"
)
