# The scratch pipeline behind 03_smoke_pipeline.R. Not run directly, and not
# the project pipeline: it has its own store and writes only to
# OVERHAUL_SMOKE_DIR.
#
# It is the model core of the real _targets.R, taken from the real target
# objects so that it is the real wiring that gets exercised. Only three kinds
# of thing are replaced:
#   - the inputs, which are read from the main store instead of being rebuilt;
#   - the fit settings, cut down to 2 chains of a few iterations;
#   - the output paths, moved into the scratch folder.

library(targets)

main_store <- Sys.getenv("OVERHAUL_MAIN_STORE")
smoke_dir <- Sys.getenv("OVERHAUL_SMOKE_DIR")
smoke_samples <- as.integer(Sys.getenv("OVERHAUL_SMOKE_SAMPLES"))

# the real pipeline: this also sets its options and sources R/
real <- source("_targets.R")$value

names(real) <- vapply(
  real,
  function(target) target$settings$name,
  character(1)
)

# a target with the real one's name and storage format but another command
with_command <- function(name, command) {
  tar_target_raw(
    name = name,
    command = command,
    format = real[[name]]$settings$format
  )
}

real_command <- function(name) {
  real[[name]]$command$expr[[1]]
}

###################
# inputs, from the main store
###################

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

inputs <- lapply(
  input_names,
  function(name) {
    with_command(
      name,
      bquote(
        targets::tar_read_raw(
          .(name),
          store = .(main_store)
        )
      )
    )
  }
)

###################
# paths and fit settings
###################

fit_command <- real_command("model_fit")
fit_command$n_burnin <- 20
fit_command$n_samples <- smoke_samples
fit_command$n_chains <- 2
fit_command$n_cores <- 2

draws_command <- real_command("coef_draws")
draws_command$output_file <- file.path(smoke_dir, "coef_draws.rds")

cubes_command <- real_command("abundance_cubes")
cubes_command$write_dir <- file.path(smoke_dir, "abundance_cubes_masked")

replaced <- list(
  with_command(
    "fit_image",
    file.path(smoke_dir, "model_fit.RData")
  ),
  with_command(
    "pred_prefix",
    file.path(smoke_dir, "multispecies_pp")
  ),
  with_command(
    "plot_dir",
    file.path(smoke_dir, "plots")
  ),
  with_command("model_fit", fit_command),
  with_command("coef_draws", draws_command),
  with_command("abundance_cubes", cubes_command)
)

###################
# the rest of the model core, exactly as in _targets.R
###################

as_defined <- real[
  c(
    "model_fit_image",
    "preds",
    "pred_dist_not_masked",
    "pred_p",
    "pred_lambda",
    "pred_cv",
    "thresholds_maxsss",
    "pred_binary",
    "pred_dominant",
    "pred_dominant_001",
    "expert_maps_buffer_1000",
    "pred_dominant_expert",
    "pred_dominant_001_expert",
    "plot_pred_dominant_expert",
    "rel_abund_rgb"
  )
]

c(
  inputs,
  replaced,
  unname(as_defined)
)
