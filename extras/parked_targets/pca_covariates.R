# Parked targets: A principal-components version of the covariates.
#
# Moved verbatim, still commented out, from _targets.R on 2026-11-24, so the
# pipeline file stays short. Nothing here runs: tar_source() reads only R/.
# Each block left a pointer comment where it was in _targets.R; to restore
# one, paste it back at its pointer and uncomment it.
#
# Blocks:
#   1. PCA covariate layers (was lines 2021-2052, just before
#      `complex_members`)


# ---------------------------------------------------------------------------
# 1. PCA covariate layers
# ---------------------------------------------------------------------------
 # ######
 # # PCA Covariate layers
 # ######
 # tar_terra_rast(
 #   pca_covariate_layers,
 #   make_pca_covariate_layers(
 #     covariate_rast_5,
 #     target_covariate_names,
 #     model_data_spatial
 #   )
 # ),
 #
 # tar_target(
 #   model_data_spatial_pca,
 #   get_spatial_values(
 #     pca_covariate_layers,
 #     model_data_spatial,
 #     project_mask_5
 #   ) |>
 #     select(
 #       - all_of(target_covariate_names),
 #     )
 # ),
 #
 # tar_target(
 #   covs_plots_pca,
 #   make_covariate_plots(
 #     model_data_spatial_pca,
 #     cvnames = names(pca_covariate_layers),
 #     fname = "outputs/figures/cov_violins_pca.png"
 #   )
 # ),
