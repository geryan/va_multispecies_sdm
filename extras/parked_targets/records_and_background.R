# Parked targets: Alternative record and background-point targets.
#
# Moved verbatim, still commented out, from _targets.R on 2026-11-24, so the
# pipeline file stays short. Nothing here runs: tar_source() reads only R/.
# Each block left a pointer comment where it was in _targets.R; to restore
# one, paste it back at its pointer and uncomment it.
#
# Blocks:
#   1. Background points by k-means in environmental space (was lines
#      1300-1307, just before `bg_kmeans_list_spatial`)
#   2. Records without inferred zeros; records plus background (was lines
#      1480-1503, just before `record_data_spatial_all`)


# ---------------------------------------------------------------------------
# 1. Background points by k-means in environmental space
# ---------------------------------------------------------------------------
  # tar_target(
  #   bg_kmeans_list_env,
  #   bg_points_kmeans_env(
  #     n_bg,
  #     covariate_rast,
  #     n_samples_per_bg = 200
  #   )
  # ),


# ---------------------------------------------------------------------------
# 2. Records without inferred zeros; records plus background
# ---------------------------------------------------------------------------
 # tar_target(
 #   model_data_records_ni,
 #   generate_model_data_records_no_impute(
 #     full_data_records,
 #     target_species = target_species
 #   )
 # ),

 # tar_target(
 #   model_data_all,
 #   bind_rows(
 #     model_data_records,
 #     bg_points |>
 #       as_tibble() |>
 #       rename(
 #         latitude = y,
 #         longitude = x
 #       ) |>
 #       mutate(
 #         data_type = "bg",
 #         n = 0
 #       )
 #   )
 # ),
