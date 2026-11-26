# Parked targets: Spatial block cross-validation (the cv_* targets), off since
# 2026-10-19 at the user's request: no CV for now. See agent_notes/model.md.
#
# Moved verbatim, still commented out, from _targets.R on 2026-11-24, so the
# pipeline file stays short. Nothing here runs: tar_source() reads only R/.
# Each block left a pointer comment where it was in _targets.R; to restore
# one, paste it back at its pointer and uncomment it.
#
# Blocks:
#   1. Spatial block cross-validation (was lines 2960-3070, just before
#      `so_i_dont_have_to_go_backward_and_add_commas`)


# ---------------------------------------------------------------------------
# 1. Spatial block cross-validation
# ---------------------------------------------------------------------------
 ###################
 # spatial block cross-validation
 #
 # Every check on this model is in-sample. This holds out blocks of SPACE, refits, and
 # scores the held-out count records, so there is a number for how well the model predicts
 # where it has not been -- which is the only way to know whether the 187 landcover x
 # bioregion interaction columns are buying spatial structure or fitting noise.
 #
 # Two things about the construction are load-bearing and are argued at length in the
 # functions themselves:
 #
 #   - folds are assigned to the ~250 background VORONOI CELLS, and every data coordinate
 #     takes the fold of the cell it sits in. The PO/bg likelihood is a Berman-Turner
 #     quadrature whose weights tile the continent exactly, so the held-out region has to
 #     be a union of whole cells or the presences and the integral disagree along every
 #     boundary. See cv_spatial_folds().
 #
 #   - because folds are then a function of the COORDINATE alone, a fold's training frame
 #     can be handed to the unmodified production fit function as a row subset:
 #     `distinct_idx` picks the same physical row per retained coordinate as it does on the
 #     full data, so the design matrix and offsets are bit-identical. Verified for both
 #     sides of all five folds. See cv_designmat(), which is also why prediction must go
 #     through the per-COORDINATE design and not a record's own covariate row.
 #
 # `seed` is the pipeline seed defined above the background points, so the quadrature the
 # folds are cut from and the folds themselves trace back to one number.

 # tar_target(
 #   cv_tag,
 #   "20260909"
 # ),
 #
 # tar_target(
 #   cv_k,
 #   5L
 # ),
 #
 # tar_target(
 #   cv_dir,
 #   sprintf(
 #     "outputs/cv/spatial_block_%s",
 #     cv_tag
 #   )
 # ),
 #
 # # returns c(cv_folds.csv, cv_blocks.gpkg, cv_autocor.csv). The block size is the median
 # # variogram range across the model covariates AFTER dropping the fits that ran away --
 # # on the 2026-09 covariate set `tree`, `water` and `urban` all fail to reach a sill, and
 # # taking blockCV's own median over the contaminated set gives a meaningless number.
 # tar_target(
 #   cv_folds_file,
 #   cv_spatial_folds(
 #     model_data_spatial = model_data_spatial_cx,
 #     bg_kmeans_df = bg_kmeans_df,
 #     covariate_rast = covariate_rast_10,
 #     target_covariate_names = target_covariate_names,
 #     k = cv_k,
 #     seed = seed,
 #     output_dir = cv_dir
 #   ),
 #   format = "file"
 # ),
 #
 # # the branching index, in the esa_landcover_years idiom: the fold fits below map over it
 # tar_target(
 #   cv_fold_ids,
 #   seq_len(cv_k)
 # ),
 #
 # # One branch per fold, each fitted in its own callr subprocess so TensorFlow memory is
 # # released between folds -- five sequential greta fits in one session will not survive
 # # 16 GB, which is the same reason extras/sre_lmax_sweep.sh spawns a fresh Rscript per
 # # configuration. The target returns the compact draws .rds, not the ~1.6 GB fit image:
 # # `format = "file"` hashes whatever it is handed, and hashing gigabytes on every status
 # # check is not worth it. The image path is recorded inside the .rds.
 # #
 # # `cv_folds_file` is deliberately NOT in the pattern, so every branch receives all of its
 # # paths -- the same trick as esa_landcover_proportion.
 # #
 # # 10 chains rather than the production 40, and 1000 burn-in rather than 2000: a fold is
 # # then ~2 h instead of ~10 h, and five folds fit in a night. That is a deliberate
 # # approximation -- CV measures predictive skill and does not need the tails resolved to
 # # the production standard -- and each fold carries its own convergence gate so a fold that
 # # did not mix cannot quietly contribute a number.
 # tar_target(
 #   cv_fold_draws,
 #   fit_cv_fold(
 #     model_data_spatial = model_data_spatial_cx,
 #     cv_folds = cv_folds_file,
 #     fold = cv_fold_ids,
 #     target_covariate_names = target_covariate_names,
 #     target_species = target_species,
 #     bioregion_names = bioregion_names,
 #     n_burnin = 1000,
 #     n_samples = 1000,
 #     n_chains = 10,
 #     n_cores = 8,
 #     seed = seed,
 #     output_dir = file.path(cv_dir, "folds")
 #   ),
 #   pattern = map(cv_fold_ids),
 #   format = "file"
 # ),

 # STILL TO COME, as their functions are written:
 #
 #   cv_fold_metrics score_cv_counts()      pattern = map(cv_fold_draws, cv_fold_ids)
 #   cv_fold_checks  cv_predictive_checks() pattern = map(cv_fold_draws, cv_fold_ids)
 #   cv_summary      summarise_cv_metrics() not branched -- takes every fold's paths
 #   cv_error_map    plot_cv_error_map()    not branched
 #   cv_fold_map     plot_cv_folds()
