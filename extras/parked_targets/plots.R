# Parked targets: Plots that were switched off.
#
# Moved verbatim, still commented out, from _targets.R on 2026-11-24, so the
# pipeline file stays short. Nothing here runs: tar_source() reads only R/.
# Each block left a pointer comment where it was in _targets.R; to restore
# one, paste it back at its pointer and uncomment it.
#
# Blocks:
#   1. Covariate plots for all records (was lines 1753-1763, just before
#      `simple_point_plots`)
#   2. Expert range map plots (was lines 1868-1874, just before `africa_mess`)
#   3. Abundance on a log10 scale (was lines 2897-2928, just before
#      `abundance_cubes`)


# ---------------------------------------------------------------------------
# 1. Covariate plots for all records
# ---------------------------------------------------------------------------
 # tar_target(
 #   covs_plots_all,
 #   make_covariate_plots(
 #     record_data_spatial_all,
 #     target_species,
 #     target_covariate_names, # this needs tweaking as it
 #     # won't currently get the non-target covs which is the whole point - need to extract from covariate rasters
 #     offset_names,
 #     bias_names
 #   )
 # ),


# ---------------------------------------------------------------------------
# 2. Expert range map plots
# ---------------------------------------------------------------------------
 # tar_target(
 #   expert_map_plots,
 #   make_expert_map_plots(
 #     expert_maps,
 #     new_mask
 #   )
 # ),


# ---------------------------------------------------------------------------
# 3. Abundance on a log10 scale
# ---------------------------------------------------------------------------
 # abundance again, on a shared log10 scale, from 0.001 to the maximum across
 # all species. Anything below 0.001, masked cells included, takes the palest
 # colour, labelled "<=0.001". The linear scale flattens the rarer species; this
 # shows where they sit
 # tar_target(
 #   plot_pred_lambda_log,
 #   make_distribution_plots(
 #     pred_lambda,
 #     model_data_spatial_cx,
 #     plot_dir = plot_dir,
 #     colscheme = "orchid",
 #     distpoints = FALSE,
 #     guide = "abundance",
 #     log_scale = TRUE,
 #     prefix = "lambda_log"
 #   )
 # ),
 #
 # # the log-scale version of panel_lambda.png
 # tar_target(
 #   plot_pred_panels_log,
 #   make_distribution_panels(
 #     pred_lambda,
 #     model_data_spatial_cx,
 #     plot_dir = plot_dir,
 #     colscheme = "orchid",
 #     guide = "abundance",
 #     log_scale = TRUE,
 #     prefix = "lambda_log"
 #   ),
 #   format = "file"
 # ),
