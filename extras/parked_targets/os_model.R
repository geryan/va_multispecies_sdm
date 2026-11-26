# Parked targets: The alternative model with a per-species offset strength (the
# _os targets), off since 2026-11-06 at the user's request. See
# agent_notes/os_model.md.
#
# Moved verbatim, still commented out, from _targets.R on 2026-11-24, so the
# pipeline file stays short. Nothing here runs: tar_source() reads only R/.
# Each block left a pointer comment where it was in _targets.R; to restore
# one, paste it back at its pointer and uncomment it.
#
# Blocks:
#   1. Offset-strength (os) alternative model (was lines 3074-3316, just before
#      `so_i_dont_have_to_go_backward_and_add_commas`)


# ---------------------------------------------------------------------------
# 1. Offset-strength (os) alternative model
# ---------------------------------------------------------------------------
 # The os section below was commented out on 2026-11-06 at the user's request:
 # only the main pipeline runs for now. Uncomment the whole block to restore it.
 # ###################
 # # alternative model: per-species offset strength ("os"), added 2026-10-19
 # #
 # # The same model as model_fit, except that each species' offset is raised to its own
 # # power kappa: log lambda = ... + kappa[s] * log(offset), with log kappa ~
 # # Student-t(3, 0, 0.05), so kappa stays near 1 (the current model) unless a species'
 # # data clearly favour another value. Motivated by the presence records the gambiae
 # # offset hides (extras/check_masked_presences.R). Functions are copies of the current
 # # ones with an `os_` prefix; the current functions are untouched.
 # #
 # # Only the fit, the distribution and abundance maps, and a comparison with the current
 # # model. The comparison is in-sample: PSIS-LOO on every record, and where the two
 # # models' binary ranges differ against the presence records and the offset. It needs
 # # both fits, so it waits for model_fit too. Results in outputs/model_comparison/<run_tag>/.

 # tar_target(
 #   fit_image_os,
 #   "outputs/images/model_fit_os.RData"
 # ),

 # tar_target(
 #   pred_prefix_os,
 #   "outputs/rasters/multispecies_pp_os"
 # ),

 # tar_target(
 #   plot_dir_os,
 #   sprintf(
 #     "outputs/figures/distribution_plots/distn_os_%s",
 #     run_tag
 #   )
 # ),

 # tar_target(
 #   comparison_dir_os,
 #   sprintf(
 #     "outputs/model_comparison/%s",
 #     run_tag
 #   )
 # ),

 # # ~12 h, as model_fit
 # tar_target(
 #   model_fit_os,
 #   os_fit_model_multispecies_pp_count(
 #     image_name = fit_image_os,
 #     model_data_spatial = model_data_spatial_cx,
 #     target_covariate_names = target_covariate_names,
 #     target_species = target_species,
 #     bioregion_names = bioregion_names,
 #     n_burnin = 2000,
 #     n_samples = 1000,
 #     n_chains = 40,
 #     n_cores = 8,
 #     kappa_scale = 0.05
 #   )
 # ),

 # # the image as a file target, as model_fit_image
 # tar_target(
 #   model_fit_image_os,
 #   model_fit_os,
 #   format = "file"
 # ),

 # # the 12 monthly offsets behind offsets_avg_10 (2024), at 10 km. The os maps average
 # # offset ^ kappa over the months; with kappa = 1 that is offsets_avg_10
 # tar_terra_rast(
 #   offsets_2024_10,
 #   aggregate(
 #     offsets_5[[289:300]],
 #     fact = 2,
 #     fun = "mean",
 #     cores = 4,
 #     na.rm = TRUE
 #   )
 # ),

 # # run in a fresh R process (R/in_fresh_process.R), so the memory greta::calculate()
 # # holds is given back. `lambda` here includes the offset
 # tar_target(
 #   preds_os,
 #   {
 #     pred_files <- in_fresh_process(
 #       "os_predict_lambda",
 #       list(
 #         image_name = model_fit_image_os,
 #         prediction_layer = covariate_rast_10,
 #         target_species = target_species,
 #         output_file_prefix = pred_prefix_os,
 #         offset = offsets_2024_10,
 #         sm = TRUE,
 #         nsims = 100
 #       )
 #     )
 #     c(
 #       pred_files,
 #       md5 = list(
 #         tools::md5sum(
 #           unlist(pred_files)
 #         )
 #       )
 #     )
 #   }
 # ),

 # tar_terra_rast(
 #   pred_p_os,
 #   mask_landcover_and_expert_offset(
 #     p = rast(preds_os$p),
 #     expert = expert_offset_maps_10,
 #     bare = landcover_bare_10
 #   )
 # ),

 # tar_terra_rast(
 #   pred_lambda_os,
 #   mask_landcover_and_expert_offset(
 #     p = rast(preds_os$lambda),
 #     expert = expert_offset_maps_10,
 #     bare = landcover_bare_10
 #   )
 # ),

 # tar_target(
 #   plot_pred_p_os,
 #   make_distribution_plots(
 #     pred_dist = pred_p_os,
 #     model_data_spatial_cx,
 #     plot_dir = plot_dir_os
 #   )
 # ),

 # tar_target(
 #   plot_pred_lambda_os,
 #   make_distribution_plots(
 #     pred_lambda_os,
 #     model_data_spatial_cx,
 #     plot_dir = plot_dir_os,
 #     colscheme = "orchid",
 #     distpoints = FALSE,
 #     guide = "abundance",
 #     prefix = "lambda"
 #   )
 # ),

 # # 1,000 draws out of the image, for the comparison; in a fresh process, as preds_os
 # tar_target(
 #   coef_draws_os,
 #   in_fresh_process(
 #     "os_extract_cv_draws",
 #     list(
 #       image_name = model_fit_image_os,
 #       output_file = "outputs/draws/coef_draws_os.rds",
 #       n_keep = 1000
 #     )
 #   ),
 #   format = "file"
 # ),

 # tar_target(
 #   thresholds_maxsss_os,
 #   maxsss_thresholds(
 #     pred_p = pred_p_os,
 #     model_data_spatial = model_data_spatial_cx
 #   )
 # ),

 # # PSIS-LOO on every record, os - current, by data type and species
 # tar_target(
 #   compare_loo_os,
 #   os_compare_loo(
 #     draws_file_current = coef_draws,
 #     draws_file_os = coef_draws_os,
 #     model_data_spatial = model_data_spatial_cx,
 #     output_dir = comparison_dir_os
 #   )
 # ),

 # # presence cells hidden and shown, range by offset level, and the low-offset area
 # # before the masks, each model at its own maxSSS threshold
 # tar_target(
 #   compare_maps_os,
 #   os_compare_maps(
 #     pred_p = pred_p,
 #     pred_p_os = pred_p_os,
 #     p_unmasked = pred_dist_not_masked,
 #     p_unmasked_os = rast(preds_os$p),
 #     thresholds = thresholds_maxsss,
 #     thresholds_os = thresholds_maxsss_os,
 #     offset = offsets_avg_10,
 #     model_data_records = model_data_records,
 #     target_species = target_species,
 #     output_dir = comparison_dir_os
 #   )
 # ),

 # tar_target(
 #   plot_kappa_os,
 #   os_plot_kappa(
 #     draws_file = coef_draws_os,
 #     output_dir = comparison_dir_os
 #   ),
 #   format = "file"
 # ),

 # tar_target(
 #   plot_range_change_os,
 #   os_plot_range_change(
 #     pred_p = pred_p,
 #     pred_p_os = pred_p_os,
 #     thresholds = thresholds_maxsss,
 #     thresholds_os = thresholds_maxsss_os,
 #     model_data_records = model_data_records,
 #     target_species = target_species,
 #     output_dir = comparison_dir_os
 #   ),
 #   format = "file"
 # ),

 # tar_target(
 #   validation_dir_os,
 #   sprintf(
 #     "outputs/figures/validation/os_%s/",
 #     run_tag
 #   )
 # ),

 # # predictive checks, residuals and R-hats for the os fit, as resids_and_rhats; in a
 # # fresh process, as preds_os
 # tar_target(
 #   resids_and_rhats_os,
 #   in_fresh_process(
 #     "validation_and_checking",
 #     list(
 #       model_fit_image_multisp_pp_count_sm = model_fit_image_os,
 #       nsims = 100,
 #       plotdir = validation_dir_os
 #     )
 #   )
 # ),
