# Parked targets: Versions of resids_and_rhats, preds and coef_draws that
# differ from the active ones only in whether they run in the pipeline's own
# process or in a fresh one (R/in_fresh_process.R). See
# agent_notes/pipeline.md.
#
# Moved verbatim, still commented out, from _targets.R on 2026-11-24, so the
# pipeline file stays short. Nothing here runs: tar_source() reads only R/.
# Each block left a pointer comment where it was in _targets.R; to restore
# one, paste it back at its pointer and uncomment it.
#
# Blocks:
#   1. resids_and_rhats in the pipeline's own process (was lines 2233-2242,
#      just before `preds`)
#   2. preds in a fresh R process (was lines 2270-2300, just before
#      `pred_dist_not_masked`)
#   3. coef_draws in the pipeline's own process (was lines 2684-2694, just
#      before `plot_response_curves`)


# ---------------------------------------------------------------------------
# 1. resids_and_rhats in the pipeline's own process
# ---------------------------------------------------------------------------
 # the same target run in the pipeline's own process, as it was until
 # 2026-11-09
 # tar_target(
 #   resids_and_rhats,
 #   validation_and_checking(
 #     model_fit_image_multisp_pp_count_sm = model_fit_image,
 #     nsims = 100,
 #     plotdir = validation_dir
 #   )
 # ),


# ---------------------------------------------------------------------------
# 2. preds in a fresh R process
# ---------------------------------------------------------------------------
 # the same target run in a fresh R process (R/in_fresh_process.R).
 # greta::calculate() in predict_lambda() leaves ~5.2 GB held after it
 # returns, which pushed the targets after it over R's 16 GB limit. Not
 # needed while resids_and_rhats and coef_draws run in fresh processes
 # (since 2026-11-09). To use it, comment out the target above and uncomment
 # this one
 # tar_target(
 #   preds,
 #   {
 #     pred_files <- in_fresh_process(
 #       predict_lambda,
 #       list(
 #         image_name = model_fit_image,
 #         prediction_layer = covariate_rast_10, # use 10k for faster preds
 #         target_species = target_species,
 #         output_file_prefix = pred_prefix,
 #         offset = offsets_avg_10,
 #         sm = TRUE, # if predict survey method
 #         nsims = 100 # lower for faster preds
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


# ---------------------------------------------------------------------------
# 3. coef_draws in the pipeline's own process
# ---------------------------------------------------------------------------
 # the same target run in the pipeline's own process, as it was until
 # 2026-11-09
 # tar_target(
 #   coef_draws,
 #   extract_cv_draws(
 #     image_name = model_fit_image,
 #     output_file = "outputs/draws/coef_draws.rds",
 #     n_keep = 1000
 #   ),
 #   format = "file"
 # ),
