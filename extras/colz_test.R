
  model_fit_sre_rep <-  fit_model_multispecies_pp_count_source_effect_reparam(
    image_name = "outputs/images/model_fit_test_source_re_rep_colz.RData",
    model_data_spatial = model_data_spatial |>
      filter(
        !(species == "coluzzii" & longitude > 40 & presence == 1)
      ),
    target_covariate_names = target_covariate_names,
    target_species = target_species,
    bioregion_names = bioregion_names,
    n_burnin = 200,
    n_samples = 200,
    n_chains = 10,
    n_cores = 8
  )

  preds_sm_rep <- predict_lambda_reparam(
    image_name = model_fit_sre_rep,
    prediction_layer = covariate_rast_10, # use 10k for faster preds
    target_species,
    output_file_prefix = "outputs/rasters/reparam_multispecies_pp_rep_colz",
    offset = offsets_avg_10,
    sm = TRUE, # if predict survey method
    nsims = 100 # lower for faster preds
  )

    pred_dist_not_masked_rep <- rast(preds_sm_rep$p)

  pred_p_rep <- mask_landcover_and_expert_offset(
    p = pred_dist_not_masked_rep,
    expert = expert_offset_maps_10,
    bare = landcover_bare_10
  )

  plot_pred_p_rep <- make_distribution_plots(
    pred_dist = pred_p_rep,
    model_data_spatial |>
      filter(
        !(species == "coluzzii" & longitude > 40 & presence == 1)
      ),
    plot_dir = "outputs/figures/distribution_plots/distn_20260825_rep_colz"
  )
