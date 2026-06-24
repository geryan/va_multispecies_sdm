# run m6

# spartan stuff
#.libPaths("/home/ryange/R/gr_lib")
.libPaths("~/R/gr_lib")


n_burnin  <- 20000
n_samples <- 2000
n_chains  <- 50
n_sims    <- 100

model_name <- "m6"

model_dir <- sprintf(
  "spartan_model_comparison/%s",
  model_name
)


library(targets)
tar_load_globals()
tar_load_everything()

m6_fit <- fit_m6(
  model_data_spatial = model_data_spatial,
  target_covariate_names = target_covariate_names,
  target_species = target_species,
  subrealm_names = subrealm_names,
  bioregion_names = bioregion_names,
  soiltype_names = soiltype_names,
  project_mask = project_mask_5,
  image_name = sprintf(
    "%s/%s.RData",
    model_dir,
    model_name
  ),
  n_burnin = n_burnin,
  n_samples = n_samples,
  n_chains = n_chains
)


m6_fit <- sprintf(
  "%s/%s.RData",
  model_dir,
  model_name
)


resids_m6 <- validation_and_checking(
  model_fit_image_multisp_pp_count_sm = m6_fit,
  nsims = n_sims,
  plotdir = model_dir
)


pred_lambda_5 <- predict_lambda_m6_with_masking(
  image_name = m6_fit,
  prediction_layer = covariate_rast_5, # use 10k for faster preds
  target_species,
  output_file_prefix = sprintf(
    "%s/%s_mask",
    model_dir,
    model_name
  ),
  offset = offsets_avg_5,
  sm = TRUE, # if predict survey method
  nsims = n_sims, # lower for faster preds
  bioregion_mask = bioregion_mask_5 * (1 - landcover_bare)
)

pred_dist_sm <- rast(pred_lambda$p)

p <- add_expert_offset(pred_dist_sm, expert_offset_maps_10)


distribution_plots_sm <- make_distribution_plots(
  p,
  model_data_spatial,
  plot_dir = model_dir
)

writeRaster(
  p,
  10
)



