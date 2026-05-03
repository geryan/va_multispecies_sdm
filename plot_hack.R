library(targets)
library(tidyverse)
library(terra)
library(tidyterra)
library(ggplot2)
source("R/match_offset_data.R")
source("R/extract_ym_indexed_layer_data.R")
source("R/average_last_year.R")
source("R/make_distribution_plots.R")
source("R/distplot.R")
source("R/distplotlist.R")
source("R/saveplotlist.R")
source("R/add_pa_points_list.R")
source("R/add_pa_points.R")
source("R/add_pa_points_list.R")
source("R/scale_predictions.R")

# load the count predictions

pred_file_multisp_pp_count_sm <- list(
  pa = "outputs/rasters/multisp_pp_count_sm_pa.tif",
  count = "outputs/rasters/multisp_pp_count_sm_count.tif"
)

#lambda <- rast(pred_file_multisp_pp_count_sm$count)
lambda <- pred_lambda_mean

# load the expert opinion masks
tar_load(expert_offset_maps_500)
expert_offset_mask_10 <- expert_offset_maps_500 |>
  aggregate(2)

# copy the gambiae one for coluzzi
expert_offset_mask_10$coluzzii <- expert_offset_mask_10$gambiae

# make sure they line up
expert_offset_mask_10 <- expert_offset_mask_10[[names(lambda)]]

# make a non-bare-area mask
tar_load(landcover_bare)
bare_mask <- 1 - landcover_bare$bare
bare_mask_10 <- aggregate(bare_mask, 2)

# combine with the expert masks
bare_expert_mask_10 <- bare_mask_10 * expert_offset_mask_10
names(bare_expert_mask_10) <- names(expert_offset_mask_10)

# tar_load(pred_file_multisp_pp_count_sm)

# mask lambda, reducing abundance where the land is bare, or the expert opinion
# map says no
lambda_masked <- lambda * bare_expert_mask_10
epsilon <- .Machine$double.eps
lambda_masked[bare_expert_mask_10 == 0] <- epsilon

log_lambda_masked <- log(lambda_masked)
log_lambda_masked[is.infinite(log_lambda_masked)] <- NA

# convert to probability of presence in HLC
inv_cloglog <- function(x) {
  1 - exp(-exp(x))
}

pred_dist_masked <- inv_cloglog(log_lambda_masked)


tar_load(record_data_spatial)
tar_load(bg_kmeans_df)
tar_load(offsets_5)
model_data_spatial_no_offset <- bind_rows(
  record_data_spatial |>
    mutate(weight = 1),
  bg_kmeans_df |>
    mutate(
      data_type = "bg",
      presence = 0,
      n = 0
    )
)
model_data_spatial <- match_offset_data(
  model_data_spatial_no_offset,
  offsets_5
)

make_distribution_plots(
  pred_dist_masked, # raster stack
  model_data_spatial, # data with points in it
  plot_dir = "outputs/figures/distribution_plots/distn_offset_20260313" # output directory
)

writeRaster(
  x = pred_dist_masked,
  filename = "outputs/rasters/dist_with_offset_20260313.tif"
)

