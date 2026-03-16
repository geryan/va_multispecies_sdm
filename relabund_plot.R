library(targets)
library(terra)
library(ggplot2)
library(tidyterra)
tar_load(pred_file_multisp_pp_count_sm)
lambda <- rast(pred_file_multisp_pp_count_sm$count)


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

writeRaster(lambda_masked,
            "~/Dropbox/sharing/va_abundance_prototype_20260303.tif")

lambda_plot <- lambda_masked
names(lambda_plot) <- paste("An.", names(lambda_masked))
# clamp values
plot_max <- 35
lambda_plot <- app(lambda_plot,
                   function(x) {pmin(x, plot_max)})

relabund_plot <- ggplot() +
  geom_spatraster(
    data = lambda_plot
  ) +
  theme_void() +
  facet_wrap(
    ~lyr
  ) +
  scale_fill_gradient(
    low = grey(0.9),
    high = "navy",
    na.value = "transparent",
    transform = "log1p",
    name = "abundance\n(average HLC)",
    limits = c(0, plot_max)
  )

ggsave("~/Desktop/lambda_plot.png",
       relabund_plot,
       width = 10,
       height = 10,
       dpi = 900,
       bg = "white")


# check the saved file has the names
tmp <- rast("~/Dropbox/sharing/va_abundance_prototype_20260303.tif")
names(tmp)


# can we adjust An arabiensis predicted abundance distribution by scaling the
# mechanistic abundance distribution with covariates?

log_offset <- log(offsets_avg_10)

library(geodata)
geodata_path("data/raster/geodata/")
vap <- geodata::worldclim_global("vapr", res = 10)
tav <- geodata::worldclim_global("tavg", res = 10)

vap_mean <- mean(vap)
tav_mean <- mean(tav)
vap_mean_af <- crop(vap_mean, log_offset)
tav_mean_af <- crop(tav_mean, log_offset)

plot(vap_mean_af)
plot(tav_mean_af)
plot(log_offset)

# temperature seems more relevant for the Northern Sahel
tav_mean_af_res <- terra::resample(tav_mean_af, log_offset)
tav_mean_af_res <- mask(tav_mean_af_res, log_offset)
tav_mean_af_res_scale <- scale(tav_mean_af_res)

plot(tav_mean_af_res_scale)

# plot offset modified by different coefficients for static average temperature
# correction
par(mfrow = c(2, 2))
n_plot <- prod(par()$mfrow)
coefs <- round(seq(0, 2, length.out = n_plot), 2)
for (this_coef in coefs) {
  offset_cor <- exp(log_offset + this_coef * tav_mean_af_res_scale)
  plot(sqrt(offset_cor),
       # range = c(0, 5),
       main = paste("tavg coefficient:", this_coef))
}
