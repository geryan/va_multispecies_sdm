# make quick species composition map from PA rasters

library(terra)
library(tidyterra)
library(ggplot2)

# load stack
multisp <- rast("~/Downloads/expert_offset_preds_mspp.tif")

# subset to target species
target_species <- c("arabiensis", "gambiae_complex", "funestus")
target_multisp <- multisp[[match(target_species, names(multisp))]]

# convert to abundance by inverting cloglog link, then exponentiating

# p = 1 - exp(-exp(x))
# lambda = exp(x)
# p = 1 - exp(-lambda)
# exp(-lambda) = 1 - p
# lambda = -log(1 - p)

target_multisp_pa <- app(target_multisp, function(x) pmin(x, 0.999))
target_multisp_abund <- -log(1 - target_multisp_pa)

# mask out low probability areas
target_multisp_abund <- target_multisp_abund * (target_multisp_pa > 0.1)
# 
# clamped <- target_multisp_pa
# clamped$arabiensis <- target_multisp_pa$arabiensis > 0.25
# clamped$gambiae_complex <- target_multisp_pa$gambiae_complex > 0.25
# clamped$funestus <- target_multisp_pa$funestus > 0.25
# any <- any(clamped)
# plot(clamped)
# plot(any)
# 
# mask_level <- 0.15
# any <- app(target_multisp_pa, function(x) {
#   any(x > mask_level)
# })

# only show places with all species present
mask_level <- 0.05
all <- app(target_multisp_pa, function(x) {
  all(x > mask_level)
})
plot(all)

# renormalise to proportions
totals <- app(target_multisp_abund, sum)
props <- target_multisp_abund / totals

# fill in divide by 0s and remask
props[is.na(props)] <- 0
props <- mask(props, target_multisp_abund$arabiensis)

# set places with no predictions to be grey
plots <- props * 255
plots[all == 0] <- 220

# plot as RGB
terra::plotRGB(plots,
               r = 1, # red = arabiensis
               g = 2, # green = gambiae/coluzzi
               b = 3) # blue = funestus
               
