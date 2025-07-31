# make quick species composition map from PA rasters

rm(list = ls())

library(targets)
tar_load_globals()
tar_load_everything()


# load stack
multisp <- rast("outputs/rasters/va_plots_20250718/expert_offset_preds_mspp.tif")

# subset to target species
rgb_target_species <- c("arabiensis", "gambiae_coluzzii", "funestus")
target_multisp <- multisp[[match(rgb_target_species, names(multisp))]]

# convert to abundance by inverting cloglog link, then exponentiating

# p = 1 - exp(-exp(x))
# lambda = exp(x)
# p = 1 - exp(-lambda)
# exp(-lambda) = 1 - p
# lambda = -log(1 - p)

target_multisp_pa <- app(target_multisp, function(x) pmin(x, 0.999))
target_multisp_abund <- -log(1 - target_multisp_pa)

any <- app(target_multisp_pa, function(x) any(x > 0.01))

# renormalise to proportions
totals <- app(target_multisp_abund, sum)
props <- target_multisp_abund / totals

most <- global(totals, "max", na.rm = TRUE)$max
totals_norm <- any * totals / most
names(totals_norm) <- "transparency"

plots <- c(props, trans = 1 - totals_norm) * 255
# plot as RGB

writeRaster(
  x = plots,
  filename = "outputs/rasters/va_plots_20250718/anopheles_rel_abund_rgb.tif"
)

terra::plotRGB(
  plots,
  r = 1, # red = arabiensis
  g = 2, # green = gambiae/coluzzi
  b = 3, # blue = funestus
  a = 4 # transparent = none
)

ggsave(
  filename = "outputs/figures/distribution_plots/plots_for_va_mtg_20250718/rgb_agcf.png",
  width = 3200,
  height = 3200,
  dpi = 300,
  units = "px"
)

png(
  filename = "outputs/figures/distribution_plots/plots_for_va_mtg_20250718/rgb_agcf.png",
  width = 3200,
  height = 3200,
  res = 300,
  units = "px"
)
terra::plotRGB(
  plots,
  r = 1, # red = arabiensis
  g = 2, # green = gambiae/coluzzi
  b = 3, # blue = funestus
  a = 4 # transparent = none
)
dev.off()


###### plotting over mask


target_multisp_pa <- app(target_multisp, function(x) pmin(x, 0.999))
target_multisp_abund <- -log(1 - target_multisp_pa)


thold <- 0.025
any_above_threshold <- app(
  x = target_multisp_pa,
  fun = function(x){
    any(x > thold)
  }
) |>
  app(
    fun = function(x){ ifelse(x == 0, NA, 1)}
  )
plot(any_above_threshold)


aatvals <- values(any_above_threshold) |>
  as.numeric()
mvals <- values(multisp[[1]]) |>
  as.numeric()

naatvals <- case_when(
  is.na(mvals) ~ NA,
  !is.na(aatvals) ~ NA,
  .default = 1
)
naat <- any_above_threshold
naat[] <- naatvals
naat
plot(naat)


# renormalise to proportions
totals <- app(target_multisp_abund, sum)
props <- target_multisp_abund / totals

most <- global(totals, "max", na.rm = TRUE)$max
totals_norm <- totals / most
names(totals_norm) <- "transparency"
plot(totals_norm)

relabund_unmasked <- c(props, trans = 1 - totals_norm) * 255

relabund <- terra::mask(
  relabund_unmasked,
  any_above_threshold
)

# plot as RGB
plotRGB(relabund)

prgb <- RGB(relabund, value = 1:3)

colz <- colorize(prgb, to = "col")

plot(colz)

plot(
  x = new_mask,
  col = "grey80",
  legend = FALSE,
  axes = FALSE,
  box = FALSE
)
plot(
  colz,
  add = TRUE
)

png(
  filename = "outputs/figures/distribution_plots/plots_for_va_mtg_20250718/rgb_agcf_withbg.png",
  width = 3200,
  height = 3200,
  res = 300,
  units = "px"
)
plot(
  x = new_mask,
  col = "grey80",
  legend = FALSE,
  axes = FALSE,
  box = FALSE
)
plot(
  colz,
  add = TRUE
)

dev.off()
