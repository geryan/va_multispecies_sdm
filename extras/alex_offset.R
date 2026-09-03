# alex offset

tar_load(uga)

uga_p <- rast("~/Documents/tki_work/vector_atlas/va_multispecies_sdm/spartan_model_comparison/m6/m6_mask_p.tif") |>
  crop(uga) |>
  mask(uga)

plot(uga_p)

writeRaster(uga_p, "extras/uganda_preds_p.tif")
