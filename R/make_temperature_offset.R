make_temperature_offset <- function(x){

  geodata::worldclim_global(
    var = "tavg",
    res = 10,
    path = "data/raster/geodata/"
  ) |>
    mean() |>
    crop(y = x) |>
    terra::resample(x) |>
    mask(mask = x) |>
    scale_rast_to_1() |>
    set_layer_names(layernames = "offset_temp")


}
