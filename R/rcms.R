rcms <- function(x){
  terra::rast(x) |>
    terra::crop(africa_mask) |>
    terra::mask(africa_mask) |>
    sdmtools::standardise_rast()
}
