rcms <- function(x, y){
  terra::rast(x) |>
    terra::crop(y) |>
    terra::mask(y) |>
    terra::scale()
}
