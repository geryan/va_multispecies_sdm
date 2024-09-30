#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param lyrs
#' @param dat
#' @param bgs
#' @return
#' @author geryan
#' @export
get_spatial_values <- function(
    lyrs,
    dat,
    bgs) {

  bind_rows(
    dat |>
      select(
        lon,
        lat,
      ),
    bgs |>
      as_tibble() |>
      rename(
        lon = x,
        lat = y
      )
  ) |>
    as.matrix() |>
    extract(
      x = lyrs,
      y = _
    )

}
