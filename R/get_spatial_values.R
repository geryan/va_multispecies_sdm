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
    bgs,
    old = FALSE
  ) {

  if(old) {
    spatial_values <- bind_rows(
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

    return(spatial_values)
  }

  dat_distinct <- dat |>
    select(
      longitude,
      latitude
    ) |>
    distinct()

  svs <- terra::extract(
      x = lyrs,
      y = dat_distinct |>
        rename(
          lon = longitude,
          lat = latitude
        ) |>
        as.matrix()
    )

  sv_distinct <- bind_cols(
    dat_distinct,
    svs
  )

  dat |>
    left_join(
      y = sv_distinct
    )

}
