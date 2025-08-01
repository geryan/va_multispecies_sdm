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
    project_mask
  ) {

  dat_distinct <- dat |>
    select(
      longitude,
      latitude
    ) |>
    distinct()

  svs <- terra::extract(
      x = c(project_mask, lyrs),
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
    ) |>
    filter(!is.na(project_mask)) |>
    select(-project_mask)

}
