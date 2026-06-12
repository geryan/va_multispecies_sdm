#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param oneearth_vect
#' @param bioregion_names
#' @param project_mask
#' @return
#' @author geryan
#' @export
make_bioregion_mask <- function(
    oneearth_vect,
    bioregion_names,
    project_mask
  ) {

  r <- oneearth_vect |>
    tidyterra::filter(bioregion %in% bioregion_names) |>
    rasterize(
      x = _,
      y = project_mask
    )

  naidx <- which(is.na(values(r)))

  r[naidx] <- 0


  r|>
    mask(project_mask) |>
    sdmtools::set_layer_names("bioregion_mask")

}
