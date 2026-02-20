#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param bioregions
#' @return
#' @author geryan
#' @export
split_bioregions <- function(project_mask) {

  # because geotargets doesn't handle categorical raster data well
  # this reads in the bioregion data and processes directly
  # repeating code from get_bioregions because otherwise
  # the region names are lost
  #
  # opens OneEarth Bioregions downloaded from
  # https://www.oneearth.org/datasets/
  # One Earth Bioregions Framework, One Earth. {2023} Los Angeles, CA. oneearth.org

  r <- vect("data/one_earth-bioregions-2023.geojson") |>
    crop(project_mask) |>
    rasterize(
      y = project_mask,
      field = "Bioregions"
    ) |>
    mask(project_mask)

  bionames <- unique(r)[,1]

  s <- sapply(
    X = bionames,
    FUN = function(x, r){

      y <- r %in% x

      y <- as.numeric(y)

      names(y) <- x

      y

    },
    r = r
  ) |>
    rast()

  s

}
