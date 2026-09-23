#' Report the native grid of the downloaded Human Footprint GeoTIFFs
#'
#' Step-3 verification for the Mu et al. Human Footprint series: opens each
#' source GeoTIFF *in its own projection*, before any reprojection, and reports
#' what is actually there.
#'
#' The point is the last column. A year whose grid does not match the others
#' exactly is not a cosmetic problem -- it means the series is not a series,
#' and every year-matched value taken from it would be taken from a different
#' place. The figshare deposit is a plausible candidate for this: the 2000-2018
#' files are internally uncompressed and identical in size, while 2019 onwards
#' were added later, are internally compressed, and cannot be assumed to share
#' the grid.
#'
#' Run this against `data/raw/mu_hfp/tif/`, which means downloading with
#' `keep_source = TRUE`; the sources are otherwise deleted.
#'
#' @param paths character vector of source GeoTIFFs
#' @param reference index of the file every other is compared against
#' @return a data frame, one row per file, with CRS name, resolution, extent,
#'   dimensions, data type, min/max and whether the grid matches `reference`
#' @author geryan
#' @export
mu_hfp_grid_report <- function(
    paths,
    reference = 1L
  ) {

  paths <- as.character(paths)

  missing_paths <- paths[!file.exists(paths)]

  if (length(missing_paths)) {
    stop(
      sprintf(
        "mu_hfp_grid_report(): file not found: %s",
        paste(missing_paths, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  rasters <- lapply(paths, terra::rast)

  ref <- rasters[[reference]]

  out <- do.call(
    rbind,
    lapply(
      seq_along(rasters),
      function(i) {

        r <- rasters[[i]]
        e <- as.vector(terra::ext(r))
        mm <- terra::minmax(r)

        data.frame(
          file = basename(paths[i]),
          crs_name = terra::crs(r, describe = TRUE)$name,
          res_x = terra::res(r)[1],
          res_y = terra::res(r)[2],
          nrow = terra::nrow(r),
          ncol = terra::ncol(r),
          nlyr = terra::nlyr(r),
          xmin = e[["xmin"]],
          xmax = e[["xmax"]],
          ymin = e[["ymin"]],
          ymax = e[["ymax"]],
          datatype = terra::datatype(r)[1],
          min = mm[1, 1],
          max = mm[2, 1],
          matches_reference = terra::compareGeom(
            ref,
            r,
            stopOnError = FALSE
          ),
          stringsAsFactors = FALSE
        )

      }
    )
  )

  rownames(out) <- NULL

  out

}
