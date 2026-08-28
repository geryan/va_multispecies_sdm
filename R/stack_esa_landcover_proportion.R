#' Turn the per-year proportion stacks inside out: one file per land cover class
#'
#' proportion_esa_landcover() branches over years, so it produces 31 files of
#' 10 layers -- a year per file, a class per layer. What the model wants is the
#' transpose: 10 files of 31 layers, a class per file and a year per layer, so
#' that one class reads as a time series with a single terra::rast() call.
#'
#' This is a pure restack: the layers are already on the new_mask grid and are
#' file backed, so terra::rast() stays lazy and writeRaster streams. Nothing
#' is recomputed and RAM does not scale with the number of years.
#'
#' Called once per class, so it fits tar_target(pattern = map(<class names>)).
#'
#' @param paths the per-year files from proportion_esa_landcover(), in any
#'   order -- they are sorted by `years`
#' @param years one year per element of `paths`
#' @param class name of the single class to pull out, matched against the
#'   layer names written by proportion_esa_landcover()
#' @return path to the written .tif, for tar_target(format = "file")
stack_esa_landcover_proportion <- function(
    paths,
    years,
    class,
    outputdir = "outputs/raster/esa_landcover_proportion",
    filename = NULL
  ){

  class <- as.character(class)

  if (length(class) != 1L) {
    stop(
      sprintf("stack_esa_landcover_proportion(): expected one class, got %d", length(class)),
      call. = FALSE
    )
  }

  if (length(paths) != length(years)) {
    stop("stack_esa_landcover_proportion(): length(paths) != length(years)", call. = FALSE)
  }

  dir.create(outputdir, showWarnings = FALSE, recursive = TRUE)

  if (is.null(filename)) {
    filename <- file.path(
      outputdir,
      sprintf("esa_landcover_proportion_%s.tif", class)
    )
  }

  ord <- order(as.integer(years))
  paths <- paths[ord]
  years <- as.integer(years[ord])

  layers <- lapply(
    seq_along(paths),
    function(i) {
      r <- terra::rast(paths[i])
      if (!class %in% names(r)) {
        stop(
          sprintf(
            "stack_esa_landcover_proportion(): no layer '%s' in %s (has %s)",
            class,
            basename(paths[i]),
            paste(names(r), collapse = ", ")
          ),
          call. = FALSE
        )
      }
      r[[class]]
    }
  )

  r <- terra::rast(layers)

  layer_names <- sprintf("%s_%d", class, years)

  names(r) <- layer_names

  # a real time axis as well as the names, so the stack can be subset by year
  # rather than by position
  terra::time(r, tstep = "years") <- years

  terra::writeRaster(
    r,
    filename = filename,
    overwrite = TRUE,
    datatype = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL=6")
  )

  # the year labels are the only thing identifying which layer is which, so
  # confirm the write round-tripped rather than trusting it
  out <- terra::rast(filename)

  if (!identical(names(out), layer_names)) {
    stop(
      sprintf(
        "stack_esa_landcover_proportion(): layer names did not survive the write (got %s)",
        paste(utils::head(names(out), 3), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  filename

}
