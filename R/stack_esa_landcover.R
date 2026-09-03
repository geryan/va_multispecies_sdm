#' Assemble the per-year land cover tifs into one multi-layer stack
#'
#' The inputs are already on the new_mask grid, so this is just a stack and a
#' write. `terra::rast()` over file-backed layers stays lazy and writeRaster
#' streams, so RAM does not scale with the number of years.
#'
#' Levels are applied twice, before and after the write, for the same reason
#' prepare_landcover() does it -- they do not reliably survive the round trip.
#'
#' @return path to the written stack, for tar_target(format = "file")
stack_esa_landcover <- function(
    paths,
    years,
    lookup = NULL,
    filename = "outputs/raster/esa_landcover_all.tif"
  ){

  if (length(paths) != length(years)) {
    stop("stack_esa_landcover(): length(paths) != length(years)", call. = FALSE)
  }

  ord <- order(as.integer(years))
  paths <- paths[ord]
  years <- years[ord]

  layer_names <- sprintf("esa_landcover_%d", as.integer(years))

  r <- terra::rast(lapply(paths, terra::rast))

  # order matters: `levels<-` renames the layer to the lookup's category
  # column, so the year labels have to go on afterwards, not before
  if (!is.null(lookup)) {
    for (i in seq_len(terra::nlyr(r))) {
      levels(r[[i]]) <- lookup
    }
  }

  names(r) <- layer_names

  terra::writeRaster(
    r,
    filename = filename,
    overwrite = TRUE,
    datatype = "INT1U"
  )

  # confirm the write round-tripped, rather than trusting it: the year labels
  # are the only thing identifying which layer is which
  out <- terra::rast(filename)

  if (!identical(names(out), layer_names)) {
    stop(
      sprintf(
        "stack_esa_landcover(): layer names did not survive the write (got %s)",
        paste(utils::head(names(out), 3), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!is.null(lookup) && !terra::is.factor(out)[1]) {
    warning(
      "stack_esa_landcover(): class labels did not survive the write; ",
      "re-apply with levels(r[[i]]) <- esa_landcover_lookup after reading",
      call. = FALSE
    )
  }

  filename

}
