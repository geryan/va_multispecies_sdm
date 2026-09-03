#' Read a raster target (a file path) back off disk
#'
#' `lookup` re-applies categorical levels, which do not always survive the
#' write/read round trip -- prepare_landcover() and prepare_categorical_layer()
#' both re-apply them for the same reason.
read_rast <- function(path, lookup = NULL) {
  r <- terra::rast(path)
  if (!is.null(lookup)) {
    for (i in seq_len(terra::nlyr(r))) {
      levels(r[[i]]) <- lookup
    }
  }
  r
}
