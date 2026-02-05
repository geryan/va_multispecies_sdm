fill_na_with_nearest_mean <- function(
    r,
    maxRadiusCell = NULL,
    verbose = FALSE
) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    stop("Please install the 'terra' package.")
  }
  library(terra)

  if (!inherits(r, "SpatRaster")) stop("r must be a SpatRaster")

  # Work layer-by-layer if multilayer
  nl <- terra::nlyr(r)
  if (nl > 1) {
    if (verbose) message("Input has ", nl, " layers — processing each layer independently.")
    out_layers <- vector("list", nl)
    for (li in seq_len(nl)) {
      if (verbose) message("Processing layer ", li, "/", nl)
      out_layers[[li]] <- fill_na_with_nearest_mean(
        terra::subset(r, li),
        maxRadiusCell = maxRadiusCell,
        verbose = verbose
      )
    }
    return(terra::rast(out_layers))
  }

  # Single-layer case
  # If no NAs present, return early
  na_count <- terra::global(is.na(r), "sum", na.rm = FALSE)[1,1]
  if (na_count == 0) {
    if (verbose) message("No NA cells found — returning original raster.")
    return(r)
  }

  ncolr <- terra::ncol(r); nrowr <- terra::nrow(r)
  if (is.null(maxRadiusCell)) maxRadiusCell <- max(ncolr, nrowr)  # worst-case
  if (!is.numeric(maxRadiusCell) || maxRadiusCell < 1) stop("maxRadiusCell must be >= 1")

  # cell size (map units); use largest of x/y to be safe when cells are rectangular
  cellsize <- max(terra::res(r))

  out <- r  # copy
  iteration <- 1L
  remaining_na <- TRUE

  # Precompute a mask of original non-NA so we never overwrite original non-NA cells
  orig_non_na_mask <- !is.na(values(r))

  while (iteration <= maxRadiusCell) {
    # If no NAs remain, break
    na_count <- terra::global(is.na(out), "sum", na.rm = FALSE)[1,1]
    if (na_count == 0) {
      if (verbose) message("All NA cells filled after ", iteration - 1, " radius iterations.")
      break
    }
    if (verbose) message("Iteration ", iteration, ": remaining NA cells = ", na_count,
                         "  (search radius = ", iteration, " cells)")

    # focalMat expects distance in map units; convert iteration (cells) -> units
    d <- iteration * cellsize
    w <- terra::focalMat(out, d, type = "circle")

    # compute mean for NA cells only; na.rm=TRUE so any non-NA values inside the window are averaged
    # na.policy="only" instructs terra to only compute function for cells that are NA (saves work)
    newvals <- terra::focal(
      out, w = w,
      fun = mean,
      na.rm = TRUE,
      na.policy = "only"
    )

    # Replace only NA cells in out with non-NA results from newvals (do not touch original non-NA)
    # We will fetch vectors of values (this reads the raster values into memory once per iteration).
    out_vals <- terra::values(out)
    new_vals_vec <- terra::values(newvals)

    # positions to replace: currently NA in out AND newvals has a non-NA value
    repl_idx <- which(is.na(out_vals) & !is.na(new_vals_vec))
    if (length(repl_idx) > 0) {
      out_vals[repl_idx] <- new_vals_vec[repl_idx]
      terra::values(out) <- out_vals
    }

    iteration <- iteration + 1L
  }

  # If still NA remain after reaching maxRadiusCell, warn the user
  na_count_final <- terra::global(is.na(out), "sum", na.rm = FALSE)[1,1]
  if (na_count_final > 0) {
    warning("After searching up to radius ", maxRadiusCell,
            " cells, ", na_count_final, " NA cells remain. Increase maxRadiusCell if you want to try farther neighbours.")
  }

  # Ensure we didn't change original non-NA values (defensive)
  if (!all(is.na(terra::values(r)[orig_non_na_mask]) == FALSE)) {
    # should not happen, but ensure original values preserved:
    terra::values(out)[orig_non_na_mask] <- terra::values(r)[orig_non_na_mask]
  }

  return(out)
}
