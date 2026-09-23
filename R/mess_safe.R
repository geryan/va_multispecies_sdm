#' MESS for a SpatRaster, avoiding the terra multi-block in-memory write bug
#'
#' Drop-in replacement for `bssdm::mess()` on a `SpatRaster` `x`. It produces
#' the same output list (`mess`, optionally `mess_by_variable`, `mod`, `mos`,
#' with class `"MessResult"`) but computes the MESS with `bssdm::mess()` on the
#' extracted cell values (its `data.frame` method) and then rebuilds the output
#' rasters from scratch.
#'
#' Why this exists: `bssdm::mess.SpatRaster()` builds its outputs with
#' `terra::writeStart()`/`writeValues()` into an in-memory raster
#' (`filename = ""`). When `x` has enough layers that `terra::blocks(x)$n > 1`
#' (e.g. the ~27-layer covariate + bioregion stack used for `africa_mess_br`),
#' terra 1.9.40 over-accumulates the in-memory value buffer: the returned
#' single-layer raster reports `ncell` cells but actually holds `2 * ncell`
#' values (the correct data is the *second* half). The object looks fine to
#' `mess()`, `plot()` and `values()`, but any `terra::writeRaster()` on it —
#' which is exactly what `tar_terra_nested()` does when storing the target —
#' fails with:
#'     [writeRaster] too many values for writing: 5853242 > 2926621
#' The 9- and 10-layer stacks (`africa_mess_nosea`, `africa_mess`) fit in a
#' single block, so they are unaffected. Computing via the `data.frame` method
#' side-steps terra's block writer entirely and is verified to match a correct
#' single-block MESS result exactly.
#'
#' Only the rows with at least one non-NA value are passed to the MESS
#' computation (mirroring `bssdm::mess.SpatRaster()`), so cost tracks the number
#' of non-NA cells rather than every cell.
#'
#' @param x A `terra::SpatRaster` where each layer is an environmental variable.
#' @param ref A `data.frame`/`matrix`/`list` of reference values, columns named
#'   to match `names(x)` (as passed to `bssdm::mess()`).
#' @param full Logical. If `TRUE`, include the per-variable similarity raster
#'   `mess_by_variable`. Default `FALSE`.
#' @param ... Additional arguments passed to `bssdm::mess()`.
#'
#' @return A list with class `"MessResult"`: `mess` (single-layer SpatRaster),
#'   `mess_by_variable` (multi-layer, only if `full = TRUE`), `mod` and `mos`
#'   (factor SpatRasters whose levels map variable index to variable name).
#'
#' @export
mess_safe <- function(x, ref, full = FALSE, ...) {
  if (!inherits(x, "SpatRaster")) {
    stop("`mess_safe()` requires `x` to be a terra::SpatRaster.", call. = FALSE)
  }

  # Determine the common variables exactly as bssdm does, so the rebuilt factor
  # levels use the same set and order that bssdm::mess() would.
  ref_df <- if (methods::is(ref, "data.frame")) ref else as.data.frame(ref)
  cols <- intersect(names(x), names(ref_df))
  if (length(cols) == 0) {
    stop("No variables in common between x and ref.", call. = FALSE)
  }
  n_vars <- length(cols)

  x <- x[[cols]]
  n_cell <- terra::ncell(x)

  # Extract values and keep only cells with at least one non-NA layer, matching
  # bssdm::mess.SpatRaster()'s block selection (rowSums(!is.na(...)) > 0).
  xv <- terra::values(x, mat = TRUE)
  keep <- which(rowSums(!is.na(xv)) > 0L)

  # Compute MESS on the data.frame path (unaffected by the terra write bug).
  df <- as.data.frame(xv[keep, , drop = FALSE])
  names(df) <- cols
  res <- bssdm::mess(x = df, ref = ref, full = full, ...)

  # Scatter a length-`keep` vector back into a full-length (n_cell) vector.
  scatter <- function(vals) {
    out <- rep(NA_real_, n_cell)
    out[keep] <- vals
    out
  }

  tmpl <- terra::rast(x[[1]])

  make_layer <- function(vals, name) {
    r <- terra::rast(tmpl)
    terra::values(r) <- scatter(vals)
    names(r) <- name
    terra::varnames(r) <- name
    r
  }

  make_factor <- function(codes, name) {
    r <- terra::rast(tmpl)
    terra::values(r) <- as.integer(scatter(codes))
    r <- suppressWarnings(terra::as.factor(r))
    levels(r)[[1]] <- data.frame(ID = seq_len(n_vars), variable = cols)
    names(r) <- name
    terra::varnames(r) <- name
    r
  }

  out_min <- make_layer(res$mess, "mess")
  out_mod <- make_factor(res$mod, "mod")
  out_mos <- make_factor(res$mos, "mos")

  if (isTRUE(full)) {
    # res$mess_by_variable is a (length(keep) x n_vars) matrix.
    sim_full <- matrix(NA_real_, nrow = n_cell, ncol = n_vars)
    sim_full[keep, ] <- res$mess_by_variable
    out_sim <- terra::rast(x)
    terra::values(out_sim) <- sim_full
    names(out_sim) <- cols

    result <- list(
      mess = out_min,
      mess_by_variable = out_sim,
      mod = out_mod,
      mos = out_mos
    )
  } else {
    result <- list(
      mess = out_min,
      mod = out_mod,
      mos = out_mos
    )
  }

  class(result) <- c("MessResult", class(result))
  result
}
