#' Year of each layer of a time-indexed raster
#'
#' `terra::time()` returns a bare numeric when the time axis was set with
#' `tstep = "years"` -- which is how these stacks are written -- and a Date when
#' it was set any other way. `format(x, "%Y")` is right for the second and
#' silently wrong for the first (it hands "%Y" to `format.default()` as `trim`);
#' `as.integer()` is right for the first and silently wrong for the second (a
#' Date becomes days since 1970). This picks the right one, and refuses
#' anything that does not come out as a plausible year rather than passing a
#' nonsense number downstream where it would mismatch every record.
#'
#' @param r SpatRaster with a time axis
#' @return integer vector of years, one per layer
#' @author geryan
#' @export
hfp_layer_years <- function(r) {

  tt <- terra::time(r)

  years <- if (inherits(tt, c("Date", "POSIXct", "POSIXlt"))) {
    as.integer(format(tt, "%Y"))
  } else {
    suppressWarnings(as.integer(tt))
  }

  bad <- !is.na(years) & (years < 1800L | years > 2200L)

  if (any(bad)) {
    stop(
      sprintf(
        "hfp_layer_years(): time axis does not look like years (got %s)",
        paste(utils::head(years[bad], 3), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  years

}


#' Stack the annual Human Footprint rasters into one time-indexed file
#'
#' Takes the per-year project-grid rasters written by `download_mu_hfp()` and
#' writes one multi-layer file, one layer per year, carrying a `terra::time()`
#' axis. That time axis is what `extract_year_indexed_layer_data()` matches
#' records against, so it is set here rather than assumed downstream.
#'
#' The land cover counterpart is `stack_esa_landcover_proportion()`, and the
#' shape of the result is deliberately the same: one file, one layer per year,
#' layers named `<variable>_<year>`.
#'
#' ## Scaling has to happen here
#'
#' `scale_rast_to_1()` divides every layer by *its own* maximum. On a single
#' static layer that is what you want. On a 25-year stack it is not: each year
#' would get a different divisor, so a cell whose Human Footprint never changed
#' would appear to drift, and the between-year signal -- the entire reason for
#' doing this -- would be replaced by an artefact of the annual maxima.
#'
#' So the whole series is divided by **one** constant, the maximum over every
#' year and every cell, which is only knowable once all the years are in hand.
#' Relative differences between years survive untouched, and the result is on
#' the same 0-1 scale as the static `footprint_5` it replaces, so the sign
#' constraint and prior on its coefficient carry over. The divisor is written
#' into the GeoTIFF as the `hfp_scale_divisor` metadata tag -- read it back with
#' `terra::metags()` -- and also returned as an attribute on the path.
#'
#' @param paths character vector of per-year rasters, i.e. the `mu_hfp_year`
#'   target
#' @param years year of each path. `NULL` (the default) reads each file's own
#'   time axis, which is what `download_mu_hfp()` sets
#' @param varname stem for the layer names
#' @param scale_to_1 divide the whole series by its overall maximum
#' @param outputdir directory for the output
#' @param filename output file name
#' @return path to the stacked raster, for `tar_target(format = "file")`
#' @author geryan
#' @export
stack_mu_hfp <- function(
    paths,
    years = NULL,
    varname = "footprint",
    scale_to_1 = TRUE,
    outputdir = "outputs/raster/mu_hfp",
    filename = "mu_hfp_all.tif"
  ) {

  paths <- as.character(paths)

  missing_paths <- paths[!file.exists(paths)]

  if (length(missing_paths)) {
    stop(
      sprintf(
        "stack_mu_hfp(): file not found: %s",
        paste(missing_paths, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  layers <- lapply(paths, terra::rast)

  n_lyr <- vapply(layers, function(r) as.integer(terra::nlyr(r)), integer(1))

  if (any(n_lyr != 1L)) {
    stop(
      sprintf(
        "stack_mu_hfp(): expected one layer per file, %s has %d",
        basename(paths[which(n_lyr != 1L)[1]]),
        n_lyr[which(n_lyr != 1L)[1]]
      ),
      call. = FALSE
    )
  }

  # each file's own time axis, not its position in `paths`, so a change in
  # branch order cannot relabel the series
  if (is.null(years)) {
    years <- vapply(
      layers,
      function(r) hfp_layer_years(r)[1],
      integer(1)
    )
  }

  years <- as.integer(years)

  if (anyNA(years)) {
    stop(
      sprintf(
        "stack_mu_hfp(): no year on the time axis of %s",
        paste(basename(paths[is.na(years)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (anyDuplicated(years)) {
    stop(
      sprintf(
        "stack_mu_hfp(): more than one file for year %s",
        paste(unique(years[duplicated(years)]), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  ord <- order(years)
  years <- years[ord]
  layers <- layers[ord]
  paths <- paths[ord]

  # a year the series skips would be matched to its neighbour silently
  gaps <- setdiff(seq(min(years), max(years)), years)

  if (length(gaps)) {
    stop(
      sprintf(
        "stack_mu_hfp(): missing year(s) %s",
        paste(gaps, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  for (i in seq_along(layers)[-1]) {
    if (!terra::compareGeom(layers[[1]], layers[[i]], stopOnError = FALSE)) {
      stop(
        sprintf(
          "stack_mu_hfp(): %s is not on the same grid as %s",
          basename(paths[i]),
          basename(paths[1])
        ),
        call. = FALSE
      )
    }
  }

  r <- terra::rast(layers)

  divisor <- 1

  if (scale_to_1) {

    # one constant for the whole series -- see the note above
    divisor <- max(
      terra::global(r, fun = "max", na.rm = TRUE)[, 1],
      na.rm = TRUE
    )

    if (!is.finite(divisor) || divisor <= 0) {
      stop(
        sprintf("stack_mu_hfp(): series maximum is %s, cannot scale", divisor),
        call. = FALSE
      )
    }

    r <- r / divisor

  }

  names(r) <- sprintf("%s_%d", varname, years)
  terra::time(r, tstep = "years") <- years

  # written into the GeoTIFF so the scaling is recoverable from the file alone,
  # without knowing which call produced it
  terra::metags(r) <- c(
    hfp_scale_divisor = format(divisor, digits = 15),
    hfp_source = "Mu et al. 2022, figshare 16571064, doi:10.1038/s41597-022-01284-8"
  )

  dir.create(outputdir, showWarnings = FALSE, recursive = TRUE)
  outfile <- file.path(outputdir, filename)

  terra::writeRaster(r, outfile, overwrite = TRUE)

  out <- terra::rast(outfile)

  # writeRaster silently dropping names or the time axis would leave records
  # matched to the wrong year, so check rather than assume
  if (!identical(names(out), sprintf("%s_%d", varname, years))) {
    stop(
      sprintf(
        "stack_mu_hfp(): layer names did not survive the write (got %s ...)",
        paste(utils::head(names(out), 3), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  written_years <- hfp_layer_years(out)

  if (!identical(written_years, years)) {
    stop(
      "stack_mu_hfp(): time axis did not survive the write",
      call. = FALSE
    )
  }

  tags <- terra::metags(out)

  if (!"hfp_scale_divisor" %in% tags$name) {
    warning(
      "stack_mu_hfp(): the scale divisor was not written to the file metadata",
      call. = FALSE
    )
  }

  message(
    sprintf(
      "stack_mu_hfp(): %d-%d written to %s (divisor %s)",
      min(years), max(years), outfile, format(divisor)
    )
  )

  attr(outfile, "divisor") <- divisor

  outfile

}
