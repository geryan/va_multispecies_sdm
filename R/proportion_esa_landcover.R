#' Class-proportion land cover for one year, on the new_mask grid
#'
#' Companion to prepare_esa_landcover(), answering a different question about
#' the same file. prepare_esa_landcover() takes the MAJORITY class of the ~225
#' 300 m cells under each 5 km cell and throws the rest away; this keeps all
#' 225 and reports what fraction of them fell in each of the 10 grouped covers
#' (see esa_landcover_group_defs()). A 5 km cell that is 60% savanna and 40%
#' cropland reads as pure savanna in the first and as 0.6 / 0.4 here.
#'
#' Because of that, this CANNOT be built from esa_landcover_year -- the
#' sub-grid detail is already gone by then. It has to go back to the source
#' NetCDF, which is why it takes `archive` (esa_landcover_zip) and not a tif.
#'
#' GRIDS. The source is exactly 1/360 deg on the global graticule; new_mask is
#' 0.04166665 deg, i.e. 15.0 source cells across, offset from the source
#' graticule by 0.023 of a source cell (~6.5 m). So each output cell is a
#' clean 15 x 15 = 225 source-cell block, and `method = "average"` -- which is
#' area-weighted and skips NA source cells -- reproduces a hand-tabulated
#' block mean to within 1e-3. Checked against the disagg/aggregate route on
#' the real grid offsets; they agree to 0.001, well under one source cell in
#' 225 (0.0044).
#'
#' DENOMINATOR. no_data, snow_and_ice and lichens_and_mosses are mapped to NA
#' before the average, so they leave both the numerator and the denominator:
#' the proportions are of the cells that carry usable cover, and sum to 1.
#' A cell with no usable source cell at all comes out NA in every class, not 0
#' -- which is the honest answer and keeps it out of any later sum. The
#' function verifies the sum-to-1 property before returning.
#'
#' MEMORY. Same discipline as prepare_esa_landcover(): every terra step is
#' handed a `filename` and streams block by block, so peak RAM is one block
#' rather than one raster. The segregated stack is the big intermediate -- 10
#' layers at source resolution -- and is written INT1U + DEFLATE to scratch
#' that is wiped on exit.
#'
#' @param archive path to the CDS zip (or a bare .nc) for this year
#' @param new_mask reference grid; the output matches it cell for cell
#' @param year the year, used for messages and the output filename
#' @param groups named list of group -> legend categories
#' @param tolerance how far the class proportions may sum from 1 before the
#'   function errors, which is the check that NA handling survived the warp
#' @return path to a written .tif of length(groups) layers, one per class,
#'   for tar_target(format = "file")
proportion_esa_landcover <- function(
    archive,
    new_mask,
    year,
    groups = esa_landcover_group_defs(),
    ignore = esa_landcover_ignored_classes(),
    outputdir = "outputs/raster/esa_landcover_proportion",
    varname = "lccs_class",
    tolerance = 1e-4
  ){

  year <- as.integer(year)

  dir.create(outputdir, showWarnings = FALSE, recursive = TRUE)

  outfile <- file.path(
    outputdir,
    sprintf("esa_landcover_proportion_%d.tif", year)
  )

  scratch <- file.path(tempdir(), sprintf("esa_lc_prop_%d", year))
  dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)

  nc <- esa_landcover_nc(archive, scratch)

  # the legend is read from THIS year's file rather than passed in, so that a
  # product version with a different legend errors in
  # esa_landcover_group_lookup() instead of being silently misgrouped
  key <- esa_landcover_group_lookup(
    lookup = esa_landcover_legend(nc, varname = varname),
    groups = groups,
    ignore = ignore
  )

  class_names <- names(groups)
  n_class <- length(class_names)

  r <- terra::rast(sprintf('NETCDF:"%s":%s', nc, varname))

  if (terra::crs(r) == "") {
    terra::crs(r) <- "EPSG:4326"
  }

  if (terra::nlyr(r) > 1L) {
    message(sprintf("%d: %d layers in %s, using the first", year, terra::nlyr(r), varname))
    r <- r[[1]]
  }

  message(
    sprintf(
      "%d: source %s cells at %.6f deg -> %s cells at %.6f deg (%.1f source cells per output cell)",
      year,
      format(terra::ncell(r), big.mark = ","),
      terra::res(r)[1],
      format(terra::ncell(new_mask), big.mark = ","),
      terra::res(new_mask)[1],
      prod(terra::res(new_mask) / terra::res(r))
    )
  )

  # 1. down to the study area before doing anything expensive
  cropped <- terra::crop(
    r,
    terra::ext(new_mask),
    snap = "out",
    filename = file.path(scratch, "cropped.tif"),
    overwrite = TRUE,
    datatype = "INT1U"
  )

  # 2. 38 LCCS classes -> 10 group ids, with the ignored classes going to NA
  #    so they drop out of the average entirely
  grouped <- terra::classify(
    cropped,
    rcl = as.matrix(key[!is.na(key$group_id), c("value", "group_id")]),
    others = NA,
    filename = file.path(scratch, "grouped.tif"),
    overwrite = TRUE,
    datatype = "INT1U"
  )

  # 3. one 0/1 layer per class, NA kept as NA. `classes` is given explicitly
  #    so every year has all 10 layers in the same order even where a class
  #    is absent
  segregated <- terra::segregate(
    grouped,
    classes = seq_len(n_class),
    other = 0,
    filename = file.path(scratch, "segregated.tif"),
    overwrite = TRUE,
    datatype = "INT1U",
    gdal = c("COMPRESS=DEFLATE", "ZLEVEL=1")
  )

  # 4. the actual aggregation: mean of a 0/1 layer over the source cells under
  #    each output cell IS the proportion. "average" skips NA source cells, so
  #    the denominator is the usable cells, not all 225
  proportion <- terra::resample(
    segregated,
    new_mask,
    method = "average",
    filename = file.path(scratch, "proportion.tif"),
    overwrite = TRUE,
    datatype = "FLT4S"
  )

  names(proportion) <- class_names

  out <- terra::mask(
    proportion,
    new_mask,
    filename = outfile,
    overwrite = TRUE,
    datatype = "FLT4S",
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=3", "ZLEVEL=6")
  )

  # --- checks: cheap, and each one has caught a real class of failure ---

  if (!isTRUE(terra::compareGeom(out, new_mask, stopOnError = FALSE))) {
    stop(
      sprintf("proportion_esa_landcover(): %d does not match new_mask", year),
      call. = FALSE
    )
  }

  # if the NA flag had not survived the write/warp, ignored cells would be
  # averaged in as 255 and this would be nowhere near 1
  total <- terra::app(out, "sum")
  rng <- unlist(terra::global(total, range, na.rm = TRUE))

  if (any(!is.finite(rng)) || any(abs(rng - 1) > tolerance)) {
    stop(
      sprintf(
        "proportion_esa_landcover(): %d class proportions sum to [%s], not 1 -- NA handling has gone wrong",
        year,
        paste(signif(rng, 6), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  message(
    sprintf(
      "%d: %d classes written, %s cells with cover, sums in [%s]",
      year,
      terra::nlyr(out),
      format(unlist(terra::global(!is.na(total), "sum", na.rm = TRUE)), big.mark = ","),
      paste(signif(rng, 8), collapse = ", ")
    )
  )

  outfile

}
