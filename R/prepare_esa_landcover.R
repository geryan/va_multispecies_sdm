#' Rescale one year of ESA CCI land cover onto the new_mask grid
#'
#' The CDS delivers a zipped NetCDF; only `lccs_class` (the land cover
#' classification itself) is wanted. As delivered it holds eight subdatasets --
#' lccs_class, processed_flag, current_pixel_state, observation_count,
#' change_count, and the lat/lon/time bounds -- so the variable has to be
#' named explicitly. `lccs_class` comes back as a single layer (the `time`
#' dimension is length 1) and does carry a CRS, so the two guards below are
#' belt-and-braces rather than load-bearing.
#'
#' MEMORY. Even subset to Africa the source is ~677 million cells at 300 m, so
#' nothing is ever pulled into RAM: every terra step is handed a `filename` and
#' therefore streams block by block to disk. Peak memory is one block, not one
#' raster. `terra::rast()` on the NetCDF is lazy for the same reason.
#'
#' Measured on the full Africa extent: 17 s and ~2.8 GB peak RSS per year.
#' Block size follows terra's memfrac, so the ceiling is tunable rather than
#' fixed -- terraOptions(memfrac = 0.1) brings the same run down to ~1.6 GB
#' with no change in runtime or output.
#'
#' RESAMPLING. 300 m -> 1 km on categorical data, so `method = "mode"`: each
#' output cell takes the majority class of the ~9 source cells under it.
#' Resampling straight onto `new_mask` (rather than aggregate(fact = 3) then
#' snap) means no assumption that the delivered subset nests exactly inside the
#' 1/120 graticule -- terra reconciles the grids itself, and the result is
#' guaranteed cell-for-cell identical to new_mask.
#'
#' @return path to the written .tif, for tar_target(format = "file")
prepare_esa_landcover <- function(
    archive,
    new_mask,
    year,
    outputdir = "outputs/raster/esa_landcover",
    varname = "lccs_class"
  ){

  year <- as.integer(year)

  dir.create(outputdir, showWarnings = FALSE, recursive = TRUE)

  outfile <- file.path(outputdir, sprintf("esa_landcover_%d.tif", year))

  # scratch space for this year only, wiped on exit even if terra errors
  scratch <- file.path(tempdir(), sprintf("esa_lc_%d", year))
  dir.create(scratch, showWarnings = FALSE, recursive = TRUE)
  on.exit(unlink(scratch, recursive = TRUE), add = TRUE)

  nc <- esa_landcover_nc(archive, scratch)

  # GDAL's netCDF driver has no /vsizip support, hence the unzip above; the
  # NETCDF: prefix picks the one variable out of the five in the file.
  r <- terra::rast(sprintf('NETCDF:"%s":%s', nc, varname))

  # the CCI files are plain WGS84 but do not always carry a CRS through GDAL
  if (terra::crs(r) == "") {
    terra::crs(r) <- "EPSG:4326"
  }

  # lccs_class carries a length-1 `time` dimension; if a future version ever
  # returns more than one step, take the map itself rather than stacking them
  if (terra::nlyr(r) > 1L) {
    message(sprintf("%d: %d layers in %s, using the first", year, terra::nlyr(r), varname))
    r <- r[[1]]
  }

  message(
    sprintf(
      "%d: source %s cells at %.6f deg",
      year,
      format(terra::ncell(r), big.mark = ","),
      terra::res(r)[1]
    )
  )

  # 1. cut down to the study area first, so the expensive step is as small as
  #    possible. snap = "out" keeps full coverage of new_mask. No-op if the
  #    CDS already honoured the `area` request.
  cropped <- terra::crop(
    r,
    terra::ext(new_mask),
    snap = "out",
    filename = file.path(scratch, "cropped.tif"),
    overwrite = TRUE,
    datatype = "INT1U"
  )

  # 2. 300 m -> 1 km by majority class, landing exactly on the new_mask grid
  resampled <- terra::resample(
    cropped,
    new_mask,
    method = "mode",
    filename = file.path(scratch, "resampled.tif"),
    overwrite = TRUE,
    datatype = "INT1U"
  )

  # 3. clip to the mask's own valid cells and write the keeper
  #    INT1U: LCCS codes top out at 220, and 255 carries NA
  out <- terra::mask(
    resampled,
    new_mask,
    filename = outfile,
    overwrite = TRUE,
    datatype = "INT1U"
  )

  if (!isTRUE(terra::compareGeom(out, new_mask, stopOnError = FALSE))) {
    stop(
      sprintf("prepare_esa_landcover(): %d does not match new_mask", year),
      call. = FALSE
    )
  }

  outfile

}

#' Unpack the CDS delivery and return the path to the NetCDF inside it
#'
#' Handles both shapes the CDS uses: a .zip wrapping the .nc, or a bare .nc.
esa_landcover_nc <- function(archive, exdir){

  if (grepl("\\.nc$", archive, ignore.case = TRUE)) {
    return(archive)
  }

  files <- utils::unzip(archive, exdir = exdir)

  nc <- grep("\\.nc$", files, ignore.case = TRUE, value = TRUE)

  if (length(nc) != 1L) {
    stop(
      sprintf(
        "esa_landcover_nc(): expected one .nc in %s, found %d",
        archive,
        length(nc)
      ),
      call. = FALSE
    )
  }

  nc

}
