#' Download one year of the Mu et al. (2022) Human Footprint and put it on the project grid
#'
#' Mu, H., Li, X., Wen, Y., Huang, J., Du, P., Su, W., Miao, S., Geng, M.
#' (2022). A global record of annual terrestrial Human Footprint dataset from
#' 2000 to 2018. Scientific Data 9:176. doi:10.1038/s41597-022-01284-8
#' Data: figshare 10.6084/m9.figshare.16571064
#'
#' The source is one .zip per year, each holding a single global GeoTIFF in the
#' Mollweide equal-area projection at 1 km. Despite the title the deposit now
#' runs past 2018 -- take the years from `mu_hfp_figshare_manifest()`, never
#' from the paper.
#'
#' ## What persists, and why the target does not re-download
#'
#' The 25 zips are 11 GiB and inflate to 45 GiB of global raster, essentially
#' all of it ocean and other continents. Neither is kept. The only thing that
#' survives a call is the small African raster written to `outputdir`, and that
#' file -- not the download -- is the `format = "file"` target's value.
#'
#' So the first thing this function does is look for a valid output. If one is
#' there it returns immediately, having touched neither figshare nor the disk.
#' That is what makes it safe to delete the sources: {targets} re-runs the
#' branch only when `year`, `url`, `md5` or `new_mask` actually change, and
#' even a forced re-run costs a geometry check rather than 450 MB. It is also
#' what makes an interrupted 25-year run resumable, and what lets the files be
#' fetched outside the pipeline and adopted by it afterwards.
#'
#' Set `keep_source = TRUE` to keep the zip and the global GeoTIFF, which is
#' what you want when inspecting the native grid (see `mu_hfp_grid_report()`).
#'
#' ## Getting onto the project grid
#'
#' The global raster is cropped in its own Mollweide CRS first -- to the
#' *densified* outline of `new_mask` reprojected into Mollweide, so the curved
#' edges of the projection are followed rather than cut across -- and only then
#' reprojected. That keeps ~600 million cells from ever being warped.
#'
#' Reprojection uses `method = "average"`: 1 km to ~4.6 km is aggregation, so
#' each output cell is the mean of the roughly 21 source cells beneath it.
#' Anything interpolating (bilinear and friends) would sample instead of
#' aggregate and alias the result.
#'
#' Values are NOT rescaled here. The Human Footprint index has to be scaled by
#' one constant across the whole series or the between-year signal is destroyed
#' -- see `stack_mu_hfp()`, which is where that happens, because that is the
#' first point at which all the years are in hand.
#'
#' @param year the year to fetch
#' @param url figshare download URL for that year, from the manifest
#' @param md5 expected `supplied_md5` for that year, from the manifest. The zip
#'   is checked against it and a mismatch is an error, not a warning
#' @param new_mask SpatRaster defining the output grid, i.e. `project_mask_5`
#' @param outputdir where the project-grid raster is written. This is the only
#'   output that persists
#' @param workdir scratch space for the zip and the global GeoTIFF. Both are
#'   removed before returning unless `keep_source = TRUE`
#' @param keep_source keep the downloaded zip and the extracted global GeoTIFF
#' @param overwrite rebuild even if a valid output raster is already present
#' @param fill_na fill cells that are NA in the output but have data in
#'   `new_mask` -- coastlines, mostly, where a terrestrial product does not
#'   quite reach the mask edge
#' @param timeout seconds allowed for the download
#' @return path to the project-grid raster, for `tar_target(format = "file")`
#' @author geryan
#' @export
download_mu_hfp <- function(
    year,
    url,
    md5,
    new_mask,
    outputdir = "outputs/raster/mu_hfp",
    workdir = "data/raw/mu_hfp",
    keep_source = FALSE,
    overwrite = FALSE,
    fill_na = TRUE,
    timeout = 1800
  ) {

  year <- as.integer(year)

  dir.create(outputdir, showWarnings = FALSE, recursive = TRUE)

  outfile <- file.path(outputdir, sprintf("mu_hfp_%d.tif", year))

  # --- already have it? -----------------------------------------------------
  # the whole re-runnability story lives here: a valid output short-circuits
  # everything, so the deleted zip and global tif are never missed
  if (file.exists(outfile) && !overwrite) {

    ok <- tryCatch(
      {
        existing <- terra::rast(outfile)
        terra::compareGeom(existing, new_mask, stopOnError = FALSE) &&
          terra::nlyr(existing) == 1L
      },
      error = function(e) FALSE
    )

    if (ok) {
      message(sprintf("%d: already on the project grid, skipping", year))
      return(outfile)
    }

    message(
      sprintf("%d: existing output is unusable or off-grid, rebuilding", year)
    )

  }

  dir.create(workdir, showWarnings = FALSE, recursive = TRUE)
  tifdir <- file.path(workdir, "tif")
  dir.create(tifdir, showWarnings = FALSE, recursive = TRUE)

  zipfile <- file.path(workdir, sprintf("hfp%d.zip", year))
  member <- sprintf("hfp%d.tif", year)
  globaltif <- file.path(tifdir, member)

  md5_ok <- function(path, expected) {
    file.exists(path) &&
      identical(unname(tools::md5sum(path)), tolower(expected))
  }

  # an extracted GeoTIFF that opens is as good as the zip it came from, so a
  # run interrupted between extraction and writing does not re-download 450 MB
  have_tif <- file.exists(globaltif) &&
    !inherits(try(terra::rast(globaltif), silent = TRUE), "try-error")

  # --- download -------------------------------------------------------------
  if (have_tif) {

    message(sprintf("%d: extracted GeoTIFF already present, skipping download", year))

  } else if (md5_ok(zipfile, md5)) {

    message(sprintf("%d: zip already present and checksum matches", year))

  } else {

    if (file.exists(zipfile)) {
      message(sprintf("%d: partial or corrupt zip, re-downloading", year))
      unlink(zipfile)
    }

    old_timeout <- getOption("timeout")
    options(timeout = timeout)
    on.exit(options(timeout = old_timeout), add = TRUE)

    message(sprintf("%d: downloading %s", year, url))

    req <- httr2::request(url) |>
      httr2::req_timeout(timeout) |>
      httr2::req_retry(max_tries = 3)

    # figshare serves this deposit anonymously; a token is only needed if that
    # ever changes, and is read from the environment rather than asked for
    pat <- Sys.getenv("FIGSHARE_PAT")

    if (nzchar(pat)) {
      req <- httr2::req_headers(req, Authorization = paste("token", pat))
    }

    tryCatch(
      httr2::req_perform(req, path = zipfile),
      httr2_http_403 = function(cnd) {
        unlink(zipfile)
        stop(
          sprintf(
            paste(
              "download_mu_hfp(): HTTP 403 for %s.",
              "figshare is refusing the download; this needs a personal access",
              "token in the FIGSHARE_PAT environment variable.",
              "Stopping rather than working around it."
            ),
            url
          ),
          call. = FALSE
        )
      }
    )

    if (!md5_ok(zipfile, md5)) {
      got <- unname(tools::md5sum(zipfile))
      unlink(zipfile)
      stop(
        sprintf(
          "download_mu_hfp(): %d downloaded but md5 is %s, expected %s. Deleted.",
          year, got, md5
        ),
        call. = FALSE
      )
    }

    message(sprintf("%d: downloaded, checksum matches", year))

  }

  # --- extract --------------------------------------------------------------
  if (!have_tif) {

    contents <- utils::unzip(zipfile, list = TRUE)

    if (!member %in% contents$Name) {
      stop(
        sprintf(
          "download_mu_hfp(): %s does not contain %s (holds %s)",
          basename(zipfile), member, paste(contents$Name, collapse = ", ")
        ),
        call. = FALSE
      )
    }

    message(sprintf("%d: extracting %s", year, member))
    utils::unzip(zipfile, files = member, exdir = tifdir, junkpaths = TRUE)

    if (!file.exists(globaltif)) {
      stop(
        sprintf("download_mu_hfp(): %s did not appear after unzip", globaltif),
        call. = FALSE
      )
    }

  }

  # the zip goes only once the GeoTIFF has actually opened
  src <- terra::rast(globaltif)

  if (!keep_source) {
    unlink(zipfile)
  }

  # --- crop in the source CRS, then reproject -------------------------------
  out <- mu_hfp_to_mask(
    src,
    new_mask = new_mask,
    fill_na = fill_na
  )

  names(out) <- sprintf("footprint_%d", year)
  terra::time(out, tstep = "years") <- year

  terra::writeRaster(out, outfile, overwrite = TRUE)

  # release the file handle before deleting what is behind it
  rm(src, out)
  gc(verbose = FALSE)

  if (!keep_source) {
    unlink(globaltif)
  }

  # read it back, so a file that cannot be reopened fails here rather than
  # three targets downstream
  check <- terra::rast(outfile)

  if (!terra::compareGeom(check, new_mask, stopOnError = FALSE)) {
    stop(
      sprintf("download_mu_hfp(): %d was written off the project grid", year),
      call. = FALSE
    )
  }

  message(sprintf("%d: written to %s", year, outfile))

  outfile

}


#' Put a global Mollweide Human Footprint raster on the project grid
#'
#' Split out of `download_mu_hfp()` so the reprojection can be tested on its
#' own. See there for why the crop happens in the source CRS and why the
#' reprojection averages.
#'
#' @param src global SpatRaster in the source projection
#' @param new_mask SpatRaster defining the output grid, i.e. `project_mask_5`
#' @param fill_na fill cells NA in the result but with data in `new_mask`
#' @param buffer metres of slack added around the reprojected outline before
#'   cropping, so reprojection never asks for a cell just outside the crop
#' @return SpatRaster on the grid of `new_mask`
#' @author geryan
#' @export
mu_hfp_to_mask <- function(
    src,
    new_mask,
    fill_na = TRUE,
    buffer = 10000
  ) {

  # the mask outline, densified so that reprojecting it follows the curve of
  # the Mollweide graticule rather than cutting the corners off Africa
  outline <- terra::as.polygons(terra::ext(new_mask), crs = terra::crs(new_mask))
  outline <- terra::densify(outline, interval = 0.25)

  src_ext <- terra::ext(terra::project(outline, terra::crs(src)))
  src_ext <- src_ext + buffer

  cropped <- terra::crop(src, src_ext, snap = "out")

  if (terra::ncell(cropped) == 0) {
    stop(
      "mu_hfp_to_mask(): the mask does not overlap the source raster",
      call. = FALSE
    )
  }

  # 1 km -> ~4.6 km is aggregation, so average rather than interpolate
  out <- terra::project(
    cropped,
    new_mask,
    method = "average"
  )

  out <- terra::mask(out, new_mask)

  if (fill_na) {

    gap <- terra::global(
      is.na(out) & !is.na(new_mask),
      fun = "sum",
      na.rm = TRUE
    )[1, 1]

    # a terrestrial product will not always reach the last coastal cell of a
    # mask built from something else; fill those rather than carry NAs into the
    # design matrix, exactly as bias_tt_5 does
    if (gap > 0) {
      message(sprintf("filling %d cell(s) NA inside the mask", gap))
      out <- fill_na_with_nearest_mean(out, maxRadiusCell = 50)
      out <- terra::mask(out, new_mask)
    }

  }

  out

}
