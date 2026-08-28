#' Download one year of ESA CCI / C3S land cover from the Copernicus CDS
#'
#' Wraps the `satellite-land-cover` dataset on the Copernicus Climate Data
#' Store: global annual land cover at 300 m (1/360 degree), 1992-2022, in the
#' 22-class UN FAO LCCS legend.
#'
#' The CDS splits the record across two year-locked versions, so `version` is
#' derived from `year` rather than passed in:
#'   v2_0_7cds  1992-2015  (ESA CCI)
#'   v2_1_1     2016-2022  (C3S continuation)
#' Asking for a version/year pair that does not exist is rejected by the API,
#' hence esa_landcover_version() below.
#'
#' `area` is the CDS bounding box in its own order -- c(North, West, South,
#' East) -- so the subset happens server side and we never handle the global
#' 129600 x 64800 grid. Downstream still crops defensively, so it is safe if
#' the server ever ignores the request and returns a global file.
#'
#' Confirmed honoured: the delivered file is named
#' ESACCI-LC-...-v2.0.7cds.area-subset.<N>.<E>.<S>.<W>.nc and opens at
#' 26475 x 25575 (677,098,125 cells) for the African box, against
#' 129600 x 64800 global. 446 MB zipped per year, 13 GB for 1992-2022.
#'
#' RE-DERIVING THE REQUEST SPEC RATHER THAN GUESSING AT IT. The CDS publishes
#' the authoritative input schema and the valid parameter combinations, which
#' is how the version/year lock below was established. No token needed:
#'   curl .../api/retrieve/v1/processes/satellite-land-cover
#'     -> inputs: names, enums and defaults for every parameter
#'   curl .../api/retrieve/v1/processes/satellite-land-cover/constraints \
#'     -X POST -H 'Content-Type: application/json' -d '{"inputs":{"year":["2016"]}}'
#'     -> the values still selectable given that choice
#' (base https://cds.climate.copernicus.eu). The same two endpoints work for
#' any CDS dataset -- worth reaching for before trusting a remembered API.
#'
#' CREDENTIALS. Needs a CDS Personal Access Token, once per machine:
#'   1. register at https://cds.climate.copernicus.eu and copy the token from
#'      https://cds.climate.copernicus.eu/profile
#'   2. ecmwfr::wf_set_key(key = "<token>")
#'   3. accept the ESA CCI and VITO licences on the dataset page, otherwise
#'      every request 403s:
#'      https://cds.climate.copernicus.eu/datasets/satellite-land-cover?tab=download
#'
#' @return path to the downloaded archive, for tar_target(format = "file")
download_esa_landcover <- function(
    year,
    dest_dir = "data/raster/esa_landcover",
    area = NULL, # c(N, W, S, E); NULL = global
    user = "ecmwfr",
    time_out = 7200,
    overwrite = FALSE
  ){

  year <- as.integer(year)

  dir.create(dest_dir, showWarnings = FALSE, recursive = TRUE)

  target <- sprintf("esa_landcover_%d.zip", year)
  outfile <- file.path(dest_dir, target)

  # Downloads are slow and queued server side. Never re-fetch a year we
  # already hold -- this is what makes an interrupted 31-year run resumable.
  if (file.exists(outfile) && !overwrite) {
    message(sprintf("%d: already downloaded, skipping", year))
    return(outfile)
  }

  request <- list(
    dataset_short_name = "satellite-land-cover",
    variable = "all",
    year = as.character(year),
    version = esa_landcover_version(year),
    target = target
  )

  if (!is.null(area)) {
    request$area <- as.numeric(area)
  }

  message(sprintf("%d: requesting from CDS (%s)", year, request$version))

  ecmwfr::wf_request(
    request = request,
    user = user,
    transfer = TRUE,
    path = dest_dir,
    time_out = time_out,
    verbose = TRUE
  )

  if (!file.exists(outfile)) {
    stop(sprintf("download_esa_landcover(): CDS returned no file for %d", year))
  }

  outfile

}

#' CDS version string covering a given land cover year
esa_landcover_version <- function(year){

  year <- as.integer(year)

  if (year >= 1992 && year <= 2015) {
    "v2_0_7cds"
  } else if (year >= 2016 && year <= 2022) {
    "v2_1_1"
  } else {
    stop(
      sprintf(
        "esa_landcover_version(): no CDS land cover for %d (available 1992-2022)",
        year
      ),
      call. = FALSE
    )
  }

}

#' CDS `area` box -- c(N, W, S, E) -- covering a reference raster
#'
#' Padded outward by `pad` degrees so that whatever the server snaps the
#' subset to still fully contains the reference grid.
esa_landcover_area <- function(ref, pad = 0.5){

  e <- as.vector(terra::ext(ref))

  c(
    min(90, e[["ymax"]] + pad),
    max(-180, e[["xmin"]] - pad),
    max(-90, e[["ymin"]] - pad),
    min(180, e[["xmax"]] + pad)
  )

}
