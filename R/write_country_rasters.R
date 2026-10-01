#' Write one country's prediction and travel-time rasters
#'
#' Crops each layer to the country with crop_to_country() and writes it to
#' `<outputdir>/<ISO3>/`:
#'
#' - abundance.tif, distribution.tif, cv.tif: one band per species, named for
#'   the species, at the prediction resolution
#' - traveltime.tif: travel time from research facilities, in minutes, at its
#'   native ~1 km resolution
#'
#' Countries outside the prediction grid still get files, all NA, so every
#' country has the same four.
#'
#' @param iso3 three-letter country code, matched to `boundaries$GID_0`
#' @param boundaries SpatVector of countries, from geodata::gadm(level = 0)
#' @param abundance,distribution,cv SpatRaster, one layer per species
#' @param traveltime SpatRaster, travel time in minutes
#' @param outputdir root directory; one subdirectory per country
#' @return the four file paths
#' @author geryan
#' @export
write_country_rasters <- function(
    iso3,
    boundaries,
    abundance,
    distribution,
    cv,
    traveltime,
    outputdir = "outputs/rasters/countries"
){

  v <- boundaries[boundaries$GID_0 == iso3]

  if (nrow(v) != 1) {
    stop(
      sprintf(
        "write_country_rasters(): %d boundaries for %s",
        nrow(v),
        iso3
      ),
      call. = FALSE
    )
  }

  dir <- file.path(
    outputdir,
    iso3
  )

  dir.create(
    dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  layers <- list(
    abundance = abundance,
    distribution = distribution,
    cv = cv,
    traveltime = traveltime
  )

  files <- file.path(
    dir,
    paste0(names(layers), ".tif")
  )

  for (i in seq_along(layers)) {

    r <- crop_to_country(
      layers[[i]],
      v
    )

    if (names(layers)[i] == "traveltime") {
      names(r) <- "travel_time_minutes"
    }

    terra::writeRaster(
      r,
      filename = files[i],
      overwrite = TRUE
    )

  }

  files

}
