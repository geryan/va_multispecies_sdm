# Crop and mask the monthly abundance cubes to individual countries
#
# Reads the stored `abundance_cubes` target (one GeoTIFF per species, 24 monthly
# layers, 2023-01 to 2024-12, 10 km) and writes one GeoTIFF per species and
# country to outputs/rasters/countries/<ISO3>/abundance_cubes/, with the same
# file names and layer names as the continental cubes.
#
# Countries come from the `atlas_country_boundaries` target (GADM 4.1 level 0,
# resolution 2), and are cut with crop_to_country() (R/), as the country
# rasters are: cells the boundary touches are kept.
#
# Run from the project root: Rscript extras/crop_abundance_cubes_to_countries.R

library(targets)
library(terra)
library(dplyr)
library(purrr)

source("R/crop_to_country.R")

countries <- c(
  "MOZ",
  "AGO"
)

outputdir <- "outputs/rasters/countries"

cube_files <- tar_read(abundance_cubes)

boundaries <- tar_read(atlas_country_boundaries)

jobs <- tidyr::expand_grid(
  iso3 = countries,
  cube_file = cube_files
)

written <- pmap_chr(
  jobs,
  function(iso3, cube_file) {

    v <- boundaries[boundaries$GID_0 == iso3]

    stopifnot(nrow(v) == 1)

    dir <- file.path(
      outputdir,
      iso3,
      "abundance_cubes"
    )

    dir.create(
      dir,
      recursive = TRUE,
      showWarnings = FALSE
    )

    out_file <- file.path(
      dir,
      basename(cube_file)
    )

    rast(cube_file) |>
      crop_to_country(v) |>
      writeRaster(
        filename = out_file,
        overwrite = TRUE
      )

    out_file

  }
)

message(
  "Wrote ",
  length(written),
  " files to ",
  paste(
    unique(dirname(written)),
    collapse = ", "
  )
)
