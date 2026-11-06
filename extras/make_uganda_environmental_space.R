# Uganda extract of the environmental covariates, every year available.
#
# Writes to uganda_environmental_space/:
#   - footprint_2000_2024.tif: Mu et al. Human Footprint, raw units (0-50),
#     one layer per year
#   - landcover_<class>_1992_2022.tif: ESA CCI land-cover class proportions
#     (0-1, all 11 classes sum to 1), one file per class, one layer per year
#   - uganda_boundary.gpkg: GADM level 0, the boundary used to crop and mask
#
# Both are at 5 km (0.0417 deg), the finest held in the project: the raw 1 km
# footprint downloads were not kept, and the 300 m ESA source holds one class
# per cell rather than proportions.
#
# Reads stored targets and outputs/raster/ only. Run from the repo root:
#   Rscript extras/make_uganda_environmental_space.R

library(targets)
library(terra)

landcover_years <- 1992:2022
footprint_years <- 2000:2024
outdir <- "uganda_environmental_space"
dir.create(
  outdir,
  showWarnings = FALSE
)

landcover_classes <- c(
  "crop_other",
  "irrigated",
  "tree",
  "shrubland",
  "grassland",
  "wetland",
  "mangrove",
  "urban",
  "water",
  "bare",
  "sparse"
)

# boundary
uga <- tar_read(uga)
writeVector(
  uga,
  filename = file.path(
    outdir,
    "uganda_boundary.gpkg"
  ),
  overwrite = TRUE
)

crop_mask_uga <- function(r) {
  r |>
    crop(uga) |>
    mask(uga)
}

# land cover
for (cls in landcover_classes) {
  r <- rast(
    sprintf(
      "outputs/raster/esa_landcover_proportion/esa_landcover_proportion_%s.tif",
      cls
    )
  )
  stopifnot(identical(as.integer(time(r)), landcover_years))

  writeRaster(
    crop_mask_uga(r),
    filename = file.path(
      outdir,
      sprintf(
        "landcover_%s_%s_%s.tif",
        cls,
        min(landcover_years),
        max(landcover_years)
      )
    ),
    names = sprintf(
      "%s_%s",
      cls,
      landcover_years
    ),
    overwrite = TRUE
  )
}

# footprint, raw units (the per-year files, not the scaled mu_hfp_all)
footprint <- rast(
  sprintf(
    "outputs/raster/mu_hfp/mu_hfp_%s.tif",
    footprint_years
  )
)
stopifnot(
  compareGeom(
    footprint,
    r
  )
)
names(footprint) <- sprintf(
  "footprint_%s",
  footprint_years
)
time(
  footprint,
  tstep = "years"
) <- footprint_years

writeRaster(
  crop_mask_uga(footprint),
  filename = file.path(
    outdir,
    sprintf(
      "footprint_%s_%s.tif",
      min(footprint_years),
      max(footprint_years)
    )
  ),
  overwrite = TRUE
)

# report
cat("\nFiles written to", outdir, ":\n")
print(list.files(outdir))

lc <- rast(
  file.path(
    outdir,
    sprintf(
      "landcover_%s_%s_%s.tif",
      landcover_classes,
      min(landcover_years),
      max(landcover_years)
    )
  )
)
fp <- rast(
  file.path(
    outdir,
    sprintf(
      "footprint_%s_%s.tif",
      min(footprint_years),
      max(footprint_years)
    )
  )
)

cat("\nGrid:", nrow(fp), "x", ncol(fp), "cells at", res(fp)[1], "deg\n")
cat("Land-cover layers:", nlyr(lc), "| footprint layers:", nlyr(fp), "\n")
cat("Land-cover range:", range(minmax(lc)), "\n")
cat("Footprint range:", range(minmax(fp)), "\n")

for (yr in range(landcover_years)) {
  lc_sum <- sum(lc[[grepl(sprintf("_%s$", yr), names(lc))]])
  cat("Land-cover class sum in", yr, ", range:", minmax(lc_sum)[, 1], "\n")
}

cat(
  "Cells inside Uganda: footprint",
  global(!is.na(fp[[1]]), "sum")[[1]],
  "| land cover",
  global(!is.na(lc[[1]]), "sum")[[1]],
  "\n"
)
