# Nigeria extract of the environmental covariates and An. gambiae records.
#
# Writes to nigeria_environmental_space/:
#   - footprint_2013_2022.tif: Mu et al. Human Footprint, raw units (0-50),
#     one layer per year
#   - landcover_<class>_2013_2022.tif: ESA CCI land-cover class proportions
#     (0-1, all 11 classes sum to 1), one file per class, one layer per year
#   - nigeria_boundary.gpkg: GADM level 0, the boundary used to crop and mask
#   - gambiae_records_nigeria_2013_2022.csv: non-inferred An. gambiae s.s.
#     records (month, year, latitude, longitude). Month and year are the
#     survey's midpoint (`model_date`), as in the model.
#
# 2013-2022 is the last 10 years both series cover: land cover runs 1992-2022
# and footprint 2000-2024.
#
# Reads stored targets only. Run from the repo root:
#   Rscript extras/make_nigeria_environmental_space.R

library(targets)
library(terra)
library(dplyr)
library(readr)

years <- 2013:2022
outdir <- "nigeria_environmental_space"
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
nga <- tar_read(nga)
writeVector(
  nga,
  filename = file.path(
    outdir,
    "nigeria_boundary.gpkg"
  ),
  overwrite = TRUE
)

crop_mask_nga <- function(r) {
  r |>
    crop(nga) |>
    mask(nga)
}

# land cover
for (cls in landcover_classes) {
  r <- rast(
    sprintf(
      "outputs/raster/esa_landcover_proportion/esa_landcover_proportion_%s.tif",
      cls
    )
  )
  r <- r[[time(r) %in% years]]
  stopifnot(identical(as.integer(time(r)), years))

  writeRaster(
    crop_mask_nga(r),
    filename = file.path(
      outdir,
      sprintf(
        "landcover_%s_%s_%s.tif",
        cls,
        min(years),
        max(years)
      )
    ),
    names = sprintf(
      "%s_%s",
      cls,
      years
    ),
    overwrite = TRUE
  )
}

# footprint, raw units (the per-year files, not the scaled mu_hfp_all)
footprint <- rast(
  sprintf(
    "outputs/raster/mu_hfp/mu_hfp_%s.tif",
    years
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
  years
)
time(
  footprint,
  tstep = "years"
) <- years

writeRaster(
  crop_mask_nga(footprint),
  filename = file.path(
    outdir,
    sprintf(
      "footprint_%s_%s.tif",
      min(years),
      max(years)
    )
  ),
  overwrite = TRUE
)

# records
gambiae <- tar_read(model_data_records) |>
  filter(
    species == "gambiae",
    !inferred,
    !is.na(model_date)
  ) |>
  mutate(
    year = as.integer(format(model_date, "%Y")),
    month = as.integer(format(model_date, "%m"))
  ) |>
  filter(year %in% years)

gambiae_v <- vect(
  gambiae,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)
in_nga <- relate(
  gambiae_v,
  nga,
  "intersects"
)[, 1]

records <- gambiae[in_nga, ] |>
  select(
    month,
    year,
    latitude,
    longitude
  ) |>
  arrange(
    year,
    month
  )

write_csv(
  records,
  file.path(
    outdir,
    sprintf(
      "gambiae_records_nigeria_%s_%s.csv",
      min(years),
      max(years)
    )
  )
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
      min(years),
      max(years)
    )
  )
)
fp <- rast(
  file.path(
    outdir,
    sprintf(
      "footprint_%s_%s.tif",
      min(years),
      max(years)
    )
  )
)

cat("\nLand-cover layers:", nlyr(lc), "| footprint layers:", nlyr(fp), "\n")
cat("Land-cover range:", range(minmax(lc)), "\n")
cat("Footprint range:", range(minmax(fp)), "\n")

lc_2022_sum <- sum(lc[[grepl("_2022$", names(lc))]])
cat("Land-cover class sum in 2022, range:", minmax(lc_2022_sum)[, 1], "\n")

near_nga <- relate(
  gambiae_v,
  buffer(
    nga,
    width = 5000
  ),
  "intersects"
)[, 1]
cat("\nRecords inside Nigeria:", nrow(records), "\n")
cat(
  "Records within 5 km outside the boundary (not included):",
  sum(near_nga & !in_nga),
  "\n"
)
print(count(records, year))
