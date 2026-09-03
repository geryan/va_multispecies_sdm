# Save the arabiensis MESS and ExDet objects (targets at lines 1302-1355 of
# _targets.R) out of the targets store as standalone archives in
# arabiensis_novelty/, in two versions:
#
#   <target>.tar.gz         as built by the pipeline, whole of Africa
#   <target>_masked.tar.gz  masked to the core arabiensis expert range
#                           (expert_offset_maps_10[["arabiensis"]] == 1)
#
# The mask layer is 10 km and the novelty layers 5 km, so the mask is resampled
# (nearest neighbour) onto the novelty grid.
#
# Reading and plotting the saved objects:
# arabiensis_novelty/arabiensis_novelty_plots.R
#
# Run from the project root.

library(targets)
library(targets.utils) # pak::pak("geryan/targets.utils")
library(terra)

out_dir <- "arabiensis_novelty"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

target_names <- c(
  "africa_mess_arabiensis",
  "africa_mess_nosea_arabiensis",
  "africa_exdet_arabiensis",
  "africa_exdet_nosea_arabiensis"
)


## Unmasked: tar_terra_nested() already stores each target as a tarball of
## GeoTIFFs plus a skeleton, so a straight copy is a complete snapshot.

for (n in target_names) {
  file.copy(
    from = file.path("_targets", "objects", n),
    to = file.path(out_dir, paste0(n, ".tar.gz")),
    overwrite = TRUE
  )
}


## Masked to the core arabiensis expert range

novelty <- lapply(target_names, tar_read_raw)
names(novelty) <- target_names

expert <- tar_read(expert_offset_maps_10)[["arabiensis"]]

# TRUE where a cell is kept; resampled to the 5 km novelty grid
template <- novelty[[1]][[1]]
keep <- resample(expert == 1, template, method = "near")
keep[keep == 0] <- NA

mask_result <- function(x) {
  out <- lapply(
    x,
    # write to file so the masked rasters are file-backed, keeping them clear of
    # the terra multi-block in-memory write bug (see R/mess_safe.R)
    function(r) mask(r, keep, filename = tempfile(fileext = ".tif"))
  )
  class(out) <- class(x)
  out
}

for (n in target_names) {
  write_terra_nested(
    object = mask_result(novelty[[n]]),
    path = file.path(out_dir, paste0(n, "_masked.tar.gz")),
    raster_filetype = "GTiff",
    raster_gdal = NULL,
    raster_datatype = NULL,
    vector_filetype = "GPKG",
    vector_gdal = NULL,
    raster_args = list(),
    vector_args = list()
  )
}
