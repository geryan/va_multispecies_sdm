library("targets.utils")
tl()
# alt layer with low values for mechanistic replaced with exp(-30)
# this also is aggregated by a factor of 10 for speed
static_vars <- rast("~/Documents/tki_work/vector_atlas/africa_spatial_data/outputs/raster/africa_static_vars_std.tif")

mmv <- aggregate(
  static_vars,
  fact = 10,
  filename = "data/raster/static_vars_agg.tif",
  overwrite = TRUE,
  cores = 8
)

mechvals <- values(mmv[["ag_microclim"]])

lowidx <- which(mechvals <= exp(-30))

mechvals[lowidx] <- exp(-30)

ag_microclim <- mmv[["ag_microclim"]]
ag_microclim[] <-mechvals

# aridvals <- values(mmv[["arid"]])
#
# mmv[["arid"]] <- round(aridvals, digits = 0)

r <- mmv[[3]]
idx <- is.na(values(r))
r[idx] <- NA
r[!idx] <- 1


eroded_mask <- focal(
  r,
  w = matrix(1,3,3),
  fun = min,
  na.rm = TRUE
)

internal_NA <- is.na(r) & (eroded_mask == 1)

to_fill <- classify(
  r,
  cbind(NA, -9999)
)

filled <- focal(
  to_fill,
  w = matrix(1,3,3),
  fun = function(x, ...) {
    center <- x[5] # center pixel
    if(center == -9999) {
      mean(x[x != -9999 & !is.na(x)], na.rm=TRUE)
    } else {
      center
    }
  },
  na.policy="omit"
)

r[internal_NA] <- filled[internal_NA]


rnaidx <- which(is.na(values(r)))
ridx  <- which(!is.na(values(r)))
dfs <- r
dfs[rnaidx] <- 0
dfs[ridx] <- NA

dist_from_sea <- distance(dfs) |>
  mask(mmv[[1]]) |>
  set_layer_names(
    layernames = "distance_from_sea"
  ) |>
  scale()


z <- c(
  mmv,
  dist_from_sea
)


static_vars_agg_mech_nonzero_dist_from_sea <- c(
  ag_microclim,
  mmv[[2:25]],
  dist_from_sea
)

writeRaster(
  x = static_vars_agg_mech_nonzero_dist_from_sea,
  filename = "data/raster/static_vars_agg_mech_nonzero_dist_from_sea.tif",
  overwrite = TRUE
)

static_vars_agg_mech_nonzero_dist_from_sea <- rast("data/raster/static_vars_agg_mech_nonzero_dist_from_sea.tif")


static_vars_agg_mech_nonzero_dist_from_sea
