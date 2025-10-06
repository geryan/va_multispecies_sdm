prepare_raster_data(){


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

  mmv[["ag_microclim"]] <- mechvals

  aridvals <- values(mmv[["arid"]])

  mmv[["arid"]] <- round(aridvals, digits = 0)

  dist_from_sea <- mmv[[3]]
  idx <- is.na(values(dist_from_sea))
  dist_from_sea[idx] <- NA
  dist_from_sea[!idx] <- 1


  z <- vect(dist_from_sea)

  dist_from_sea <- distance(dist_from_sea)


  modlyr <- writereadrast(
    mmv,
    "data/raster/static_vars_agg_mech_nonzero.tif"
  )


dist_from_sea <- mmv[[3]]
idx <- is.na(values(dist_from_sea))
dist_from_sea[idx] <- NA
dist_from_sea[!idx] <- 1

library(terra)
r <- dist_from_sea # replace with your raster

# Step 1: Make a mask of the main shape (non-NA areas)
main_mask <- r

# Step 2: Erode the mask to exclude edge cells (so only fully surrounded internal cells remain)
# The size of erosion determines how far from the edge you want to be considered "internal"
eroded_mask <- focal(main_mask, w=matrix(1,3,3), fun=min, na.rm = TRUE)

# Step 3: Identify internal NA cells
internal_NA <- is.na(r) & (eroded_mask == 1)


# Prepare to fill only internal NAs
to_fill <- classify(r, cbind(NA, -9999)) # Temporarily tag NAs as -9999

# Use focal to fill -9999 values with mean of neighbors
filled <- focal(to_fill, w=matrix(1,3,3), fun=function(x, ...) {
  center <- x[5] # center pixel
  if(center == -9999) {
    mean(x[x != -9999 & !is.na(x)], na.rm=TRUE)
  } else {
    center
  }
}, na.policy="omit")

# Only update internal NA cells
r[internal_NA] <- filled[internal_NA]


rnaidx <- which(is.na(values(r)))
ridx  <- which(!is.na(values(r)))
dfs <- r
dfs[rnaidx] <- 0
dfs[ridx] <- NA

dist_from_sea <- distance(dfs)
