make_new_mask <- function(
  covariate_rasters
){


  z <- covariate_rasters[[1]]

  idx <- which(!is.na(values(x)))

  z[idx] <- 1

  writereadrast(
    z,
    filename = "data/new_mask.tif"
  )

}
