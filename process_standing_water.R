# process standing water tiles

z <- import_rasts(
  path = "data/raster/surface_water/",
  ext = ".tif",
  as_list = TRUE
)


zz <- lapply(
  X = z,
  FUN = function(x){
    ext(x)[1:4] |>
      as.data.frame() |>
      t()
  }
)

unlist(zz)
