gt_lndcvr <- function(
    vars,
    path = "data/raster/geodata/"
){

  fs <- list.files(
    path = sprintf(
      "%s/landuse/",
      path
    ),
    pattern = "WorldCover"
  )


  fns <- fs |>
    sub(
      pattern = "WorldCover_",
      replacement = "",
      x = _
    ) |>
    sub(
      pattern = "_30s.tif",
      replacement = "",
      x = _
    )

  idx <- match(vars, fns)

  ff <- list.files(
    path = sprintf(
      "%s/landuse/",
      path
    ),
    pattern = "WorldCover",
    full.names = TRUE
  )


  r <- sapply(
    ff[idx],
    rast
  ) |>
    rast()

  names(r) <- vars

  r

}
