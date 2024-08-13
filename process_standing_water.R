# process standing water tiles
z <- import_rasts(
  path = "surface_water/",
  ext = ".tif",
  as_list = TRUE
)

# rounded extent of Africa (new_mask)
afext <- c(xmin = -20, xmax = 60, ymin = -40, ymax = 40)

zz <- lapply(
  X = z,
  FUN = function(x){
    ext(x)[1:4] |>
      as.data.frame() |>
      t() |>
      as_tibble()
  }
) |>
  do.call(
    what = "bind_rows",
    args = _
  )

zafidx <- zz |>
  mutate(
    xin = xmin >= afext[1],
    xax = xmax <= afext[2],
    yin = ymin >= afext[3],
    yax = ymax <= afext[4]
  ) |>
  rowwise() |>
  mutate(
    africa = all(xin, xax, yin, yax)
  ) |>
  pull(africa)

zaf <- z[zafidx]

zaf

r1 <- zaf[1:16] |>
  sprc() |>
  merge(
    filename = "sw1.tif",
    overwrite = TRUE
  )

r2 <- zaf[17:32] |>
  sprc() |>
  merge(
    filename = "sw2.tif",
    overwrite = TRUE
  )

r3 <- zaf[33:48] |>
  sprc() |>
  merge(
    filename = "sw3.tif",
    overwrite = TRUE
  )

r4 <- zaf[49:64] |>
  sprc() |>
  merge(
    filename = "sw4.tif",
    overwrite = TRUE
  )


r1 <- rast("sw1.tif")
r2 <- rast("sw2.tif")
r3 <- rast("sw3.tif")
r4 <- rast("sw4.tif")


nm <- rast("new_mask.tif")

rs1 <- resample(r1, nm, filename = "rsw1.tif", overwrite = TRUE)
rs2 <- resample(r2, nm, filename = "rsw2.tif", overwrite = TRUE)
rs3 <- resample(r3, nm, filename = "rsw3.tif", overwrite = TRUE)
rs4 <- resample(r4, nm, filename = "rsw4.tif", overwrite = TRUE)


r <- sprc(rs1, rs2, rs3, rs4) |>
  merge(
    filename = "sw.tif",
    overwrite = TRUE
  )

