# Digitise the An. coluzzii expert range from Wiebe et al. (2017), Fig. 1a:
# the area between the solid range line and the west African coast.
#
# Wiebe A, Longbottom J, Gleave K, et al. (2017) Geographical distributions of
# African malaria vector sibling species and evidence for insecticide
# resistance. Malaria Journal 16:85. doi:10.1186/s12936-017-1734-y
#
# Source image: the web version of Fig. 1 (784 x 985 px), saved as
# data/vector/other_expert_maps/wiebe2017/wiebe2017_fig1.png. Panel a is about
# 4.2 px per degree, so the line is good to about +/- 1 px (~25 km).
#
# Steps:
# 1. georeference panel a: an affine fit from lon/lat (the figure is plain
#    lon/lat) to pixels, matching GADM country borders and coasts to the
#    figure's grey/black lines;
# 2. trace the solid range line as the least-cost path through dark,
#    unsaturated pixels between where it meets the coast in the north
#    (Mauritania) and the south (Angola), kept away from the white exterior so
#    it can't follow the dashed range-plus-buffer edge;
# 3. close the line over the Atlantic and clip to mainland Africa (islands
#    dropped, at the user's request).
#
# Run from the project root: Rscript extras/digitise_wiebe2017_coluzzii.R
# Writes data/vector/other_expert_maps/wiebe2017/coluzzii.shp and a check
# figure, coluzzii_check.png, next to it.

library(terra)
library(igraph)

out_dir <- "data/vector/other_expert_maps/wiebe2017"
img <- png::readPNG(
  file.path(
    out_dir,
    "wiebe2017_fig1.png"
  )
)

# panel a lies inside the frame lines at rows/cols 2 and 329/331
a <- img[4:328, 4:330, 1:3]
nr <- dim(a)[1]
nc <- dim(a)[2]
mx <- pmax(a[, , 1], a[, , 2], a[, , 3])
mn <- pmin(a[, , 1], a[, , 2], a[, , 3])

# --- 1. georeference -------------------------------------------------------

# Africa from the cached full-resolution GADM level-1 files
africa <- list.files(
  "data/raster/geodata/gadm",
  pattern = "_1_pk\\.rds$",
  full.names = TRUE
) |>
  lapply(
    function(f) aggregate(
      unwrap(readRDS(f)),
      "COUNTRY"
    )
  ) |>
  vect()

# distance (px) from each pixel to the nearest grey/black line pixel
line_px <- mx < 0.6 & (mx - mn) < 0.15
r <- rast(
  nrows = nr,
  ncols = nc,
  xmin = 0,
  xmax = nc,
  ymin = -nr,
  ymax = 0,
  crs = "local"
)
values(r) <- as.vector(t(ifelse(line_px, 1, NA)))
D <- as.matrix(
  distance(r),
  wide = TRUE
)

# bilinear lookup of D at pixel coordinates (cell centres at col - 0.5, row - 0.5)
lookup_d <- function(px, py) {
  x <- px + 0.5
  y <- py + 0.5
  x0 <- floor(x)
  y0 <- floor(y)
  fx <- x - x0
  fy <- y - y0
  out <- rep(NA_real_, length(px))
  i <- which(x0 >= 1 & x0 < nc & y0 >= 1 & y0 < nr)
  out[i] <- D[cbind(y0[i], x0[i])] * (1 - fx[i]) * (1 - fy[i]) +
    D[cbind(y0[i], x0[i] + 1)] * fx[i] * (1 - fy[i]) +
    D[cbind(y0[i] + 1, x0[i])] * (1 - fx[i]) * fy[i] +
    D[cbind(y0[i] + 1, x0[i] + 1)] * fx[i] * fy[i]
  out
}

border_pts <- as.lines(africa) |>
  crop(ext(-20, 60, -36, 38)) |>
  simplifyGeom(tolerance = 0.02) |>
  densify(interval = 15000) |>
  as.points() |>
  crds()
set.seed(1)
border_pts <- border_pts[sample(nrow(border_pts), 6000), ]

# affine px = A %*% (lon, lat) + b, on centred and scaled lon/lat
z <- scale(border_pts)
cen <- attr(z, "scaled:center")
sc <- attr(z, "scaled:scale")
to_px <- function(th, z) {
  cbind(
    th[1] + th[3] * z[, 1] + th[5] * z[, 2],
    th[2] + th[6] * z[, 1] + th[4] * z[, 2]
  )
}

# start from three control points read off the image: Dakar, Cape Agulhas and
# the southern tip of Madagascar
cp_ll <- rbind(c(-17.5, 14.7), c(20, -34.8), c(46.3, -25.6))
cp_px <- rbind(c(23.3, 100), c(180, 315), c(289.3, 271.3))
cp_z <- sweep(sweep(cp_ll, 2, cen), 2, sc, "/")
bx <- solve(cbind(1, cp_z), cp_px[, 1])
by <- solve(cbind(1, cp_z), cp_px[, 2])
th0 <- c(bx[1], by[1], bx[2], by[3], bx[3], by[2])

loss <- function(th, cap) {
  p <- to_px(th, z)
  dd <- lookup_d(p[, 1], p[, 2])
  if (mean(!is.na(dd)) < 0.3) return(1e6)
  mean(pmin(dd, cap), na.rm = TRUE)
}
fit <- optim(th0, loss, cap = 8, control = list(maxit = 5000))
fit <- optim(fit$par, loss, cap = 3, control = list(maxit = 5000))
fit <- optim(fit$par, loss, cap = 2, method = "BFGS")
th <- fit$par
fit_d <- lookup_d(to_px(th, z)[, 1], to_px(th, z)[, 2])
cat(
  "georeference: median border-to-line distance",
  round(median(fit_d, na.rm = TRUE), 2),
  "px;",
  round(100 * mean(fit_d < 1, na.rm = TRUE)),
  "% within 1 px\n"
)

px_to_lonlat <- function(px, py) {
  M <- matrix(c(th[3], th[6], th[5], th[4]), 2)
  zz <- solve(M, rbind(px - th[1], py - th[2]))
  cbind(
    lon = zz[1, ] * sc[1] + cen[1],
    lat = zz[2, ] * sc[2] + cen[2]
  )
}

# --- 2. trace the solid line ----------------------------------------------

white <- mx > 0.9 & (mx - mn) < 0.12
near_white <- white
for (dr in -2:2) for (dc in -2:2) {
  shifted <- matrix(FALSE, nr, nc)
  rr <- max(1, 1 + dr):min(nr, nr + dr)
  cc <- max(1, 1 + dc):min(nc, nc + dc)
  shifted[rr - dr, cc - dc] <- white[rr, cc]
  near_white <- near_white | shifted
}
# cheap through dark grey/black; saturated colours (the dark purple of the
# probability surface) are expensive, as is anything within 2 px of white
cost <- exp(10 * mx) * (1 + 20 * (mx - mn)) + ifelse(near_white, 2000, 0)

id <- matrix(seq_len(nr * nc), nr, nc)
edges <- lapply(
  list(c(0, 1, 1), c(1, 0, 1), c(1, 1, sqrt(2)), c(1, -1, sqrt(2))),
  function(d) {
    r1 <- 1:(nr - d[1])
    c1 <- if (d[2] >= 0) 1:(nc - d[2]) else (1 - d[2]):nc
    from <- as.vector(id[r1, c1])
    to <- as.vector(id[r1 + d[1], c1 + d[2]])
    data.frame(
      from = from,
      to = to,
      weight = d[3] * (cost[from] + cost[to]) / 2
    )
  }
) |>
  do.call(rbind, args = _)
g <- graph_from_data_frame(
  edges,
  directed = FALSE,
  vertices = data.frame(name = seq_len(nr * nc))
)

# ends of the line at the coast (col, row of panel a)
start <- c(25, 80)
end <- c(148, 232)
v <- shortest_paths(
  g,
  from = id[start[2], start[1]],
  to = id[end[2], end[1]],
  output = "vpath"
)$vpath[[1]] |>
  names() |>
  as.integer()
path <- cbind(
  col = (v - 1) %/% nr + 1,
  row = (v - 1) %% nr + 1
)

# smooth out the pixel staircase (7-px moving average, ends kept)
smooth7 <- function(x) {
  s <- stats::filter(x, rep(1 / 7, 7), sides = 2)
  ifelse(is.na(s), x, s)
}
line_ll <- px_to_lonlat(
  smooth7(path[, "col"]) - 0.5,
  smooth7(path[, "row"]) - 0.5
)

# --- 3. polygon ------------------------------------------------------------

# extend each end 2 degrees out to sea along the line's direction, then close
# the polygon far out in the Atlantic
n <- nrow(line_ll)
extend <- function(p_end, p_in) {
  d <- p_end - p_in
  p_end + 2 * d / sqrt(sum(d^2))
}
north_out <- extend(line_ll[1, ], line_ll[min(8, n), ])
south_out <- extend(line_ll[n, ], line_ll[max(1, n - 7), ])
ring <- rbind(
  c(-35, north_out[2]),
  north_out,
  line_ll,
  south_out,
  c(-35, south_out[2]),
  c(-35, north_out[2])
)
closed <- vect(
  ring,
  type = "polygons",
  crs = "EPSG:4326"
)

mainland <- aggregate(africa) |>
  disagg()
mainland <- mainland[which.max(expanse(mainland))] |>
  fillHoles()

coluzzii <- intersect(closed, mainland) |>
  aggregate()
coluzzii$species <- "coluzzii"
coluzzii$source <- "Wiebe et al. 2017 Malar J 16:85, Fig. 1a, solid range line"
cat("area:", round(expanse(coluzzii, unit = "km") / 1e6, 2), "million km2\n")
print(ext(coluzzii))

writeVector(
  coluzzii,
  file.path(out_dir, "coluzzii.shp"),
  overwrite = TRUE
)

# --- check figure: the polygon and traced line over the georeferenced panel --

panel <- rast(
  a * 255,
  extent = ext(0, nc, -nr, 0)
)
corners <- px_to_lonlat(c(0, nc, 0), c(0, 0, nr))
panel_ll <- rast(
  a[, , 1:3] * 255
)
# with no rotation to speak of, the panel's lon/lat extent is its corners
ext(panel_ll) <- ext(
  corners[1, "lon"],
  corners[2, "lon"],
  corners[3, "lat"],
  corners[1, "lat"]
)
crs(panel_ll) <- "EPSG:4326"
RGB(panel_ll) <- 1:3

png(
  file.path(out_dir, "coluzzii_check.png"),
  width = 1600,
  height = 1600,
  res = 150
)
plotRGB(panel_ll, mar = c(2, 2, 2, 2), axes = TRUE, main = "An. coluzzii, Wiebe et al. 2017 Fig. 1a")
polys(coluzzii, col = rgb(0, 0.6, 1, 0.25), border = rgb(0, 0.4, 1), lwd = 2)
lines(line_ll, col = "red", lwd = 1)
dev.off()
