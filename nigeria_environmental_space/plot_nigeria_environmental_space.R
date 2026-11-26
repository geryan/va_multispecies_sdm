# Simple look at the Nigeria environmental data and An. gambiae records.
# Run with this folder as the working directory.
#
# footprint_2013_2022.tif         Human Footprint (Mu et al.), 0-50, one layer per year
# landcover_<class>_2013_2022.tif ESA CCI land-cover proportion, 0-1, one layer per year;
#                                 the 11 classes sum to 1 in each cell
# gambiae_records_...csv          An. gambiae s.s. records (midpoint month and year)

library(terra)

# read ----------------------------------------------------------------------

nigeria <- vect("nigeria_boundary.gpkg")

footprint <- rast("footprint_2013_2022.tif")

landcover_files <- list.files(
  pattern = "^landcover_.*\\.tif$"
)
landcover <- lapply(
  landcover_files,
  rast
)
names(landcover) <- sub(
  "^landcover_(.*)_2013_2022\\.tif$",
  "\\1",
  landcover_files
)

records <- read.csv("gambiae_records_nigeria_2013_2022.csv")
records_v <- vect(
  records,
  geom = c("longitude", "latitude"),
  crs = "EPSG:4326"
)

footprint
landcover$tree
head(records)

# records -------------------------------------------------------------------

plot(
  nigeria,
  main = "An. gambiae records, 2013-2022"
)
plot(
  records_v,
  "year",
  type = "classes",
  add = TRUE,
  pch = 16
)

barplot(
  table(
    factor(
      records$year,
      levels = 2013:2022
    )
  ),
  main = "Records per year",
  xlab = "Year",
  ylab = "Records"
)

# each variable in the last year (2022) --------------------------------------

last_year <- c(
  list(footprint = footprint[[nlyr(footprint)]]),
  lapply(
    landcover,
    function(r) r[[nlyr(r)]]
  )
) |>
  rast()

plot(
  last_year,
  nc = 4,
  axes = FALSE,
  fun = function() {
    lines(nigeria)
    points(
      records_v,
      pch = 16,
      cex = 0.6,
      col = "red"
    )
  }
)

# the footprint over time ----------------------------------------------------

plot(
  footprint,
  range = range(minmax(footprint))
)

# change in footprint over the decade
plot(
  footprint[["footprint_2022"]] - footprint[["footprint_2013"]],
  main = "Footprint change, 2013 to 2022"
)
lines(nigeria)
