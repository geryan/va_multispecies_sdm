# TerraClimate climate layers for Nigeria, on the grid of the land-cover extract
# in nigeria_environmental_space/.
#
# Writes to nigeria_environmental_space/, for ppt (precipitation, mm), tmax
# (maximum 2 m temperature, degC) and vpd (vapour pressure deficit, kPa):
#   - terraclimate_<var>_annual_2000_2025.tif: one layer per year. Annual ppt
#     is the total of the 12 months; annual tmax and vpd are the means of the
#     12 monthly values.
#   - terraclimate_<var>_monthly_2019_2022.tif: one layer per month, the
#     monthly values as TerraClimate gives them.
#
# Source: TerraClimate v1.1 (1/30 degree, monthly, 1950-2025), from the
# Climatology Lab THREDDS server. Only a box around Nigeria is downloaded, via
# the NetCDF Subset Service, into download_dir. That folder is deleted at the
# end once every output has passed its checks (set delete_downloads to FALSE to
# keep it).
#
# Each variable is summarised to annual values on the native 1/30 degree grid,
# then averaged onto the land-cover grid (1/24 degree), weighted by area (see
# to_template()), and masked to the Nigeria boundary, as the land cover and
# footprint were.
#
# Needs only nigeria_environmental_space/ (no targets store). Run from the repo
# root:
#   Rscript extras/make_nigeria_terraclimate.R

library(terra)
library(dplyr)
library(purrr)

outdir <- "nigeria_environmental_space"
download_dir <- "data/raster/terraclimate_nigeria"
delete_downloads <- TRUE

annual_years <- 2000:2025
monthly_years <- 2019:2022

# how each variable becomes annual, and its expected units
vars <- tibble::tribble(
  ~var,   ~annual_fun, ~units,
  "ppt",  "sum",       "mm",
  "tmax", "mean",      "degC",
  "vpd",  "mean",      "kPa"
)

# broad sanity bounds for Nigeria, to catch an unapplied scale factor (x10 or
# x100) rather than to judge the data
annual_bounds <- list(
  ppt = c(50, 10000),
  tmax = c(15, 45),
  vpd = c(0.1, 8)
)

stopifnot(all(monthly_years %in% annual_years))

dir.create(
  download_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

template <- rast(
  file.path(
    outdir,
    "landcover_tree_2013_2022.tif"
  )
)[[1]]
nigeria <- vect(
  file.path(
    outdir,
    "nigeria_boundary.gpkg"
  )
)

# download ------------------------------------------------------------------

options(timeout = 300)

# the template's extent plus a margin of several native cells
bbox <- extend(
  ext(template),
  0.25
)

ncss_url <- function(var, year) {
  paste0(
    "http://thredds.northwestknowledge.net:8080/thredds/ncss/",
    "TERRACLIMATE_ALL/data/TerraClimate_",
    var,
    "_",
    year,
    ".nc",
    "?var=",
    var,
    "&north=",
    ymax(bbox),
    "&south=",
    ymin(bbox),
    "&west=",
    xmin(bbox),
    "&east=",
    xmax(bbox),
    "&horizStride=1",
    "&time_start=",
    year,
    "-01-01T00:00:00Z",
    "&time_end=",
    year,
    "-12-31T23:59:59Z",
    "&accept=netcdf4"
  )
}

download_file <- function(var, year, tries = 3) {
  dest <- file.path(
    download_dir,
    sprintf(
      "TerraClimate_%s_%s_nigeria.nc",
      var,
      year
    )
  )
  if (file.exists(dest)) {
    return(dest)
  }

  # download to a temporary name so an interrupted download is never reused
  part <- paste0(dest, ".part")
  for (i in seq_len(tries)) {
    ok <- tryCatch(
      {
        download.file(
          ncss_url(
            var,
            year
          ),
          destfile = part,
          mode = "wb",
          quiet = TRUE
        )
        TRUE
      },
      error = function(e) {
        message(
          "Attempt ",
          i,
          " failed for ",
          var,
          " ",
          year,
          ": ",
          conditionMessage(e)
        )
        FALSE
      }
    )
    if (ok) {
      file.rename(
        part,
        dest
      )
      return(dest)
    }
    Sys.sleep(5 * i)
  }
  stop("Could not download ", var, " ", year)
}

files <- tidyr::expand_grid(
  var = vars$var,
  year = annual_years
) |>
  mutate(
    file = map2_chr(
      var,
      year,
      download_file
    )
  )

# read and check each download ----------------------------------------------

read_month_stack <- function(file, var, year) {
  meta <- describe(file)
  version <- grep(
    "NC_GLOBAL#version=",
    meta,
    value = TRUE
  )
  units <- grep(
    paste0("^\\s*", var, "#units="),
    meta,
    value = TRUE
  )
  stopifnot(
    length(version) == 1,
    grepl("=v1\\.1$", trimws(version)),
    length(units) == 1,
    sub(".*=", "", trimws(units)) == vars$units[vars$var == var]
  )

  r <- rast(file)
  # the files say OGC:CRS84, which is WGS 84 lon/lat as the template is
  crs(r) <- crs(template)

  months <- seq(
    as.Date(paste0(year, "-01-01")),
    by = "month",
    length.out = 12
  )
  stopifnot(
    nlyr(r) == 12,
    all(as.Date(time(r)) == months)
  )
  names(r) <- sprintf(
    "%s_%s_%02d",
    var,
    year,
    1:12
  )
  r
}

# Area-weighted mean onto the land-cover grid, then the boundary mask.
# 1/30 and 1/24 degree share a 1/120 degree lattice, so splitting each native
# cell into 4 x 4 and averaging blocks of 5 x 5 gives each land-cover cell the
# mean of the native cells it overlaps, weighted by the overlap, with NA cells
# left out. The land-cover grid sits about 1e-4 degrees (11 m) off that
# lattice. resample(method = "average") departs from the exact mean by up to
# 0.1% here, so it isn't used.
to_template <- function(r) {
  fine <- disagg(
    r,
    fact = 4
  ) |>
    crop(
      ext(template),
      snap = "near"
    )
  stopifnot(
    nrow(fine) == 5 * nrow(template),
    ncol(fine) == 5 * ncol(template),
    all(abs(as.vector(ext(fine)) - as.vector(ext(template))) < 0.05 / 120)
  )
  out <- aggregate(
    fine,
    fact = 5,
    fun = "mean",
    na.rm = TRUE
  )
  ext(out) <- ext(template)
  mask(
    out,
    nigeria
  )
}

# build the outputs ---------------------------------------------------------

annual_file <- function(var) {
  file.path(
    outdir,
    sprintf(
      "terraclimate_%s_annual_%s_%s.tif",
      var,
      min(annual_years),
      max(annual_years)
    )
  )
}

monthly_file <- function(var) {
  file.path(
    outdir,
    sprintf(
      "terraclimate_%s_monthly_%s_%s.tif",
      var,
      min(monthly_years),
      max(monthly_years)
    )
  )
}

for (v in vars$var) {
  fun <- vars$annual_fun[vars$var == v]
  v_files <- filter(
    files,
    var == v
  )

  stacks <- pmap(
    list(
      v_files$file,
      v_files$var,
      v_files$year
    ),
    read_month_stack
  )
  names(stacks) <- v_files$year

  # annual, summarised on the native grid; a cell missing any month stays NA
  annual_native <- map(
    stacks,
    \(r) app(
      r,
      fun = fun,
      na.rm = FALSE
    )
  ) |>
    rast()
  annual <- to_template(annual_native)
  names(annual) <- sprintf(
    "%s_%s",
    v,
    annual_years
  )
  time(
    annual,
    tstep = "years"
  ) <- annual_years
  writeRaster(
    annual,
    filename = annual_file(v),
    overwrite = TRUE
  )

  # monthly
  monthly <- stacks[as.character(monthly_years)] |>
    unname() |>
    rast() |>
    to_template()
  time(monthly) <- seq(
    as.Date(paste0(min(monthly_years), "-01-01")),
    by = "month",
    length.out = nlyr(monthly)
  )
  writeRaster(
    monthly,
    filename = monthly_file(v),
    overwrite = TRUE
  )
}

# check the written files ---------------------------------------------------

# land cover is NA in a few coastal cells inside the boundary (sea in the ESA
# source), so the climate layers can have more cells than land cover
landcover_cells <- !is.na(template)
boundary_cells <- init(
  rast(template),
  1
) |>
  mask(nigeria)
boundary_cells <- !is.na(boundary_cells)
n_landcover <- global(landcover_cells, "sum")[1, 1]

check_output <- function(file, n_layers) {
  r <- rast(file)
  not_na <- !is.na(r)
  n_cells <- global(not_na, "sum")[, 1]
  any_value <- sum(not_na) > 0
  # cells with a value in some layers but not others
  n_partial <- global(
    any_value & sum(not_na) < nlyr(r),
    "sum"
  )[1, 1]
  n_outside <- global(any_value & !boundary_cells, "sum")[1, 1]
  stopifnot(
    compareGeom(r, template),
    nlyr(r) == n_layers,
    n_partial == 0,
    n_outside == 0
  )
  tibble::tibble(
    file = basename(file),
    layers = nlyr(r),
    cells = n_cells[1],
    landcover_cells_without_value = global(landcover_cells & !any_value, "sum")[1, 1],
    cells_without_landcover = global(any_value & !landcover_cells, "sum")[1, 1],
    min = min(minmax(r)[1, ]),
    max = max(minmax(r)[2, ])
  )
}

report <- bind_rows(
  map(
    vars$var,
    \(v) check_output(
      annual_file(v),
      length(annual_years)
    )
  ),
  map(
    vars$var,
    \(v) check_output(
      monthly_file(v),
      12 * length(monthly_years)
    )
  )
)

# annual values within the broad bounds
for (v in vars$var) {
  mm <- range(minmax(rast(annual_file(v))))
  if (mm[1] < annual_bounds[[v]][1] || mm[2] > annual_bounds[[v]][2]) {
    stop(
      "Annual ",
      v,
      " outside its expected bounds: ",
      paste(
        round(mm, 2),
        collapse = " to "
      )
    )
  }
}

# the monthly and annual files agree: averaging onto the template is linear, so
# summarising the resampled months gives the resampled annual value (up to
# float32 rounding and cells at the edge of the data)
agreement <- map(
  vars$var,
  \(v) {
    fun <- vars$annual_fun[vars$var == v]
    monthly <- rast(monthly_file(v))
    annual <- rast(annual_file(v))
    map(
      monthly_years,
      \(y) {
        from_monthly <- app(
          monthly[[format(time(monthly), "%Y") == y]],
          fun = fun
        )
        d <- abs(from_monthly - annual[[time(annual) == y]])
        tibble::tibble(
          var = v,
          year = y,
          max_abs_diff = global(d, "max", na.rm = TRUE)[1, 1],
          max_value = global(abs(annual[[time(annual) == y]]), "max", na.rm = TRUE)[1, 1]
        )
      }
    ) |>
      bind_rows()
  }
) |>
  bind_rows() |>
  mutate(rel_diff = max_abs_diff / max_value)
stopifnot(all(agreement$rel_diff < 1e-5))

# Nigerian means, first and last year
means <- map(
  vars$var,
  \(v) {
    r <- rast(annual_file(v))
    tibble::tibble(
      var = v,
      year = annual_years,
      mean = global(r, "mean", na.rm = TRUE)[, 1]
    )
  }
) |>
  bind_rows()

cat("\nOutputs (land-cover cells: ", n_landcover, "):\n", sep = "")
print(report, width = Inf)
cat("\nMonthly against annual, 2019-2022 (largest relative difference):",
    signif(max(agreement$rel_diff), 3), "\n")
cat("\nNigerian means, first and last years:\n")
print(
  filter(
    means,
    year %in% range(annual_years)
  )
)

# clean up ------------------------------------------------------------------

if (delete_downloads) {
  unlink(
    download_dir,
    recursive = TRUE
  )
  cat("\nDeleted", download_dir, "\n")
}
