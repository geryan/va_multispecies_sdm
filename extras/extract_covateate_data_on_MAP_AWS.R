library(terra)
library(tidyverse)

read_cov_data <- function(
    odir,
    prefix,
    suffix,
    lyr_prefix
){

  onames <- list.files(
    path = odir,
    pattern = suffix
  ) |>
    sub(
      pattern = prefix,
      replacement = lyr_prefix,
      x = _
    ) |>
    sub(
      pattern = suffix,
      replacement = "",
      x = _
    )

  covs_raw <- list.files(
    odir,
    pattern = suffix,
    full.names = TRUE
  ) |>
    terra::rast()

  names(covs_raw) <- onames

  covs_raw

}

# read the data stacks

tcb <- read_cov_data(
  odir = "/mnt/s3/mastergrids/MODIS_Global/MCD43D6_v061_BRDF_Reflectance/TCB_v061/5km/Monthly/",
  prefix = "TCB_v061\\.",
  suffix = "\\.mean\\.5km\\.mean\\.tif",
  lyr_prefix = "tcb_"
)

# because month 1 in 2000 is missing
# replace it with month 1 2001
tcb <- c(tcb[[12]], tcb)

evi <- read_cov_data(
  odir = "/mnt/s3/mastergrids/MODIS_Global/MCD43D6_v061_BRDF_Reflectance/EVI_v061/5km/Monthly/",
  prefix = "EVI_v061\\.",
  suffix = "\\.mean\\.5km\\.mean\\.tif",
  lyr_prefix = "evi_"
)

# because month 1 in 2000 is missing
# replace it with month 1 2001
evi <- c(evi[[12]], evi)



lst_night <- read_cov_data(
  odir = "/mnt/s3/mastergrids/MODIS_Global/MOD11A2_v061_LST/LST_Night_v061/5km/Monthly/",
  prefix = "LST_Night_v061\\.",
  suffix = "\\.mean\\.5km\\.mean\\.tif$",
  lyr_prefix = "lst_night_"
)

# because month 1 in 2000 is missing
# replace it with month 1 2001
lst_night <- c(lst_night[[12]], lst_night)


#####
extract_ym_indexed_layer_data <- function(
    dat,
    r,
    min_year,
    max_year = NULL
) {

  idx_tbl <- dat |>
    mutate(
      month = month(date),
      year = year(date),
      year = ifelse(year < min_year, min_year, year)
    )

  if(!is.null(max_year)){
    idx_tbl <- idx_tbl |>
      mutate(
        year = ifelse(year > max_year, max_year, year)
      )
  }

  idx_tbl <- idx_tbl |>
    mutate(
      lyridx = 12 * (year - min_year) + month
    )


  cellidx <- idx_tbl |>
    select(x, y) |>
    as.matrix() |>
    cellFromXY(
      object = r[[1]],
      xy = _
    )

  idx <- tibble(lyridx = idx_tbl$lyridx, cellidx = cellidx)

  # because of terra indexing, can't directly pull out
  # offsets[[idx$lyridx]][idx$cellidx]
  # because that will pull out a stack of layers length(idx$lyridx) and then
  # extract the cell values for all of those layers ffs
  # so instead go layer by layer

  # empty object for loop to fill
  indexed_value <- rep(NA_real_, nrow(idx))

  unique_layers <- unique(idx$lyridx)

  for (layer in unique_layers) {
    # rows in idx that correspond to this layer
    rowidx <- which(idx$lyridx == layer)

    # cells for those rows
    cells <- idx$cellidx[rowidx]

    # extract all at once for this layer
    vals <- terra::extract(
      x = r[[layer]],
      y = cells,
      raw = TRUE
    )
    indexed_value[rowidx] <- as.numeric(vals)
  }

  tibble(
    idx_tbl, cellidx = cellidx, value = indexed_value
  )

}

average_last_year <- function(r){

  nl <- nlyr(r)

  mean(r[[(nl-11): nl]])

}

match_offset_data <- function(model_data_spatial, offsets_5) {

  dat_date <- model_data_spatial |>
    select(x = longitude, y = latitude, date = model_date) |>
    distinct() |>
    filter(!is.na(date))

  date_matched_data <- extract_ym_indexed_layer_data(
    dat = dat_date,
    r = offsets_5,
    min_year = 2000
  )

  dat_no_date <- model_data_spatial |>
    filter(is.na(model_date)) |>
    select(x = longitude, y = latitude) |>
    distinct()

  offset_last_avg_5 <- average_last_year(offsets_5)

  no_date_matched_data <- terra::extract(
    offset_last_avg_5,
    dat_no_date
  )

  matched_data <- rbind(
    date_matched_data |>
      select(
        longitude = x,
        latitude = y,
        model_date = date,
        offset = value
      ),
    cbind(
      dat_no_date |>
        rename(
          longitude = x,
          latitude = y,
        ),
      model_date = NA,
      offset = no_date_matched_data$mean
    )
  )

  model_data_spatial |>
    left_join(
      y = matched_data,
      by = c("longitude", "latitude", "model_date")
    )

}


model_data_spatial <- readr::read_csv("mod_dat_spat.csv")


tcb_dat <- match_offset_data(model_data_spatial, tcb)
evi_dat <- match_offset_data(model_data_spatial, evi)
lst_night_dat <- match_offset_data(model_data_spatial, lst_night)

all(tcb_dat$latitude == evi_dat$latitude)
all(tcb_dat$latitude == lst_night_dat$latitude)
all(tcb_dat$latitude == model_data_spatial$latitude)

mds_updated <- model_data_spatial |>
  select(-evi, -tcb, -lst_night) |>
  mutate(
    tcb = tcb_dat$offset.y,
    evi = evi_dat$offset.y,
    lst_night = lst_night_dat$offset.y
  )


