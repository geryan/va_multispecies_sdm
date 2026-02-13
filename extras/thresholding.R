# explore defining a climatic limits mask for vectors

library(terra)
library(tidyverse)

# load the occurrence data and subset to only the presences
dat <- read_csv("~/Desktop/mod_dat_spat_updated.csv")
dat_pres <- dat |>
  filter(presence == 1)

# load the offset layers and compute a 2000-2024 average
files <- list.files("~/Dropbox/github/mosmicrosim/processing/vector_rasters/",
                    pattern = "^an_gambiae",
                    full.names = TRUE)

names <- files |>
  basename() |>
  stringr::str_remove("^an_gambiae.") |>
  stringr::str_remove(".adult.tif$")

ag <- terra::rast(files)
names(ag) <- names

# fill in bad NAs (all well outside the area of interest) with 0s
mask <- terra::rast("~/Dropbox/github/mosmicrosim/temp/raster_mask.tif")
ag[is.na(ag)] <- 0
ag <- mask(ag, mask)

# find years and months for plotting and summarising
slice_year <- names |>
  stringr::str_split_i("\\.", 1) |>
  as.numeric()
slice_month <- names |>
  stringr::str_split_i("\\.", 2) |>
  as.numeric()
years <- sort(unique(slice_year))
months <- sort(unique(slice_month))

# create monthly synoptic version
monthly_list <- list()
for(month in months) {
  monthly_list[[month]] <- mean(ag[[slice_month == month]])
}
synoptic <- terra::rast(monthly_list)
names(synoptic) <- month.name[months]

# get the average and maximum climatic suitability over all months from this
# synoptic layer
average <- mean(synoptic)
maximum <- max(synoptic)

# extract the average climatic suitability
pres_coords <- dat_pres |>
  select(
    longitude,
    latitude
  ) |>
  as.matrix()

pres_coords_vect <- vect(pres_coords,
                         crs = "+proj=longlat +datum=WGS84")

# get the maximum of maximum suitability in a buffer region, to account for
# geolocation errors
buffer_m <- 10000
pres_coords_buff <- terra::buffer(pres_coords_vect, buffer_m)

vals <- terra::extract(maximum,
                       pres_coords_buff,
                       fun = "max",
                       ID = FALSE)[, 1]

dat_pres$offset_avg <- vals

# exclude this fraction of occurrence records with our threshold
quantile_prob <- 0.01

# define a threshold for each species
thresholds <- dat_pres |>
  dplyr::group_by(
    species
  ) |>
  summarise(
    threshold = quantile(
      offset_avg,
      quantile_prob,
      na.rm = TRUE
    )
  ) |>
  pivot_wider(
    names_from = species,
    values_from = threshold
  ) |>
  as.list()

thresholds

mask_list <- list()

for (species in names(thresholds)) {
  mask_list[[species]] <- average > thresholds[[species]]
}

plot(rast(mask_list))

# Maybe apply this to the final abundance predictions models for all species
# combine to define a common limit, that covers all species and accounts for
# landcover (aridity) too.
