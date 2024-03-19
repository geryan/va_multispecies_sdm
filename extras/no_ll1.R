library(readr)
library(dplyr)

raw_data <- read_csv(
  file = "data/final_species_20240314.csv",
  guess_max = 30000
)


no_ll1 <- raw_data |>
  # remove missing lat longs
  dplyr::filter(
    is.na(longitude_1) &
      is.na(latitude_1)
  )


no_ll1_thin <- no_ll1 |>
  dplyr::select(
    `...1`,
    X.1,
    X,
    source_ID,
    bio_data,
    occ_data,
    adult.data,
    larval.site.data,
    area.type,
    longitude_1,
    latitude_1,
    longitude_2,
    latitude_2,
    longitude_3,
    latitude_3,
    longitude_4,
    latitude_4,
    longitude_5,
    latitude_5,
    longitude_6,
    latitude_6,
    longitude_7,
    latitude_7,
    longitude_8,
    latitude_8
  )

write_csv(
  no_ll1,
  "extras/final_species_no_lat_long_1.csv"
)

write_csv(
  no_ll1_thin,
  "extras/final_species_no_lat_long_1_thin.csv"
)


# how many per area type?
table(no_ll1$area.type)
# large polygon         point         Point small polygon     wide area
#            25          3667            14            11           162

# how many missing bionomic data or occurrence data
table(no_ll1$bio_data, no_ll1$occ_data)
# 0    1
# 0    0 5896
# 1  143    0
