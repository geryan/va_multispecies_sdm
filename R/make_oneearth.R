#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param mask
#' @return
#' @author Nick Golding
#' @export
make_oneearth <- function(mask,
                                     path = "data/vector/one_earth-bioregions-2023.geojson") {

  # https://www.oneearth.org/datasets/
  # The One Earth Bioregions Framework shapefile is under a
  # Attribution-NonCommercial 4.0 International (CC BY-NC 4.0) license, which
  # allows for any non-commercial use as long as you cite One Earth as follows:

  # One Earth Bioregions Framework, One Earth. {2023} Los Angeles, CA.
  # oneearth.org

  # download geojson from here, and unzip
  # https://www.oneearth.org/downloads/

  # set up the levels fdown to bioregion by manually transcribing the web information for One Earth
  # as they have a manual approval step before sharing the full data - and they
  # display text as images to prevent copy-pasting!
  # https://www.oneearth.org/bioregions-2023/
  bioregion_lookup <- tribble(
    ~bioregion_code, ~bioregion, ~subrealm, ~realm,
    "AT01", "Tristan Volcanic Islands", "Southern Afrotropics", "Afrotropics",
    "AT02", "South African Cape Shrublands & Mountain Forests", "Southern Afrotropics", "Afrotropics",
    "AT04", "Mascarene Tropical Forest Islands", "Madagascar & East African Coast", "Afrotropics",
    "AT05", "Seychelles & Comoros Tropical Islands", "Madagascar & East African Coast", "Afrotropics",
    "AT06", "Madagascar Island", "Madagascar & East African Coast", "Afrotropics",
    "AT07", "East Africa Coastal Forests", "Madagascar & East African Coast", "Afrotropics",
    "AT08", "Southeast African Subtropical Grasslands", "Southern Afrotropics", "Afrotropics",
    "AT09", "Greater Karoo & Kalahari Drylands", "Southern Afrotropics", "Afrotropics",
    "AT10", "Southwest African Coastal Drylands", "Southern Afrotropics", "Afrotropics",
    "AT11", "Greater African Subequatorial Savannas & Mixed Woodlands", "Sub-Equatorial Afrotropics", "Afrotropics",
    "AT12", "Victoria Basin & Albertine Rift Forests", "Equatorial Afrotropics", "Afrotropics",
    "AT13", "South Congolian Forest-Savannas & Coastal Scarp", "Equatorial Afrotropics", "Afrotropics",
    "AT14", "Central Congolian Tropical Forests", "Equatorial Afrotropics", "Afrotropics",
    "AT15", "North Congolian Lowland Forests", "Equatorial Afrotropics", "Afrotropics",
    "AT16", "Mandara Mountain & North Congolian Forest-Savannas", "Equatorial Afrotropics", "Afrotropics",
    "AT17", "Gulf of Guinea Coastal forests & Mangroves", "Equatorial Afrotropics", "Afrotropics",
    "AT18", "St Helena & Ascension Islands", "Sub-Equatorial Afrotropics", "Afrotropics",
    "AT19", "West African Coastal Forests & Savanna", "Equatorial Afrotropics", "Afrotropics",
    "AT20", "West Sudanian Savanna", "Sub-Saharan Afrotropics", "Afrotropics",
    "AT21", "Lake Turkana-Sudd Grasslands, Bushlands & Forests", "Horn of Africa", "Afrotropics",
    "AT22", "South Red Sea & Gulf of Aden Coastal Drylands", "Horn of Africa", "Afrotropics",
    "AT23", "Sahel Acacia Savannas", "Sub-Saharan Afrotropics", "Afrotropics",
    "PA22", "Madeira Evergreen Island", "North Africa", "Southern Eurasia",
    "PA23", "South Mediterranean Mixed Woodlands & Forests", "North Africa", "Southern Eurasia",
    "PA24", "Northern Sahara Deserts, Savannas, & Marshes", "North Africa", "Southern Eurasia",
    "PA25", "Southern Sahara Deserts & Mountain Woodlands", "North Africa", "Southern Eurasia",
    "PA26", "Red Seas, Arabian Deserts & Salt Marshes", "Greater Arabian Peninsula", "Southern Eurasia"
  )

  # make a SpatVector of these, cropped to Africa and with the classification
  # information added
  path |>
    terra::vect() |>
    terra::crop(
      mask
    ) |>
    rename(
      bioregion_code = Bioregions
    ) |>
    left_join(
      bioregion_lookup,
      by = "bioregion_code"
    )

}
