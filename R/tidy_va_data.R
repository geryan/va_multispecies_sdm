tidy_va_data <- function(raw_data){

  raw_data |>
    dplyr::select(
      adult.data,
      larval.site.data,
      lon = longitude_1,
      lat = latitude_1,
      insecticide.control,
      ITN.use
    ) |>
    dplyr::filter(
      insecticide.control != "yes", # exclude
      ITN.use != "yes", # exclude
      !is.na(lon),
      !is.na(lat)
    ) |>
    dplyr::select(
      -insecticide.control,
      -ITN.use
    )



}
