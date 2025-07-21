check_impossible_coordinates <- function(
    latitude,
    longitude
  ){

  case_when(
    latitude < -90 ~ TRUE,
    latitude > 90 ~ TRUE,
    longitude > 180 ~ TRUE,
    longitude < -180 ~ TRUE,
    .default = FALSE
  )

}
