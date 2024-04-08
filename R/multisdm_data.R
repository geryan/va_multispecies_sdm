multisdm_data <- function(x){

  x |>
    dplyr::filter(species %in% c("arabiensis", "coluzzi", "funestis", "gambiae")) |>
    group_by(lon, lat, pa) |>
    mutate(nsp = n())

}
