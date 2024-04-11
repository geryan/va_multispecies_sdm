multisdm_data <- function(x){

  z <- x |>
    dplyr::filter(
      species %in% c(
        "arabiensis",
        "coluzzi",
        "funestis",
        "gambiae"
      )
    )

  pa <- z |>
    filter(pa == "pa")

  po <- z |>
    filter(pa == "po")

}
