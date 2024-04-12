multisdm_data <- function(x, covs){

  z <- x |>
    dplyr::filter(
      species %in% c(
        "arabiensis",
        "coluzzii",
        "funestus",
        "gambiae"
      )
    )

  pa <- z |>
    filter(pa == "pa") |>
    dplyr::select(lon, lat, species, presence) |>
    group_by(lon, lat, species) |>
    summarise(presence = max(presence), .groups = "drop") |>
    tidyr::pivot_wider(
      names_from = species,
      values_from = presence
    ) |>
    mutate(
      across(
        c(
          "arabiensis",
          "coluzzii",
          "funestus",
          "gambiae"
        ),
        ~ ifelse(is.na(.x), 0, .x)
      )
    )


  po <- z |>
    filter(pa == "po") |>
    select(lon, lat, species)


  pa_covs <- extract_covariates(
    covariates = covs,
    presences_and_absences = pa |>
      rename(x = lon, y = lat)
  ) |>
    select(tcw, tcb, built_volume, everything())

  extract_covariates(
    covariates = covs,
    presences = po |>
      rename(x = lon, y = lat)
  ) |>
    bind_cols(po) |>
    select(-presence, -lat, -lon)



}
