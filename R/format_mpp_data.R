format_mpp_data <- function(records, background, modlyr){

  z <- records |>
    dplyr::filter(
      species %in% c(
        "arabiensis",
        "coluzzii",
        "funestus",
        "gambiae"#,
        #"nili",
        #"moucheti",
        #"merus",
        #"melas",
        #"pharoensis"
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
    covariates = modlyr,
    presences_and_absences = pa |>
      rename(x = lon, y = lat)
  ) |>
    # select(
    #   tcw,
    #   tcb,
    #   built_volume,
    #   everything()
    # ) |>
    drop_na() |>
    as.data.frame()

  po_covs <- extract_covariates(
    covariates = modlyr,
    presences = po |>
      rename(x = lon, y = lat)
  ) |>
    bind_cols(po) |>
    select(-presence, -lat, -lon) |>
    drop_na() |>
    make_mpp_list(species)


  bg <- extract_covariates(
    covariates = modlyr,
    presences = background |>
      as_tibble()
  ) |>
    select(-presence) |>
    drop_na() |>
    as.data.frame()


  list(pa = pa_covs, po = po_covs, bg = bg)

}




