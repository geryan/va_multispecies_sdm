tidy_va_data <- function(raw_data){

  raw_data |>
    dplyr::select(
      occ_data,
      bio_data,
      adult.data,
      larval.site.data,
      lon = longitude_1,
      lat = latitude_1,
      insecticide.control,
      ITN.use,
      starts_with("sampling.method"),
      starts_with("n_")
    ) |>
    # remove points where insecticide is used
    dplyr::mutate(
      no_ic = case_when(
        is.na(insecticide.control) ~ TRUE,
        insecticide.control == "yes" ~ FALSE,
        TRUE ~ TRUE
      ),
      no_itn = case_when(
        is.na(ITN.use) ~ TRUE,
        ITN.use == "yes" ~ FALSE,
        TRUE ~ TRUE
      )
    ) |>
    dplyr::filter(
      no_ic & no_itn
    ) |>
    # remove missing lat longs
    dplyr::filter(
      !is.na(lon) &
      !is.na(lat)
    )

}
