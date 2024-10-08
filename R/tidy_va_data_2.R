tidy_va_data <- function(raw_data){

  raw_data |>
    dplyr::select(
      source_ID,
      occ_data,
      bio_data,
      binary.presence,
      binary.absence,
      adult.data,
      larval.site.data,
      lon = longitude_1,
      lat = latitude_1,
      area.type,
      insecticide.control,
      ITN.use,
      starts_with("sampling.method"),
      starts_with("n_"),
      binary.presence,
      binary.absence,
      month_st,
      month_end,
      year_st,
      year_end,
      species
    )  |>
    # remove missing lat longs
    dplyr::filter(
      !is.na(lon) &
        !is.na(lat)
    )|>
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
    filter(occ_data == 1) # consider whether occ_data == 0 could be PO data

}
