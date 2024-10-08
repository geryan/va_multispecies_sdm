make_data_records <- function(
    raw_data,
    target_area_raster
  ){

  tidy_records <- raw_data |>
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
    filter(occ_data == 1) |> # consider whether occ_data == 0 could be PO data
    rowwise() |> # NB this rowwise is necessary for the below `any` to work by row, but may be slow on a very large dataset
    mutate(
      any_sm_na_count = any(
        !is.na(sampling.method_1) & is.na(n_1),
        !is.na(sampling.method_2) & is.na(n_2),
        !is.na(sampling.method_3) & is.na(n_3),
        !is.na(sampling.method_4) & is.na(n_4)
      ), # this checks if there are any non-empty sampling methods with a NA count
      all_sm_na = all(is.na(c_across(starts_with("sampling.method")))) # check if all survey methods are NA so no zero count
    ) |>
    rename(entered_n_tot = n_tot) |> # renaming because want to keep for checking but will get double-counted by the sum(c_across(...)) below if left with a name beginning "n_"
    mutate(
      count = case_when(
        any_sm_na_count ~ NA, # assign NA n_tot if there is a non-empty sampling method that is NA
        all_sm_na ~ NA,
        TRUE ~ sum(c_across(starts_with("n_")), na.rm = TRUE) # otherwise sum up the values ignoring NAs
      )
    ) |>
    #select(-entered_n_tot) |>
    mutate(
      presence = case_when(
        #binary.absence == "yes" ~ 0, ignore this and only use
        count == 0 ~ 0,
        TRUE ~ 1
      )
    ) |>
    group_by(source_ID) |>
    mutate(
      pa = ifelse(any(presence == 0), "pa", "po")
    )|>
    ungroup() |>
    select(
      source_ID,
      species,
      lon,
      lat,
      #binary.presence,
      #binary.absence,
      #starts_with("n_"),
      count,
      presence,
      pa,
      starts_with("sampling.method_")
    ) |>
    mutate(
      species = clean_species(species)
    ) |>
    arrange(species, pa, presence) |>
    distinct()

  idx <- which(
    !is.na(
      terra::extract(
        x = target_area_raster,
        y = tidy_records |>
          select(
            lon,
            lat
          ) |>
          as.matrix()
      )
    )
  )

  tidy_records[idx,]

}
