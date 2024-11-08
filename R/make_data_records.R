make_data_records <- function(
    raw_data,
    target_area_raster
  ){

  tidy_records <- raw_data |>
    dplyr::select(
      source_id,
      # occ_data,
      # bio_data,
      binary_presence,
      binary_absence,
      # adult.data,
      # larval.site.data,
      lon = longitude_1,
      lat = latitude_1,
      #area.type,
      insecticide_control,
      itn_use,
      starts_with("sampling_occurrence"),
      starts_with("occurrence_n_"),
      # binary.presence,
      # binary.absence,
      # month_st,
      # month_end,
      # year_st,
      # year_end,
      species
    )  |>
    select( # remove extraneous cols selected with helper funs above
      -occurrence_n_total
    ) |>
    mutate( # clean up species names
      species = clean_species(species)
    ) |>
    # remove missing lat longs
    dplyr::filter(
      !is.na(lon) &
        !is.na(lat)
    )|>
    # remove points where insecticide is used
    dplyr::mutate(
      no_ic = case_when(
        is.na(insecticide_control) ~ TRUE,
        insecticide_control == "yes" ~ FALSE,
        TRUE ~ TRUE
      ),
      no_itn = case_when(
        is.na(itn_use) ~ TRUE,
        itn_use == "yes" ~ FALSE,
        TRUE ~ TRUE
      )
    ) |>
    dplyr::filter(
      no_ic & no_itn
    ) |>
    mutate(
      across(
        starts_with("occurrence_n_"),
        ~ ifelse(.x == 1000000, NA_integer_, .x) # get rid of NAs in old data that have been replaced with 1000000
      )
    ) |>
    rowwise() |> # NB this rowwise is necessary for the below `any` to work by row, but may be slow on a very large dataset
    mutate(
      any_sm_na_count = any(
        !is.na(sampling_occurrence_1) & is.na(occurrence_n_1),
        !is.na(sampling_occurrence_2) & is.na(occurrence_n_2),
        !is.na(sampling_occurrence_3) & is.na(occurrence_n_3),
        !is.na(sampling_occurrence_4) & is.na(occurrence_n_4)
      ), # this checks if there are any non-empty sampling methods with a NA count
      all_sm_na = all(is.na(c_across(starts_with("sampling_occurrence_")))) # check if all survey methods are NA so no zero count
    ) |>
    mutate(
      count = case_when(
        any_sm_na_count ~ NA, # assign NA n_tot if there is a non-empty sampling method that is NA
        all_sm_na ~ NA,
        TRUE ~ sum(c_across(starts_with("occurrence_n_")), na.rm = TRUE) # otherwise sum up the values ignoring NAs
      )
    ) |>
    mutate(
      presence = case_when(
        binary_absence == "yes" ~ 0, #ignore this and only use
        count == 0 ~ 0,
        TRUE ~ 1
      )
    ) |>
    group_by(source_id) |>
    mutate(
      pa = ifelse(any(presence == 0), "pa", "po")
    )|>
    ungroup() |>
    select(
      source_id,
      species,
      lon,
      lat,
      #binary.presence,
      #binary.absence,
      #starts_with("n_"),
      count,
      presence,
      pa#,
      #starts_with("sampling.method_")
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
