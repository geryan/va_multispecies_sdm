make_detailed_data_records <- function(
    raw_data,
    target_area_raster
  ){

  raw_data |>
    dplyr::select(
      source_id,
      # occ_data,
      # bio_data,
      # binary.presence,
      # binary.absence,
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
    rowwise() |>
    mutate(
      control = any(!no_ic, !no_itn)
    ) |>
    select(-no_itn, -no_ic, -insecticide_control, -itn_use) |>
    ungroup() |>
    pivot_longer(
      cols = c(starts_with("sampling_occurrence"), starts_with("occurrence_n")),
      #cols = sampling.method_1:n_4,
      cols_vary = "slowest",
      names_to = c(".value", "set"),
      names_pattern = "(.*.)_(.)"
    ) |>
    select(-set) |>
    filter(!is.na(sampling.method)) |>
    arrange(source_ID, species, sampling.method, control) |>
    group_by(source_ID) |>
    mutate(
      #pa = ifelse(any(n == 0), "pa", "po")
      type = case_when(
        any(n == 0) ~ "pa",
        TRUE ~ "po"
      )
    )|>
    ungroup()|>
    mutate(
      count = ifelse(is.na(n), FALSE, TRUE),
      n = ifelse(is.na(n), 1, n),
      species = clean_species(species),
      method = clean_sampling_method(sampling.method)
    )

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
