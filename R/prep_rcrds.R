prep_rcrds <- function(va_data){
  r <- va_data |>
    filter(!is.na(species)) |>
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
        binary.absence == "yes" ~ 0,
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
      #source_ID,
      species,
      lon,
      lat,
      #binary.presence,
      #binary.absence,
      #starts_with("n_"),
      #count,
      presence,
      pa#,
      #starts_with("sampling.method_")
    ) |>
    mutate(
      species = clean_species(species)
    ) |>
    arrange(species, pa, presence) |>
    distinct()

  locs <- r |>
    select(lon, lat) |>
    distinct()

  gambiae <- r |>
    filter(species == "gambiae") |>
    full_join(
      y = locs
    ) |>
    mutate(
      species = "gambiae",
      presence = ifelse(is.na(presence), 0, presence),
      pa = ifelse(is.na(pa), "pa", pa)
    )

  coluzzii <- r |>
    filter(species == "coluzzii") |>
    full_join(
      y = locs
    ) |>
    mutate(
      species = "coluzzii",
      presence = ifelse(is.na(presence), 0, presence),
      pa = ifelse(is.na(pa), "pa", pa)
    )

  arabiensis <- r |>
    filter(species == "arabiensis") |>
    full_join(
      y = locs
    ) |>
    mutate(
      species = "arabiensis",
      presence = ifelse(is.na(presence), 0, presence),
      pa = ifelse(is.na(pa), "pa", pa)
    )

  funestus <- r |>
    filter(species == "funestus") |>
    full_join(
      y = locs
    ) |>
    mutate(
      species = "funestus",
      presence = ifelse(is.na(presence), 0, presence),
      pa = ifelse(is.na(pa), "pa", pa)
    )

  bind_rows(
    gambiae,
    coluzzii,
    arabiensis,
    funestus,
  )

}

