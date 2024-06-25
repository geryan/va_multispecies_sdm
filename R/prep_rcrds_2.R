prep_rcrds_2 <- function(va_data, new_mask_v) {
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

  vlocs <- vect(locs) |>
    mask(new_mask_v)


  # gambiae

  gambiae_b <- r |>
    filter(species == "gambiae") |>
    select(lon, lat) |>
    vect() |>
    buffer(width = 2)

  gambiae_abs <- vlocs[!is.related(vlocs, gambiae_b, "intersects")] |>
    geom(
      df = TRUE
    ) |>
    as_tibble() |>
    select(lon = x, lat = y)

  gambiae <- r |>
    filter(species == "gambiae") |>
    bind_rows(
      gambiae_abs |>
        mutate(
          species = "gambiae",
          pa = "pa",
          presence = 0
        )
    )



  # coluzzii

  coluzzii_b <- r |>
    filter(species == "coluzzii") |>
    select(lon, lat) |>
    vect() |>
    buffer(width = 2)

  coluzzii_abs <- vlocs[!is.related(vlocs, coluzzii_b, "intersects")] |>
    geom(
      df = TRUE
    ) |>
    as_tibble() |>
    select(lon = x, lat = y)

  coluzzii <- r |>
    filter(species == "coluzzii") |>
    bind_rows(
      coluzzii_abs |>
        mutate(
          species = "coluzzii",
          pa = "pa",
          presence = 0
        )
    )



  # arabiensis

  arabiensis_b <- r |>
    filter(species == "arabiensis") |>
    select(lon, lat) |>
    vect() |>
    buffer(width = 2)

  arabiensis_abs <- vlocs[!is.related(vlocs, arabiensis_b, "intersects")] |>
    geom(
      df = TRUE
    ) |>
    as_tibble() |>
    select(lon = x, lat = y)

  arabiensis <- r |>
    filter(species == "arabiensis") |>
    bind_rows(
      arabiensis_abs |>
        mutate(
          species = "arabiensis",
          pa = "pa",
          presence = 0
        )
    )



  # funestus

  funestus_b <- r |>
    filter(species == "funestus") |>
    select(lon, lat) |>
    vect() |>
    buffer(width = 2)

  funestus_abs <- vlocs[!is.related(vlocs, funestus_b, "intersects")] |>
    geom(
      df = TRUE
    ) |>
    as_tibble() |>
    select(lon = x, lat = y)

  funestus <- r |>
    filter(species == "funestus") |>
    bind_rows(
      funestus_abs |>
        mutate(
          species = "funestus",
          pa = "pa",
          presence = 0
        )
    )




  bind_rows(
    gambiae,
    coluzzii,
    arabiensis,
    funestus,
  )

}

