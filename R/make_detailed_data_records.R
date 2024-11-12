make_detailed_data_records <- function(
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
    rowwise() |>
    mutate(
      control = any(!no_ic, !no_itn)
    ) |>
    select(-no_itn, -no_ic, -insecticide_control, -itn_use) |>
    mutate(
      across(
        starts_with("occurrence_n_"),
        ~ ifelse(.x == 1000000, NA_integer_, .x) # get rid of NAs in old data that have been replaced with 1000000
      )
    )  |>
    ungroup() |>
    pivot_longer(
      cols = c(starts_with("sampling_occurrence"), starts_with("occurrence_n")),
      #cols = sampling.method_1:n_4,
      cols_vary = "slowest",
      names_to = c(".value", "set"),
      names_pattern = "(.*.)_(.)"
    ) |>
    select(-set) |>
    filter(!is.na(sampling_occurrence)) |>
    rename(
      sampling_method = sampling_occurrence,
      n = occurrence_n
    )|>
    arrange(source_id, species, sampling_method, control) |>
    mutate(
      n = ifelse(
        binary_absence == "yes",
        0,
        n
      )
    ) |>
    group_by(source_id) |>
    mutate(
      type = case_when(
        all(!is.na(n)) ~ "count",
        n == 0 ~ "pa",
        !is.na(n) ~ "count",
        binary_absence == "yes" ~ "pa",
        any(n == 0) ~ "pa",
        TRUE ~ "po"
      )
    )|>
    group_by(
      source_id,
      species
    ) |>
    mutate(
      type = case_when(
        all(!is.na(n)) ~ "count",
        TRUE ~ type
      )
    ) |>
    ungroup()|>
    mutate(
      n = ifelse(is.na(n), 1, n),
      method = clean_sampling_method(sampling_method)
    ) |>
    select(
      source_id,
      species,
      lon,
      lat,
      control,
      method,
      type,
      n
    ) |>
    distinct() |>
    arrange(species, type, control, method, source_id, n)



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
