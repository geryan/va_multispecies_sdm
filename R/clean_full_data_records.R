clean_full_data_records <- function(
    raw_data
){

  raw_data |>
    dplyr::select(
      source_id,
      # occ_data,
      # bio_data,
      binary_presence,
      binary_absence,
      # adult.data,
      # larval.site.data,
      starts_with("latitude"),
      starts_with("longitude"),
      area_type,
      insecticide_control,
      itn_use,
      starts_with("sampling_occurrence"),
      starts_with("occurrence_n_"),
      month_start,
      month_end,
      year_start,
      year_end,
      species,
      starts_with("species_id"),
      confidence_in_georef,
      mosquitoes_tested_n,
      mosquitoes_dead_n,
      percent_mortality
    )  |>
    select( # remove extraneous cols selected with helper funs above
      -occurrence_n_total
    ) |>
    # fruit off rows with no species or no source_id
    filter(!is.na(species)) |>
    # nb all of the no source_id also have no species except 1
    # so the second term here doesn't change much
    filter(!is.na(source_id)) |>
    # these points from this source almost certainly mis-identified
    filter(!(source_id == 4682 & species == "merus")) |>
    filter(!(source_id == 1002611 & species == "moucheti")) |>
    mutate(
      # clean up species names
      species = clean_species(species),
      # create unique row id in case needed for later
      raw_data_row_id = row_number()
    ) |>
    # rename samping_occurrence_n and occurrence_n_n cols
    # to replace first _ with . so that it doesn't bugger up the next
    # step which uses the _ to split the variable name from the id number
    rename_with(
      .fn = function(x){
        sub(
          pattern = "_",
          replacement = ".",
          x = x
        )
      },
      .cols = c(
        starts_with("sampling_occurrence"),
        starts_with("occurrence_n")
      )
    ) |>
    # pivot longer so each latlon/sampling.occurrence/occurrence.n is on its
    # own row. Such tidy. So longer. Wow.
    pivot_longer(
      cols = c(
        starts_with("latitude"),
        starts_with("longitude"),
        starts_with("sampling.occurrence"),
        starts_with("occurrence.n")
      ),
      names_to = c(".value", "id_obs"),
      names_pattern = ("(.*)_(.*)")
    ) |>
    # put underscores back in names instead of nasty dots
    rename(
      sampling_occurrence = sampling.occurrence,
      occurrence_n = occurrence.n
    ) |>
    # fruit off rows with no lat long or sample method or count
    filter(
      !(
        is.na(latitude) &
          is.na(longitude) &
          is.na(sampling_occurrence) &
          is.na(occurrence_n)
      )
    ) |>
    # fruit off rows with only one of lon or lat (this should be very small ~5)
    filter(
      !(
        (is.na(longitude) & !is.na(latitude)) |
          (!is.na(longitude) & is.na(latitude))
      )
    ) |>
    # fruit off rows with impossible coordinates (12 of em)
    mutate(
      impossible_coordinates = check_impossible_coordinates(
        latitude,
        longitude
      )
    ) |>
    filter(
      !impossible_coordinates
    ) |>
    select(-impossible_coordinates) |>
    # rearrange col order so it has info i want to see up front
    select(
      species,
      occurrence_n,
      sampling_occurrence,
      latitude,
      longitude,
      area_type,
      month_start,
      month_end,
      year_start,
      year_end,
      binary_absence,
      binary_presence,
      itn_use,
      insecticide_control,
      starts_with("species_id"),
      confidence_in_georef,
      source_id,
      raw_data_row_id,
      id_obs,
      everything()
    ) |>
    # tidy up the dates
    mutate(
      start_date = sprintf(
        "%04d%02d",
        year_start,
        month_start
      ),
      start_date = ym(start_date),
      end_date = sprintf(
        "%04d%02d",
        year_end,
        month_end
      ),
      end_date = ym(end_date),
      # and calculate study period
      # this first comes out as days which is over-precise given only month date
      study_days = difftime(
        time1 = end_date,
        time2 = start_date,
        units = "days"
      ) |>
        as.numeric(),
      # convert back to approx months and add 0.5 so start and end month of the
      # same month does not give zero time
      study_months = study_days / 30 + 0.5,
      # check the fruiting end date isn't before the start date
      # make it NA if it is
      ends_before_starts = start_date > end_date,
      study_days = ifelse(ends_before_starts, NA, study_days),
      study_months = ifelse(ends_before_starts, NA, study_months),
      .after = year_end
    ) |>
    select(
      - starts_with("month"),
      - starts_with("year"),
      - ends_before_starts,
    ) |>
    select(
      raw_data_row_id,
      id_obs,
      everything()
    ) |>
  # we want to fill any non-na sampling methods with the latitude and longitude
  # from earlier in the same row or failing that later in the same row
  # likewise want to fill any non-na lat longs with missint sampling methods
  # in same fashion
  # CHECK THIS WITH MS / AW
  # to do this we arrange and fill down by row id and id obs
  # this will fruit up if there are any rows of raw_data that have NA lat1 or lon1
    mutate(id_obs = as.numeric(id_obs)) |>
    arrange(
      #source_id,
      raw_data_row_id,
      id_obs
    ) |>
  # ok so on with filling down
    # actually this grouping stops the above problem with writing into other rows
    # but it's a good check to have anyway so fruit off mate
    # get fruited yourself
    group_by(raw_data_row_id) |>
    fill(
      latitude,
      longitude,
      sampling_occurrence,
      .direction = "down"
    ) |>
    fill(
      latitude,
      longitude,
      sampling_occurrence,
      .direction = "up"
    ) |>
    ungroup() |>
    filter(!(is.na(latitude) & is.na(longitude))) |>
    # clean sampling method
    mutate(
      sampling_method = clean_sampling_method(sampling_occurrence)
    ) |>
    # and genetic id cols
    rowwise() |>
    mutate(
      genetic_id = check_genetic_id(
        species_id_1,
        species_id_2
      )
    ) |>
    ungroup() |>
    # flag insecticide use
    mutate(
      ic = case_when(
        is.na(insecticide_control) ~ FALSE,
        insecticide_control == "yes" ~ TRUE,
        TRUE ~ FALSE
      ),
      itn = case_when(
        is.na(itn_use) ~ FALSE,
        itn_use == "yes" ~ TRUE,
        TRUE ~ FALSE
      ),
      mtn = !is.na(mosquitoes_tested_n),
      mdn = !is.na(mosquitoes_dead_n),
      pcm = !is.na(percent_mortality)
    ) |>
    rowwise() |>
    mutate(
      insecticide = any(ic, itn),
      prob_bioassay = any(mtn, mdn, pcm)
    ) |>
    ungroup() |>
    # check if point or not  -
    mutate(
      point_data = check_point_data_type(area_type)
    ) |>
    mutate(
      # record the detailed sampling method (for posterior checks), and then
      # reduce to fewer classes for modelling
      sampling_method_detailed = sampling_method,
      sampling_method = reduce_sampling_methods(sampling_method),
      known_indoor = case_when(
        stringr::str_ends(sampling_method, "ind") ~ TRUE,
        sampling_method == "hri" ~ TRUE,
        .default = FALSE
      ),
      ic_ki = insecticide & known_indoor
    ) |>
    filter(!ic_ki, !prob_bioassay) |>
    # get rid of cols with insecticide use or bioassay larval samples
    select(
      - sampling_occurrence,
      - starts_with("species_id"),
      - ic,
      -itn,
      -insecticide_control,
      -itn_use,
      -area_type
    )



  # idx <- which(
  #   !is.na(
  #     terra::extract(
  #       x = target_area_raster,
  #       y = tidy_records |>
  #         select(
  #           lon,
  #           lat
  #         ) |>
  #         as.matrix()
  #     )
  #   )
  # )
  #
  # tidy_records[idx,]

}
