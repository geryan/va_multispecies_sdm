generate_data_records <- function(
    raw_data,
    target_area_raster
  ){

 tidy_data_1 <- raw_data |>
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
      #area.type,
      insecticide_control,
      itn_use,
      starts_with("sampling_occurrence"),
      starts_with("occurrence_n_"),
      month_start,
      month_end,
      year_start,
      year_end,
      species,
      confidence_in_georef
    )  |>
    select( # remove extraneous cols selected with helper funs above
      -occurrence_n_total
    ) |>
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
    # fuck off rows with no lat long or sample method or count
    filter(
      !(
        is.na(latitude) &
          is.na(longitude) &
          is.na(sampling_occurrence) &
          is.na(occurrence_n)
      )
    ) |>
    # rearrange col order so it has info i want to see up front
    select(
      species,
      occurrence_n,
      sampling_occurrence,
      latitude,
      longitude,
      month_start,
      month_end,
      year_start,
      year_end,
      binary_absence,
      binary_presence,
      itn_use,
      insecticide_control,
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
      study_time = difftime(
        time1 = end_date,
        time2 = start_date,
        units = "days"
      ) |>
        as.numeric(),
      # convert back to approx months and add 0.5 so start and end month of the
      # same month does not give zero time
      study_months = study_time / 30 + 0.5,
      .after = year_end
    ) |>
    select(
      - starts_with("month"),
      - starts_with("year")
    ) |>
    select(
      raw_data_row_id,
      id_obs,
      everything()
    ) |>
    mutate(id_obs = as.numeric(id_obs)) |>
    arrange(
      #source_id,
      raw_data_row_id,
      id_obs
    )

 # we want to fill any non-na sampling methods with the latitude and longitude
 # from earlier in the same row
 # CHECK THIS WITH MS / AW
 # to do this we arrange and fill down by row id and id obs
 # this will fuck up if there are any rows of raw_data that have NA lat1 or lon1
 # here we check for that and cease operations if there is missing data
 # and complain about this state of affairs
 tidy_1_lat_check <- tidy_data_1 |>
   filter(id_obs == 1 & is.na(latitude)) |>
   nrow()

 tidy_1_lon_check <- tidy_data_1 |>
   filter(id_obs == 1 & is.na(longitude)) |>
   nrow()

 if(tidy_1_lat_check != 0 | tidy_1_lon_check != 0) {
   stop("raw_data includes rows with missing latitude_1 or longitude_1")
 }

 # ok so on with filling down

 tidy_data_1 |>
   # actually this stops the above problem with writing into other rows
   # but it's a good check to have anyway so fuck off mate
  group_by(raw_data_row_id) |>
  fill(
    latitude,
    longitude,
    .direction = "down"
  )


    ## later when

    # remove points where insecticide is used

    ## hold this for later

    # dplyr::mutate(
    #   no_ic = case_when(
    #     is.na(insecticide_control) ~ TRUE,
    #     insecticide_control == "yes" ~ FALSE,
    #     TRUE ~ TRUE
    #   ),
    #   no_itn = case_when(
    #     is.na(itn_use) ~ TRUE,
    #     itn_use == "yes" ~ FALSE,
    #     TRUE ~ TRUE
    #   )
    # ) |>
    # dplyr::filter(
    #   no_ic & no_itn
    # ) |>

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
