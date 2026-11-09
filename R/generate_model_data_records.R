#' Model records for the target species, with data types and inferred zeros
#'
#' Keeps the target species' point records with usable georeferencing, and
#' adds inferred zeros: at each location-date (source, sampling method,
#' location, start date and duration) with genetic identification or a stated
#' absence, every target species not recorded there gets a row with presence 0.
#'
#' Data types, set per location-date:
#' - a row with its own count is `count` (zeros included), except for larvae
#'   and "other" methods, which are `pa`;
#' - where some species were counted and others recorded without a count, the
#'   uncounted rows (presences, and the inferred zeros) are `pa`;
#' - where everything recorded was counted, inferred zeros are zero counts;
#' - otherwise rows at a location-date with inferred zeros, or a stated
#'   absence, are `pa`, and the rest are `po`.
#'
#' @title Generate model data records
#' @param full_data_records cleaned records, from `clean_full_data_records()`
#' @param target_species species to keep and to infer zeros for
#' @return a tibble with one row per record, including the inferred zeros
#' @author geryan
#' @export
generate_model_data_records <- function(
    full_data_records,
    target_species
  ) {

  # full_data_records |>
  #   # target species only |>
  #   filter(species %in% target_species) %$%
  #   table(sampling_method, species)


  fdr <- full_data_records |>
    # target species only |>
    filter(species %in% target_species) |>
    # fruit off vague points
    filter(
      (confidence_in_georef != "greater than 10km") |
        is.na(confidence_in_georef)
    ) |>
    # fruit off polygons and centroid of admin points
    filter(point_data) |>
    mutate(
      sampling_method = reduce_sampling_methods(sampling_method),
    ) |>
    # select(
    #   -start_date,
    #   -confidence_in_georef,
    #   -point_data,
    #   -start_date,
    #   -end_date
    # ) |>
    # simplify the binary absence and presence cols
    #  i.e. (get rid of yes no or NA to t or f)
    mutate(
      bpres = if_else(
        binary_presence == "yes",
        TRUE,
        FALSE,
        missing = FALSE
      ),
      babs = if_else(
        binary_absence == "yes",
        TRUE,
        FALSE,
        missing = FALSE
      ),
      # partial inference of data type
      data_type_inf1 = case_when(
        !is.na(occurrence_n) & sampling_method %in% c("larvae", "other") ~ "pa",
        !is.na(occurrence_n) ~ "count",
        babs & !bpres ~ "pa"
      ),
      .after = binary_presence
    )

  fdr |>
    # infer absences only from where genetic ID is done or true binary presence
    # is stated
    filter(
      genetic_id |
        (babs & !bpres)
    ) |>
    select(
      source_id,
      start_date,
      sampling_method_detailed,
      sampling_method,
      latitude,
      longitude,
      study_months
    ) |>
    distinct() |>
    expand_grid(target_species) |>
    rename(species = target_species) |>
    full_join(
      y = fdr
    ) |>
    arrange(
      source_id,
      start_date,
      sampling_method,
      sampling_method_detailed,
      latitude,
      longitude,
      study_months
    ) |>
    mutate(
      inferred = ifelse(
        is.na(raw_data_row_id),
        TRUE,
        FALSE
      ),
      .before = source_id
    ) |>
    group_by(
      source_id,
      start_date,
      sampling_method,
      sampling_method_detailed,
      latitude,
      longitude,
      study_months
    ) |>
    mutate(
      # every recorded (not inferred) row at this location-date has a count of
      # its own; zeros count as counts
      full_count = all(data_type_inf1[!inferred] %in% "count"),
      data_type = case_when(
        data_type_inf1 == "count" ~ "count",
        data_type_inf1 == "pa" ~ "pa",
        # some species counted, others recorded without a count: the uncounted
        # rows (presences and inferred zeros) are presence-absence, not zero
        # counts
        any(data_type_inf1 %in% "count") & !full_count ~ "pa",
        # everything recorded was counted: inferred zeros are zero counts
        any(data_type_inf1 == "count") ~ "count",
        any(inferred) ~ "pa",
        (babs & !bpres) ~ "pa",
        .default = "po"
      )
    )  |>
    fill(
      raw_data_row_id,
      end_date,
      study_days,
      confidence_in_georef,
      genetic_id,
      insecticide,
      .direction = "down"
    ) |>
    fill(
      raw_data_row_id,
      end_date,
      study_days,
      confidence_in_georef,
      genetic_id,
      insecticide,
      .direction = "up"
    ) |>
    ungroup() |>
    mutate(
      presence = case_when(
        occurrence_n > 0 ~ 1,
        occurrence_n == 0 ~ 0,
        bpres ~ 1,
        (babs & !bpres) ~ 0,
        !inferred ~ 1,
        .default = 0
      ),
      count = case_when(
        data_type == "count" & sampling_method == "larvae" ~ NA,
        data_type == "count" & sampling_method == "other" ~ NA,
        data_type == "count" & !is.na(occurrence_n) ~ occurrence_n,
        data_type == "count" ~ 0,
        .default = NA
      ),
      # count = ifelse(
      #   count > 1000,
      #   1000,
      #   count
      # ),
      n = ifelse(
        !is.na(count),
        count,
        presence
      ),
      model_date = start_date + (end_date - start_date)/2
    ) |>
    select(
      species,
      latitude,
      longitude,
      sampling_method,
      sampling_method_detailed,
      start_date,
      end_date,
      model_date,
      study_months,
      data_type,
      presence,
      count,
      n,
      inferred,
      source_id
    )

}
