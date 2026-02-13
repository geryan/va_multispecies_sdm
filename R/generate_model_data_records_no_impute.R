#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param full_data_records
#' @param target_species description
#' @return
#' @author geryan
#' @export
generate_model_data_records_no_impute <- function(
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
      # sampling_method = reduce_sampling_methods(sampling_method),
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
    ) |>
    arrange(
      source_id,
      start_date,
      sampling_method,
      latitude,
      longitude,
      study_months
    ) |>
    group_by(
      source_id,
      start_date,
      sampling_method,
      latitude,
      longitude,
      study_months
    ) |>
    mutate(
      data_type = case_when(
        data_type_inf1 == "count" ~ "count",
        data_type_inf1 == "pa" ~ "pa",
        any(data_type_inf1 == "count") ~ "count",
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
        .default = 0
      ),
      count = case_when(
        data_type == "count" & sampling_method == "larvae" ~ NA,
        data_type == "count" & sampling_method == "other" ~ NA,
        data_type == "count" & !is.na(occurrence_n) ~ occurrence_n,
        data_type == "count" ~ 0,
        .default = NA
      ),
      count = ifelse(
        count > 1000,
        1000,
        count
      ),
      n = ifelse(
        !is.na(count),
        count,
        presence
      )
    ) |>
    select(
      species,
      latitude,
      longitude,
      sampling_method,
      study_months,
      data_type,
      presence,
      count,
      n
    )

}
