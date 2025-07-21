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
generate_model_data_records <- function(
    full_data_records,
    target_species
  ) {

  # full_data_records |>
  #   # target species only |>
  #   filter(species %in% target_species) %$%
  #   table(sampling_method, species)


  full_data_records |>
    # target species only |>
    filter(species %in% target_species) |>
    # fuck off vague points
    filter(
      (confidence_in_georef != "greater than 10km") |
        is.na(confidence_in_georef)
    ) |>
    # fuck off polygons and centroid of admin points
    filter(point_data) |>
    mutate(
      sampling_method = reduce_sampling_methods(sampling_method),
    ) |>
    select(
      -start_date,
      -confidence_in_georef,
      -point_data,
      -start_date,
      -end_date
    )


}
