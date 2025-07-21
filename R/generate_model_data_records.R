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
    mutate(
      sampling_method = reduce_sampling_methods(sampling_method),,
    )


}
