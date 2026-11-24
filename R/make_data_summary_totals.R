#' Records of all species together, by data type, for the manuscript
#'
#' One row with the columns of `make_data_summary_table()`, for every species
#' at once. It runs that function with every record given the same species, so
#' the definitions are the same. The record columns come out as the sums over
#' species. `p_unique` is the number of distinct locations (latitude and
#' longitude) with a presence of any species, so it is less than the sum over
#' species: a location where several species were recorded counts once.
#'
#' @title Data summary totals
#' @param model_data_spatial model data, with `species`, `data_type`,
#'   `presence`, `inferred`, `latitude` and `longitude`
#' @param label the value of `species` in the row
#' @return a tibble with one row
#' @author geryan
#' @export
make_data_summary_totals <- function(
    model_data_spatial,
    label = "All species"
) {

  model_data_spatial |>
    mutate(
      species = label
    ) |>
    make_data_summary_table()

}
