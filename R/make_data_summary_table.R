#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model_data_spatial
#' @return
#' @author geryan
#' @export
make_data_summary_table <- function(model_data_spatial) {

  t1 <- model_data_spatial |>
    filter(data_type != "bg") |>
    group_by(species, data_type) |>
    summarise(
      n = n(),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = data_type,
      values_from = n
    )

  t2 <- model_data_spatial |>
    filter(data_type != "bg") |>
    filter(presence == 0) |>
    group_by(species, data_type) |>
    summarise(
      n = n(),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = data_type,
      values_from = n
    ) |>
    rename(
      count_0 = count,
      pa_0 = pa
    )

  t3 <- model_data_spatial |>
    filter(data_type != "bg") |>
    filter(presence == 0) |>
    filter(inferred) |>
    group_by(species, data_type) |>
    summarise(
      n = n(),
      .groups = "drop"
    ) |>
    pivot_wider(
      names_from = data_type,
      values_from = n
    ) |>
    rename(
      count_imp = count,
      pa_imp = pa
    )

  t1 |>
    left_join(
      y = t2,
      by = "species"
    ) |>
    left_join(
      y = t3,
      by = "species"
    ) |>
    select(
      species,
      count,
      count_0,
      count_imp,
      pa,
      pa_0,
      pa_imp,
      po
    )


}
