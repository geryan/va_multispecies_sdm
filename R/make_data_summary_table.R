#' Records per species by data type, for the manuscript
#'
#' Per species: count records (`count`), those with the species present
#' (`count_p`), absent (`count_0`) and inferred absent (`count_imp`); the same
#' for presence-absence records (`pa`, `pa_1`, `pa_0`, `pa_imp`); presence-only
#' records (`po`); all records with the species present (`p_n`, `count_p +
#' pa_1 + po`); and the distinct locations (latitude and longitude) of those
#' presences (`p_unique`). Background points are left out.
#'
#' Present means `presence == 1`. A count record can be present with a count
#' of 0: the species was recorded in a survey that counted others, but with no
#' count of its own.
#'
#' @title Data summary table
#' @param model_data_spatial model data, with `species`, `data_type`,
#'   `presence`, `inferred`, `latitude` and `longitude`
#' @return a tibble with one row per species
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

  t4 <- model_data_spatial |>
    filter(
      data_type != "bg",
      presence == 1
    ) |>
    distinct(
      species,
      latitude,
      longitude
    ) |>
    count(
      species,
      name = "p_unique"
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
    left_join(
      y = t4,
      by = "species"
    ) |>
    mutate(
      count_p = count - count_0,
      pa_1 = pa - pa_0,
      p_n = count_p + pa_1 + po
    ) |>
    select(
      species,
      count,
      count_p,
      count_0,
      count_imp,
      pa,
      pa_1,
      pa_0,
      pa_imp,
      po,
      p_n,
      p_unique
    )


}
