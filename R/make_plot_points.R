make_plot_points <- function(
    data_records,
    target_species
){

  data_records |>
    filter(species %in% target_species) |>
    mutate(
      type = case_when(
        pa == "po" ~ "presence_only",
        is.na(count) & presence == 1 ~ "presence",
        is.na(count) & presence == 0 ~ "absence",
        count > 0 ~ "presence",
        count == 0 ~ "absence"
      )
    ) |>
    select(
      species,
      lon,
      lat,
      type
    )

}
