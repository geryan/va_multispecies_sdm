make_pa_plot_data <- function(pa_data_records){
  pad <- pa_data_records |>
    filter(pa == "pa") |>
    group_by(species) |>
    summarise(
      n = n(),
      pa_presence = sum(presence),
      .groups = "drop"
    ) |>
    mutate(
      pa_absence = n - pa_presence
    ) |>
    select(-n)

  pod <- pa_data_records |>
    filter(pa == "po") |>
    group_by(species) |>
    summarise(
      po_presence = n(),
      .groups = "drop"
    )

  pad |>
    left_join(
      pod,
      by = "species"
    ) |>
    pivot_longer(
      cols = -species,
      names_to = "data_type",
      values_to = "records"
    )
}
