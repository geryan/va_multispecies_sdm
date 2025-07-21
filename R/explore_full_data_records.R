explore_full_data_records <- function(full_data_records){


  full_data_records %$%
    table(sampling_method) |>
    as_tibble()

  full_data_records %$%
    table(species) |>
    as_tibble()


  full_data_records |>
    filter(!is.na(occurrence_n)) |>
    filter(occurrence_n > 0) |>
    ggplot(
      aes(
        x = sampling_method,
        y = occurrence_n
      )
    ) +
      geom_violin() +
    geom_boxplot() +
    scale_y_log10() +
    theme(
      axis.text.x = element_text(angle = 270)
    )

    plot(
      x = full_data_records$study_months,
      y = full_data_records$occurrence_n
    )

    full_data_records |>
      ggplot() +
      geom_smooth(
        aes(
          x = study_months,
          y = occurrence_n
        )
      )


    full_data_records |>
      group_by(species) |>
      summarise(n = n())

    full_data_records |>
      group_by(species) |>
      summarise(n = n()) |>
      arrange(desc(n)) |>
      print(n = 999)


    full_data_records |>
      select(species, latitude, longitude) |>
      distinct() |>
      group_by(species) |>
      summarise(n = n()) |>
      arrange(desc(n)) |>
      print(n = 999)

  return(NULL)

}
