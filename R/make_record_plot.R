make_record_plot <- function(record_plot_data) {

  record_plot_data |>
    mutate(
      data_type = case_when(
        data_type == "po_presence" ~ "Presence only",
        data_type == "pa_absence" ~ "PA absence",
        TRUE ~ "PA presence"
      ),
      `Record\ntype` = as.factor(data_type) |>
        forcats::fct_rev()
    ) |>
    ggplot() +
    geom_bar(
      aes(
        x = species,
        y = records,
        fill = `Record\ntype`
      ),
      stat = "identity"
    ) +
    scale_y_log10() +
    scale_fill_manual(
      values = c(
        "darkorchid",
        "aquamarine2",
        "aquamarine"
      )
    ) +
    labs(
      x = "Species",
      y = "Records\n(log scale)"
    ) +
    theme(
      axis.text.x = element_text(face = "italic")
    )

}
