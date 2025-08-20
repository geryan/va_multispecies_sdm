make_covariate_plots <- function(
    model_data_spatial,
    cvnames,
    w = 3200,
    h = 2000
){


  p<- model_data_spatial |>
    filter(data_type != "bg") |>
    mutate(
     dtype = case_when(
       data_type == "po" ~ "presence_po",
       data_type == "pa" & presence == 1 ~ "presence_pa",
       data_type == "pa" & presence == 0 ~ "absence_pa",
       data_type == "count" & count > 0 ~ "presence_count",
       data_type == "count" & count == 0 ~ "absence_count",
     ),
     presence = as.factor(presence)
    )|>
    pivot_longer(
      cols = all_of(cvnames),
      names_to = "var",
      values_to = "value"
    ) |>
    ggplot() +
    geom_violinhalf(
      aes(
        x = data_type,
        y = value,
        fill = presence,
        color = presence
      ),
      flip = c(1,3),
      position = position_identity()
    ) +
    facet_grid(
      species ~ var
    ) +
    theme(
      axis.text.x = element_text(
        angle = 315,
        hjust = 0,
        vjust = 0.5
      )
    ) +
    theme_minimal()

    ggsave(
      filename = "outputs/figures/cov_violins.png",
      plot = p,
      width = w,
      height = h,
      units = "px"
    )

  NULL

}
