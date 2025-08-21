make_covariate_plots <- function(
    model_data_spatial,
    cvnames,
    w = 3200,
    h = 2000
){


  pdat <- model_data_spatial |>
    filter(data_type == "bg") |>
    select(-species) |>
    expand_grid(
      species = unique(model_data_spatial$species)[!is.na(unique(model_data_spatial$species))]
    ) |>
    select(species, everything()) |>
    bind_rows(
      model_data_spatial |>
        filter(data_type != "bg")
    ) |>
    pivot_longer(
      cols = all_of(cvnames),
      names_to = "var",
      values_to = "value"
    ) |>
    mutate(
      dtype = case_when(
        data_type == "po" ~ "po",
        data_type == "bg" ~ "po",
        data_type == "pa" ~ "pa",
        data_type == "count" ~ "count",
      ),
      pres = case_when(
        data_type == "count" & count > 0 ~ "present",
        data_type == "count" & count == 0 ~ "absent",
        presence == 1 ~ "present",
        presence == 0 ~ "absent"
      )
    )



  ggplot(pdat) +
    geom_violinhalf(
      aes(
        x = dtype,
        y = value,
        fill = pres,
        color = pres
      ),
      flip = c(1, 3, 5),
      position = position_identity()
    ) +
    facet_grid(
      #species ~ var
      var ~ species,
      scales = "free"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(
        angle = 315,
        hjust = 0,
        vjust = 0.5
      )
    ) +
    labs(
      x = "Data type",
      y = "Standardised value",

    )

    ggsave(
      filename = "outputs/figures/cov_violins.png",
      plot = p,
      width = w,
      height = h,
      units = "px"
    )

  NULL

}
