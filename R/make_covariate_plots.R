make_covariate_plots <- function(
    model_data_spatial,
    target_species,
    target_covariate_names,
    offset_names,
    w = 3200,
    h = 2000
){

  cvnames <- c(offset_names, target_covariate_names)

  p <- model_data_spatial |>
    filter(
      data_type != "bg",
      presence != 0
    ) |>
    pivot_longer(
      cols = all_of(cvnames),
      names_to = "var",
      values_to = "value"
    ) |>
    ggplot() +
    geom_violin(
      aes(
        x = species,
        y = value,
        fill = species
      )
    ) +
    facet_wrap(
      ~ var,
      scales = "free"
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
