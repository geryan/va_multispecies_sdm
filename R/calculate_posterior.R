calculate_posterior <- function(
    image_filename
){

  load(image_filename)

  posterior <- calculate(alpha, beta, gamma, delta, values = draws, nsim = 100)

  post_alpha <- posterior$alpha[,,1]
  colnames(post_alpha) <- target_species

  post_alpha <- post_alpha |>
    as_tibble() |>
    pivot_longer(
      cols = everything(),
      names_to = "species",
      values_to = "value"
    )

  ggplot(post_alpha) +
    geom_violin(
      aes(
        x = species,
        y = value,
        fill = species
      )
    ) +
    labs(y = "alpha posterior")

  ggsave(
    filename = sprintf(
      "outputs/figures/cov_violins/pa_%s.png",
      cvnames[i]
    ),
    plot = paplot,
    width = w,
    height = h,
    units = "px"
  )

}
