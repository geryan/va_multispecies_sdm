calculate_posterior_multisp_pp_with_offset <- function(
    image_filename
){

  load(image_filename)

  posterior <- calculate(alpha, beta, gamma, delta, values = draws)

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


  data_infilled_sim <- bernoulli(p)
  di_sims <- calculate(data_infilled_sim, values = draws, nsim = 1000)

  dharma <- DHARMa::createDHARMa(
    simulatedResponse = t(di_sims[[1]][, , 1]),
    observedResponse = data_infilled[model_notna_idx_pa],
    integerResponse = TRUE
  )

  # extract RQR, scaled to normal distribution for easier checking
  df_validate <- df %>%
    mutate(
      z_resid = qnorm(dharma$scaledResiduals),
      # handle some Infs
      z_resid = pmin(z_resid, max(z_resid[is.finite(z_resid)])),
      z_resid = pmax(z_resid, min(z_resid[is.finite(z_resid)])),
    ) %>%
    # add on covariate values
    left_join(
      all_extract,
      by = c("cell_id", "year_id")
    ) %>%
    # make spatial clusters
    mutate(
      cluster = kmeans(
        x = as.matrix(select(., latitude, longitude)),
        centers = 15
      )$cluster
    )


}
