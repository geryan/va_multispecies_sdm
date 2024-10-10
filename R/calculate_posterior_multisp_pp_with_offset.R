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

  # posterior predictive checking

  # draw sims of data based on estimates of p
  data_infilled_sim <- bernoulli(p)
  pa_dat_sim <- calculate(data_infilled_sim, values = draws, nsim = 1000)

  # put the actual data together with covariates values
  pa_dat <- tibble(
    row = model_notna_idx_pa[,1],
    col = model_notna_idx_pa[,2],
    obs = data_infilled[model_notna_idx_pa]
  ) |> left_join(
    y = as_tibble(x) |>
      mutate(
        row = row_number()
      ),
    by = "row"
  ) |>
    mutate(
      sp = target_species[col]
    )


  dharma <- DHARMa::createDHARMa(
    simulatedResponse = t(di_sims[[1]][, , 1]),
    observedResponse = pa_dat$obs,
    integerResponse = TRUE
  )

  # extract RQR, scaled to normal distribution for easier checking
  df_validate <- pa_dat %>%
    mutate(
      z_resid = qnorm(dharma$scaledResiduals),
      # handle some Infs
      z_resid = pmin(z_resid, max(z_resid[is.finite(z_resid)])),
      z_resid = pmax(z_resid, min(z_resid[is.finite(z_resid)])),
    )


  df_validate |>
    ggplot(
      aes(
        x = easting,
        y = northing,
        colour = z_resid
      )
    ) +
    geom_point(
      alpha = 0.5,
      size = 2
    ) +
    scale_fill_gradient(
      low = grey(0.9),
      high = grey(0.9),
      na.value = "transparent",
      guide = "none"
    ) +
    scale_colour_viridis_c() +
    theme_minimal() +
    ggtitle("Residuals")

  ggplot(df_validate) +
    geom_violin(
      aes(
        x = as.factor(obs),
        y = z_resid
      )
    ) +
    facet_wrap(~sp)


  cov_names <- colnames(x)

  df_validate_long <- df_validate  |>
    select(
      all_of(cov_names),
      sp,
      z_resid
    ) |>
    pivot_longer(
      cols = all_of(cov_names),
      names_to = "covariate_name",
      values_to = "covariate_value"
    ) |>
    group_by(
      covariate_name,
      sp
    ) |>
    # remove some outlying data skewing the smooths
    filter(
      covariate_value <= quantile(covariate_value, 0.95),
      covariate_value >= quantile(covariate_value, 0.05)
    ) |>
    ungroup()

  df_validate_long |>
    ggplot(
      aes(
        x = covariate_value,
        y = z_resid,
        group = covariate_name
      )
    ) +
    geom_point(
      alpha = 0.1
    ) +
    geom_hline(yintercept = 0,
               colour = "red",
               linetype = 2) +
    geom_smooth() +
    facet_grid(sp ~ covariate_name,
               scales = "free") +
    theme_minimal()

}
