interval_plots <- function(
  draws,
  target_species,
  target_covariate_names,
  sampling_methods = NULL
  ){

  sm <- !is.null(sampling_methods)

  sm_text <- ifelse(sm, "_sm", "")

  dsum <- summary(draws)

  df <- dsum$quantiles |>
    as_tibble() |>
    mutate(
      par = rownames(dsum$quantiles),
      idx1 = sub(
        pattern = ".*\\[",
        replacement = "",
        x = par
      ) |>
        sub(
          pattern = "\\,.*",
          replacement = "",
          x = _
        ) |>
        as.integer(),
      idx2 = sub(
        pattern = ".*\\,",
        replacement = "",
        x = par
      ) |>
        sub(
          pattern = "\\].*",
          replacement = "",
          x = _
        ) |>
        as.integer(),
      par = sub(
        pattern = "\\[.*",
        replacement = "",
        x = par
      )
    ) |>
    mutate(
      v1 = case_when(
        par == "alpha" ~ target_species[idx1],
        par == "beta" ~ target_covariate_names[idx1],
        par == "gamma" ~ target_species[idx1],
        par == "delta" ~ "intercept",
        par == "sampling_re_raw" ~ sampling_methods[idx1],
        par == "sampling_re_sd" ~ "sd"
      ),
      v2 = case_when(
        par == "beta" ~ target_species[idx2]
      )
    ) |>
    select(par, v1, v2, everything(), -idx1, -idx2)


  # alpha plots
  df |>
    filter(par == "alpha") |>
    ggplot(aes(y = v1)) +
    geom_vline(
      xintercept = 0,
      col = "grey50"
    ) +
    geom_linerange(
      aes(
        xmin =`2.5%`,
        xmax = `97.5%`
      ),
      size = 1.5,
      col = "grey70"
    ) +
    geom_linerange(
      aes(
         x = `50%`,
         xmin = `25%`,
         xmax = `75%`
      ),
      size = 2,
      col = "black",
    ) +
    geom_point(
      aes(
        x = `50%`
      ),
      size = 3,
      col = "black",
    ) +
    theme_minimal() +
    labs(
      title = "Intercept value (alpha)",
      x = "Estimate",
      y = "Species"
    ) +
    theme(axis.text.y = element_text(face = "italic"))


  ggsave(
    filename = sprintf(
      "outputs/figures/parameter_estimates/alpha%s.png",
      sm_text
    ),
    width = 2000,
    height = 1300,
    units = "px"
  )

  # beta plots
  #
  # facet by covariate
  df |>
    filter(par == "beta") |>
    ggplot(aes(y = v2)) +
    geom_vline(
      xintercept = 0,
      col = "grey50"
    ) +
    geom_linerange(
      aes(
        xmin =`2.5%`,
        xmax = `97.5%`
      ),
      size = 1,
      col = "grey70"
    ) +
    geom_linerange(
      aes(
        x = `50%`,
        xmin = `25%`,
        xmax = `75%`
      ),
      size = 1,
      col = "black",
    ) +
    geom_point(
      aes(
        x = `50%`
      ),
      size = 1,
      col = "black",
    ) +
    theme_minimal() +
    labs(
      title = "Slope value (beta)",
      x = "Estimate",
      y = "Species"
    ) +
    theme(axis.text.y = element_text(face = "italic")) +
    facet_wrap(
      .~v1,
      ncol = 3
    )


  ggsave(
    filename = sprintf(
      "outputs/figures/parameter_estimates/beta_by_cov%s.png",
      sm_text
    ),
    width = 3200,
    height = 2000,
    units = "px"
  )

  # facet by species
  df |>
    filter(par == "beta") |>
    ggplot(aes(y = v1)) +
    geom_vline(
      xintercept = 0,
      col = "grey50"
    ) +
    geom_linerange(
      aes(
        xmin =`2.5%`,
        xmax = `97.5%`
      ),
      size = 1,
      col = "grey70"
    ) +
    geom_linerange(
      aes(
        x = `50%`,
        xmin = `25%`,
        xmax = `75%`
      ),
      size = 1,
      col = "black",
    ) +
    geom_point(
      aes(
        x = `50%`
      ),
      size = 1,
      col = "black",
    ) +
    theme_minimal() +
    labs(
      title = "Slope value (beta)",
      x = "Estimate",
      y = "Covariate"
    ) +
    facet_wrap(
      .~v2,
      ncol = 3
    ) +
    theme(strip.text.x = element_text(face = "italic"))

  ggsave(
    filename = sprintf(
      "outputs/figures/parameter_estimates/beta_by_spp%s.png",
      sm_text
    ),
    width = 3200,
    height = 2000,
    units = "px"
  )

  # gamma and delta
  df |>
    filter(par == "gamma" | par == "delta") |>
    mutate(
      v1 = factor(v1, levels = c("intercept", target_species))
    ) |>
    ggplot(aes(y = v1)) +
    geom_vline(
      xintercept = 0,
      col = "grey50"
    ) +
    geom_linerange(
      aes(
        xmin =`2.5%`,
        xmax = `97.5%`
      ),
      size = 1.5,
      col = "grey70"
    ) +
    geom_linerange(
      aes(
        x = `50%`,
        xmin = `25%`,
        xmax = `75%`
      ),
      size = 2,
      col = "black",
    ) +
    geom_point(
      aes(
        x = `50%`
      ),
      size = 3,
      col = "black",
    ) +
    theme_minimal() +
    labs(
      title = "Bias intercept (delta) and slope by species (gamma)",
      x = "Estimate",
      y = "Species"
    ) #+
    #theme(axis.text.y = element_text(face = "italic"))


  ggsave(
    filename = sprintf(
      "outputs/figures/parameter_estimates/bias%s.png",
      sm_text
    ),
    width = 2000,
    height = 1300,
    units = "px"
  )

  if(sm){

    # gamma and delta
    df |>
      filter(grepl("sampling", x = par)) |>
      mutate(
        v1 = factor(v1, levels = c("sd", sampling_methods))
      ) |>
      ggplot(aes(y = v1)) +
      geom_vline(
        xintercept = 0,
        col = "grey50"
      ) +
      geom_linerange(
        aes(
          xmin =`2.5%`,
          xmax = `97.5%`
        ),
        size = 1.5,
        col = "grey70"
      ) +
      geom_linerange(
        aes(
          x = `50%`,
          xmin = `25%`,
          xmax = `75%`
        ),
        size = 2,
        col = "black",
      ) +
      geom_point(
        aes(
          x = `50%`
        ),
        size = 3,
        col = "black",
      ) +
      theme_minimal() +
      labs(
        title = "Sampling method raw estimates and sd",
        x = "Estimate",
        y = "Sampling method"
      ) #+
    #theme(axis.text.y = element_text(face = "italic"))


    ggsave(
      filename = "outputs/figures/parameter_estimates/sampling.png",
      width = 2000,
      height = 1300,
      units = "px"
    )

  }



}
