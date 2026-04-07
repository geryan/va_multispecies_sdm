#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param model_fit_image_multisp_pp_count_sm
#' @param nsims
#' @return
#' @author geryan
#' @export
validation_and_checking <- function(
    model_fit_image_multisp_pp_count_sm,
    nsims = 100,
    plotdir = "outputs/figures/ppc_sm"
  ) {


  load(model_fit_image_multisp_pp_count_sm)


  ############
  # prior predictive checks
  ############

  # prepare data - it's in greta format so get it out

  # convert list to vectors / matrices
  po_dat <- po_data_response |>
    as.numeric()
  pa_dat <- pa_data_response |>
    as.numeric()
  count_dat <- count_data_response |>
    as.numeric()

  # make list for passing to checks
  dat <- list(
    pa_dat = pa_dat,
    po_dat = po_dat,
    count_dat = count_dat
  )

  # simulate data from prior
  prior_preds_calc <- calculate(
    po_data_response,
    pa_data_response,
    count_data_response,
    nsim = nsims
  )

  # convert into matrices
  po_pred_prior <- prior_preds_calc$po_data_response[,,1] |>
    as.matrix()
  pa_pred_prior <- prior_preds_calc$pa_data_response[,,1] |>
    as.matrix()
  count_pred_prior <- prior_preds_calc$count_data_response[,,1] |>
    as.matrix()

  # make list for passing to checks
  preds_prior <- list(
    po_pred = po_pred_prior |>
      as.matrix(),
    pa_pred = pa_pred_prior |>
      as.matrix(),
    count_pred = count_pred_prior |>
      as.matrix()
  )

  # pass data to checks
  predictive_checks(
    preds = preds_prior,
    dat = dat,
    output_prefix = sprintf(
      "%s/prior",
      plotdir
    )
  )


  ############
  # posterior predictive checks
  ############

  # using bayesplot

  # simulate data from posterior
  posterior_calc <- calculate(
    po_data_response,
    pa_data_response,
    count_data_response,
    values = draws,
    nsim = nsims
  )

  # convert into matrices
  po_pred_post <- posterior_calc$po_data_response[,,1] |>
    as.matrix()
  pa_pred_post <- posterior_calc$pa_data_response[,,1] |>
    as.matrix()
  count_pred_post <- posterior_calc$count_data_response[,,1] |>
    as.matrix()

  # make list for passing to checks
  preds_posterior <- list(
    po_pred = po_pred_post |>
      as.matrix(),
    pa_pred = pa_pred_post |>
      as.matrix(),
    count_pred = count_pred_post |>
      as.matrix()
  )

  # pass data to checks
  predictive_checks(
    preds = preds_posterior,
    dat = dat,
    output_prefix = sprintf(
      "%s/posterior",
      plotdir
    )
  )


  ####
  # using DHARMa
  ####

  counts_resid <- DHARMa::createDHARMa(
    simulatedResponse = t(preds_posterior$count_pred),
    observedResponse = dat$count_dat,
    integerResponse = TRUE
  )

  pa_resid <- DHARMa::createDHARMa(
    simulatedResponse = t(preds_posterior$pa_pred),
    observedResponse = dat$pa_dat,
    integerResponse = TRUE
  )

  po_resid <- DHARMa::createDHARMa(
    simulatedResponse = t(preds_posterior$po_pred),
    observedResponse = dat$po_dat,
    integerResponse = TRUE
  )

  # plot the residuals overall (aiming for uniform distribution)
  png(
    sprintf(
      "%s/residuals.png",
      plotdir
    ),
    width = 1000,
    height = 1000,
    res = 150
  )
  par(mfrow = c(3, 1))
  hist(counts_resid$scaledResiduals,
       main = "count data")
  hist(pa_resid$scaledResiduals,
       main = "presence/absence data")
  hist(po_resid$scaledResiduals,
       main = "presence/background data")
  dev.off()

  # subset the information (the same way as in fit_model_multisp_pp_count to make
  # the observation data) and add residuals
  count_data_resids <- model_data |>
    filter(data_type == "count") |>
    mutate(
      residual = counts_resid$scaledResiduals,
      residual_norm = qnorm(pmin(pmax(residual, 1e-6), 1 - 1e-6))
    )

  pa_data_resids <- model_data |>
    dplyr::filter(data_type == "pa") |>
    mutate(
      residual = pa_resid$scaledResiduals,
      residual_norm = qnorm(pmin(pmax(residual, 1e-6), 1 - 1e-6))
    )

  po_data_resids <- model_data |>
    dplyr::filter(data_type %in% c("po", "bg")) |>
    mutate(
      residual = po_resid$scaledResiduals,
      residual_norm = qnorm(pmin(pmax(residual, 1e-6), 1 - 1e-6))
    )

  all_data_resids <- bind_rows(
    count = count_data_resids,
    pa = pa_data_resids,
    pobg = po_data_resids,
    .id = "data_type_plot"
  )

  # plot the residuals against:
  #   - species (vs data type)
  #   - detailed sampling method (vs data type)
  #   - detailed sampling method vs species (for each data type)
  #   - detection covariate (for presence-background data only)
  #   - month, faceted by country (for count data only)



  #   - species (vs data type)
  all_data_resids |>
    ggplot(
      aes(
        x = residual
      )
    ) +
    geom_histogram(
      bins = 10
    ) +
    facet_grid(
      species ~ data_type_plot,
      scales = "free_y"
    ) +
    theme_minimal()
  ggsave(
    filename = sprintf(
      "%s/redids_spp_by_type.png",
      plotdir
    )
  )

  # Mass close to 0: data at the lower value end of the posterior, so
  # overpredicting

  # Mass close to 1: data at the upper value end of the posterior, so
  # underpredicting

  # Count data are quite skewy, with underprediction
  # some underprediction.

  #   - detailed sampling method (vs data type)
  all_data_resids |>
    ggplot(
      aes(
        x = residual
      )
    ) +
    geom_histogram(
      bins = 10
    ) +
    facet_grid(
      sampling_method ~ data_type_plot,
      scales = "free_y"
    ) +
    theme_minimal()
  ggsave(
    filename = sprintf(
      "%s/redids_sampling_by_type.png",
      plotdir
    )
  )

  # not much evidence for misfitting by sampling method


  #   - detailed sampling method (vs data type)
  count_plot <- count_data_resids |>
    ggplot(
      aes(
        x = residual
      )
    ) +
    geom_histogram(
      bins = 10
    ) +
    facet_grid(
      species ~ sampling_method_detailed,
      scales = "free_y"
    ) +
    theme_minimal()
  count_plot
  ggsave(
    filename = sprintf(
      "%s/redids_count_spp_by_sampling.png",
      plotdir
    ),
    plot = count_plot
  )

  # for count data, detailed sampling methods are the same as reduced?

  # most data for HRI, arabiensis/funestus very overdispersed, nili
  # underdispersed. Cancelling out in the earlier plot?

  # coluzzii underdispersed? Lots of noise / small sample size here

  #   - detailed sampling method (vs data type)
  pa_plot <- pa_data_resids |>
    ggplot(
      aes(
        x = residual
      )
    ) +
    geom_histogram(
      bins = 10
    ) +
    facet_grid(
      species ~ sampling_method_detailed,
      scales = "free_y"
    ) +
    theme_minimal()
  pa_plot

  ggsave(
    filename = sprintf(
      "%s/redids_pa_spp_by_sampling.png",
      plotdir
    ),
    plot = pa_plot
  )

  # larval PA data overdispersed, especially for arabiensis and coluzzi and
  # gambiae

  po_plot <- po_data_resids |>
    ggplot(
      aes(
        x = residual
      )
    ) +
    geom_histogram(
      bins = 10
    ) +
    facet_grid(
      species ~ sampling_method,
      scales = "free_y"
    ) +
    theme_minimal()
  po_plot

  ggsave(
    filename = sprintf(
      "%s/redids_po_spp_by_sampling.png",
      plotdir
    ),
    plot = po_plot
  )


  # detection covariate
  po_data_resids |>
    mutate(
      travel_time_bins = cut(
        travel_time,
        c(0, 0.8, 1))
    ) |>
    ggplot(
      aes(
        x = residual
      )
    ) +
    geom_histogram(
      bins = 10
    ) +
    facet_grid(
      travel_time_bins ~ species,
      scales = "free_y"
    ) +
    theme_minimal()

  ggsave(
    filename = sprintf(
      "%s/redids_po_detection_by_species.png",
      plotdir
    )
  )

  # po/bg predictions are underpredicting for very high values of travel time


  #   - month, faceted by country (for count data only)
  count_data_resids |>
    filter(
      as.numeric(end_date - start_date) < 32
    ) |>
    mutate(
      month = lubridate::month(model_date)
    ) |>
    ggplot(
      aes(
        x = residual
      )
    ) +
    geom_histogram(
      bins = 10
    ) +
    facet_grid(
      ~ month,
      scales = "free_y"
    ) +
    theme_minimal()
  ggsave(
    filename = sprintf(
      "%s/redids_count_month.png",
      plotdir
    )
  )

  # possible underprediction in some months, but it doesn't seem systematic re.
  # month. Note also that this would be more meaningful splot by region (different
  # seasonal peaks)


  # plot each species residuals over space
  count_data_resids |>
    arrange(
      abs(residual_norm)
    ) |>
    ggplot(
      aes(
        x = longitude,
        y = latitude,
        colour = residual_norm
      )
    ) +
    geom_point(
      data = po_data_resids,
      colour = "grey",
      size = 2
    ) +
    geom_point() +
    facet_wrap(
      ~species
    ) +
    theme_minimal() +
    coord_equal() +
    scale_color_gradient2(
      low = "forestgreen",
      high = "purple"
    ) +
    ggtitle(
      "count data",
      "green = overprediction, purple = underprediction"
    )
  ggsave(
    filename = sprintf(
      "%s/redids_count_spp_spatial.png",
      plotdir
    )
  )


  pa_data_resids |>
    arrange(
      abs(residual_norm)
    ) |>
    ggplot(
      aes(
        x = longitude,
        y = latitude,
        colour = residual_norm
      )
    ) +
    geom_point(
      data = po_data_resids,
      colour = "grey",
      size = 2
    ) +
    geom_point() +
    facet_wrap(
      ~species
    ) +
    theme_minimal() +
    coord_equal() +
    scale_color_gradient2(
      low = "forestgreen",
      high = "purple"
    ) +
    ggtitle(
      "presence-absence data",
      "green = overprediction, purple = underprediction"
    )

  ggsave(
    filename = sprintf(
      "%s/redids_pa_spp_spatial.png",
      plotdir
    )
  )


  po_data_resids |>
    arrange(
      abs(residual_norm)
    ) |>
    ggplot(
      aes(
        x = longitude,
        y = latitude,
        colour = residual_norm
      )
    ) +
    geom_point(
      data = po_data_resids,
      colour = "grey",
      size = 2
    ) +
    geom_point() +
    facet_wrap(
      ~species
    ) +
    theme_minimal() +
    coord_equal() +
    scale_color_gradient2(
      low = "forestgreen",
      high = "purple"
    ) +
    ggtitle(
      "presence-background data",
      "green = overprediction, purple = underprediction"
    )

  ggsave(
    filename = sprintf(
      "%s/redids_po_spp_spatial.png",
      plotdir
    )
  )


  # plot these against covariate values

  all_data_resids |>
    pivot_longer(
      cols = all_of(target_covariate_names),
      values_to = "covariate_value",
      names_to = "covariate_name"
    ) |>
    ggplot(
      aes(
        x = covariate_value,
        y = residual_norm
      )
    ) +
    geom_point() +
    geom_smooth() +
    facet_grid(
      species ~ covariate_name,
      scales = "free_y"
    ) +
    theme_minimal()

  ggsave(
    filename = sprintf(
      "%s/redids_spp_by_cov.png",
      plotdir
    )
  )



  # no very obvious residual (non-linear) relationships for included covariates.


  # zoom in on count data for arabiensis and funestus


  # plot each species residuals over space
  count_data_resids |>
    arrange(
      abs(residual_norm)
    ) |>
    filter(
      species %in% c("arabiensis", "funestus")
    ) |>
    ggplot(
      aes(
        x = longitude,
        y = latitude,
        colour = residual_norm
      )
    ) +
    geom_point(
      data = po_data_resids |>
        filter(
          species %in% c("arabiensis", "funestus")
        ),
      colour = "grey",
      size = 4
    ) +
    geom_point(
      size = 0.5
    ) +
    facet_wrap(
      ~species
    ) +
    theme_minimal() +
    coord_equal() +
    scale_color_gradient2(
      low = "forestgreen",
      high = "purple"
    ) +
    ggtitle("count data",
            "green = overprediction, purple = underprediction")

  ggsave(
    filename = sprintf(
      "%s/redids_count_spatial_arabiensis_funestus.png",
      plotdir
    )
  )



  ###############
  # Convergence checking
  # traceplots and rhat
  ###############

  # traceplots
  mcmc_trace(
    x = draws,
    regex_pars = "alpha"
  )
  ggsave(
    "outputs/figures/traceplots/sm_alpha.png"
  )

  # too many, takes forever, you can't read them anyway
  # mcmc_trace(
  #   x = draws,
  #   regex_pars = "beta"
  # )
  # ggsave(
  #   "outputs/figures/traceplots/sm_beta.png"
  # )

  mcmc_trace(
    x = draws,
    regex_pars = c("delta", "gamma")
  )
  ggsave(
    "outputs/figures/traceplots/sm_delta_gamma.png"
  )

  mcmc_trace(
    x = draws,
    regex_pars = "sampling"
  )
  ggsave(
    "outputs/figures/traceplots/sm_sampling.png"
  )

  rhats <- coda::gelman.diag(draws,
                    autoburnin = FALSE,
                    multivariate = FALSE)

  write_csv(
    rhats,
    file = "outputs/last_rhats.csv"
  )


  ###### Plot parameter estimates

  # EDIT SO IT DOESNT DO ALL THE FUCKING BETAS
  # specify pars
  interval_plots(
    draws = draws,
    target_species = target_species,
    target_covariate_names = target_covariate_names,
    sampling_methods = sampling_methods
  )

  NULL

}
