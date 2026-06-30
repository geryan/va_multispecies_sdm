#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param image_name
#' @param n_burnin
#' @param n_samples
#' @param n_chains
#' @param n_cores
#' @return
#' @author geryan
#' @export
refit_model_multispecies_pp_count_source_effect <- function(
  fit_image_name,
  refit_image_name,
    n_burnin = 100,
    n_samples = 100,
    n_chains = 3,
    n_cores = NULL) {

  load(fit_image_name)

  nsim <- n_chains

  init_sims <- calculate(
    alpha_mean,
    alpha_sd,
    alpha_raw,
    gamma_mean,
    gamma_sd,
    gamma_raw,
    delta,
    beta_raw,
    zeta_raw,
    sampling_re_raw,
    sampling_re_sd,
    sqrt_inv_size,
    values = draws,
    nsim = nsim
  )

  inits <- split_sims(init_sims)

  Lmax <- 25
  Lmin <- round(Lmax / 8)

  draws <- greta::mcmc(
    m,
    warmup = n_burnin,
    sampler = hmc(Lmin = Lmin, Lmax = Lmax),
    n_samples = n_samples,
    chains = n_chains,
    initial_values = inits,
    n_cores = n_cores
  )


  ############
  # Save image
  ############


  # can't use save.image inside function inside targets
  # because it only saves the global environment not
  # function env.

  save(
    list = ls(all.names = TRUE),
    file = refit_image_name
  )

  return(refit_image_name)

}
