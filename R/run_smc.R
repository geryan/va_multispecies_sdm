# Apply SMC to a greta model to approximate the posterior, and provide
# information to initialise MCMC sampling. run_smc() is the main function

# the following functions are copied from generate_valid_inits:
# get_free_states
# make_inits
# enforce_dim

# given a number of samples n and a list of greta arrays, use greta::calculate()
# to return a list of prior samples of those greta arrays
prior_sim_once <- function(n, greta_array_list, ...) {
  do.call(calculate,
          c(greta_array_list, list(nsim = n, ...)))
}

# attempt to do this without rebuilding dags all the time
prior_sim_once_fast <- function(n, greta_array_list, dag) {

  greta:::calculate_target_tensor_list(
    dag = dag,
    fixed_greta_arrays = list(),
    values = list(),
    stochastic = TRUE,
    target = greta_array_list,
    nsim = n
  )

}

sample_free_states_from_priors <- function(mod, n) {

  # 1. simulate n times from priors for all parameters (as in calculate)
  variable_nodes <- mod$dag$node_list[mod$dag$node_types == "variable"]
  variable_greta_arrays <- lapply(variable_nodes,
                                  greta:::as.greta_array)

  # create a new dag, so we don't mess up the main one by changing it to
  # sampling mode
  dag <- greta:::dag_class$new(variable_greta_arrays, tf_float = "float64")
  sims <- prior_sim_once_fast(n = n,
                              greta_array_list = variable_greta_arrays,
                              dag = dag)

  # 2. convert these back to free state values
  inits_list <- lapply(seq_len(n),
                       make_inits,
                       sims,
                       variable_greta_arrays)

  free_states <- get_free_states(inits_list,
                                 variable_greta_arrays,
                                 mod)

  free_states
}

# convert an inits list into a matrix of free states
get_free_states <- function(inits_list, variable_greta_arrays, mod) {
  # attach the variable greta arrays here, to be found by
  # parse_initial_values(), which is hard-coded to look in a particular number
  # of parent frames above
  attach(variable_greta_arrays, warn.conflicts = FALSE)
  free_state_list <- lapply(inits_list,
                            greta:::parse_initial_values,
                            mod$dag)
  do.call(rbind, free_state_list)
}

# need to convert these outputs to inits
make_inits <- function(i, simulations, greta_arrays) {
  values <- lapply(simulations, greta.dynamics:::slice_first_dim, i)
  values <- mapply(enforce_dim, values, greta_arrays, SIMPLIFY = FALSE)
  do.call(initials, values)
}

# ensure the r value has the same dimensions as the corresponding greta array
enforce_dim <- function(r_value, greta_array) {
  array(r_value, dim = dim(greta_array))
}

# check that all the variable greta arrays are available so that inits can be
# specified for them
inits_are_deterministic <- function(mod) {

  # check all the required greta arrays are visible
  visible_greta_arrays <- mod$visible_greta_arrays
  visible_nodes <- lapply(visible_greta_arrays, greta:::get_node)
  visible_node_names <- vapply(visible_nodes,
                               greta:::member,
                               "unique_name",
                               FUN.VALUE = character(1))

  variable_nodes <- mod$dag$node_list[mod$dag$node_types == "variable"]
  variable_node_names <- vapply(variable_nodes,
                                greta:::member,
                                "unique_name",
                                FUN.VALUE = character(1))

  all(variable_node_names %in% visible_node_names)

}

# function to create a list of initial values object from a matrix of free states
initials_from_free_states <- function(mod, free_states) {

  if(!inits_are_deterministic(mod)) {
    msg <- cli::format_warning(
      c("not all variable greta arrays are visible in the current workspace",
        "some initial values for some variables cannot all be validated")
    )
    warning(msg, call. = FALSE)
  }

  n <- nrow(free_states)
  dag <- mod$dag
  tfe <- dag$tf_environment
  visible_greta_arrays <- mod$visible_greta_arrays

  # find visible greta arrays that are variables
  vga_nodes <- lapply(visible_greta_arrays, greta:::get_node)
  vga_node_names <- vapply(vga_nodes,
                           greta:::member,
                           "unique_name",
                           FUN.VALUE = character(1))
  all_variable_node_names <- names(dag$node_types[dag$node_types == "variable"])
  keep <- vga_node_names %in% all_variable_node_names
  visible_variable_greta_arrays <- visible_greta_arrays[keep]

  # create a new dag based on these, to compute values from free state
  new_dag <- greta:::dag_class$new(visible_variable_greta_arrays)
  new_dag$mode <- "all_forward"
  sims <- new_dag$trace_values(free_states, flatten = FALSE)

  # split the greta array values into inits with make_inits()
  inits_list <- lapply(seq_len(n),
                       make_inits,
                       sims,
                       visible_variable_greta_arrays)

  inits_list
}


# compute the parameters of current particles needed to compute the
# cross-correlation with the another set of particles
particle_corr_params <- function(x) {
  err <- sweep(x, 2, colMeans(x), FUN = "-")
  sd <- sqrt(colSums(err ^ 2))
  list(
    error = err,
    sd = sd
  )
}

# given some particles y, and the parameters of a previous set of particles x
# (x_params computed with particle_corr_params()), compute the
# cross-correlations between the particles
particle_crosscor <- function(y, x_params) {
  y_params <- particle_corr_params(y)
  cross_err <- colSums(x_params$error * y_params$error)
  cross_correl <- abs(cross_err / (x_params$sd * y_params$sd))
  cross_correl
}

# efficient log density of MV normal distribution, given precomputed cholesky
# factor
log_prob_mvn <- function(z, mean, U) {
  k <- ncol(U)
  const <- k * log(2 * pi)
  log_det <- 2 * sum(log(diag(U)))
  tmp <- base::backsolve(U, t(z) - mean, transpose = TRUE)
  rss <- colSums(tmp ^ 2)
  -0.5 * (log_det + const + rss)
}

# efficient simulation from a multivariate random normal, given precomputed
# cholesky factor
sim_mvn <- function(n, mu, L) {
  k <- nrow(L)
  u <- matrix(rnorm(n * k), k, n)
  u_scaled <- L %*% u
  t(sweep(u_scaled, 1, mu, FUN = "+"))
}

# numerically stable log(sum(exp(x)))
logsumexp <- function(x) {
  max <- max(x)
  max + log(sum(exp(x - max)))
}



# Given a greta model object, run Sequential Monte Carlo (SMC) with tempering to
# generate n_particles simulations from the posterior density of the model. For
# most models, these are likely to be a less accurate representation of the
# posterior than samples from MCMC, since they are heavily influenced by a
# multivariate normal kernel used to update the estimates. However these samples
# are likely to be useful for initialising MCMC chains, by enabling good initial
# values and an estimate of the diagonal mass matrix for tuning the sampler.
# This code is heavily inspired by the pyMC's excellent implementation of SMC.

# greta_model - a greta model oject returned by greta:model()

# n_prior_samples - the number of particles to simulate from the prior
# in order to approximate the prior density.

# relative_ess_target - the target ratio of the effective sample size to the
# number particles after resampling, used for tuning inverse_temperature increases. Ie.
# if relative_ess_target = 0.5, then after resampling the effective sample size
# of the particles will be half of n_particles.

# mh_correl_threshold - the threshold to determine whether successive MCMC
# steps are continuing to treduce the correlation between the particles and
# their pre-MCMC (ie. immediately post resampling) states. After each MCMC step,
# the Pearson cross-correlations are computed between the new particles and the
# pre-MCMC particles. A particle is judged to have successfully reduced
# correlation in that step if the correlation reduces by
# 'mh_correl_threshold' or more. When fewer than 90% of particles have
# reduced correlation by this much over the previus MCMC step, no further MCMC
# steps are taken.

# mh_type - the type of Metropolis Hasting proposal distribution to
# use. Both employ the multivariate normal approximation to the target
# distribution to inform proposals. mh_type = "standard" generates a new
# proposed particle location for each particle by taking a zero-mean
# multivariate normal jump, with covariance matrix given by the estimated target
# distribution covariance matrix, but with the marginal standard deviations
# scaled by mh_rel_epsilon, to reduce excessively large steps. mh_type =
# "independent" proposes new particles independent of the previous particle
# location by sampling directly from the estimated multivariate normal
# distribution approximating the target distribution. The acceptance criterion
# is adjusted to ensure the sampler has detailed balance. This is the default
# option since it does not require tuning, and performs well.

# mh_rel_epsilon - the step size of a standard Metropolis Hastings proposal,
# relative to the marginal standard deviations of the target distribution. This
# is only used if mh_type = "standard"

# max_stages - the maximum number of tempering stages to run, before stopping
# the algorithm. If the algorithm stops before the model is fully tempered, this
# will be evident in the convergence diagnostics, which will report the final
# inverse_temperature.

# max_tune_attempts - the maximum number of times the proposed temprature
# increase is adjusted at each tempering stage, to achieve an effective sample
# size determined by relative_ess_target. The remaining inverse_temperature range is
# successively halved until the expected ESS is above that amount.

# max_mh_steps - the maximum number of Metropolis Hastings steps to run
# for particles after resampling at each stage. Ideally fewer will result in
# sufficient reduction in particle degeneracy (controlled by
# mh_correl_threshold) so this just provides an upper limit to control run
# time.

# final_mh_steps - the number of additional MCMC iterations to run for each
# particle on the posterior distribution after completing the final stage of
# tempering

# compute_batch_size - the maximum number of particles to process simultaneously
# when evaluating greta functions; sampling from the model prior and
# evaluatingthe model log posterior. While processing all samples is most
# efficient (greta parallelises the operations well), this can run up against
# memory constraints. Reduce this to reduce memory use.

# Details

# Note that we approximate the prior density as a multivariate normal when
# simulating the initial particles and computing the tempered distributions.
# This has no impact on the final density, since it depends only on the
# posterior, which is computed explicitly using greta. We do this since
# computing the prior density directly using greta is both fiddly (greta makes
# no explicit distinction between prior and likelihood, only distributions,
# variables, and data) and potentially computationally intensive.

# Value

# A named list with elements:

# particles - a matrix of the final particle states, if the model has converged
# and worked well, these will be samples from the model posterior of the
# models's 'free state' transformation. Ie. the vector of unconstrained
# real-valued parameter values. See `initials_from_free_states()` to convert
# these to values corresponding to greta arrays

# convergence - a logical indicating whether the SMC tempering algorithm
# successfully reached the final stage (and the particles can be considered as
# posterior samples). A warning will also be issued if the algorithm reaches
# max_stages before convergence.

# inverse_temperature - the inverse temperature reached by the end of sampling.
# If convergence = TRUE, this will be 1.

# inverse_temperature - the inverse temperature reached by the end of sampling.
# If convergence = TRUE, this will be 1.

# tempering_stages - the number of tempering stages reached by the end of sampling.

run_smc <- function(greta_model,
                    n_particles = 1000,
                    relative_ess_target = 0.5,
                    mh_correl_threshold = 0.01,
                    mh_type = c("independent", "standard"),
                    mh_rel_epsilon = 0.01,
                    n_prior_samples = n_particles,
                    max_stages = 100,
                    max_tune_attempts = 20,
                    max_mh_steps = 5,
                    final_mh_steps = 50,
                    compute_batch_size = Inf) {

  # choose the Metropolis Hastings proposal type
  mh_type <- match.arg(mh_type)

  # log ESS target when tuning inverse temperature
  log_ess_target <- log(relative_ess_target * n_particles)

  # make sure the batch size is no bigger than the number of particles
  compute_batch_size <- min(compute_batch_size, n_particles)

  # create an index to the batches
  particle_index <- seq_len(n_particles)
  batch_index_list <- split(particle_index,
                            ceiling(particle_index / compute_batch_size))
  n_batches <- length(batch_index_list)

  # get the greta dag object for running tensorflow, and moving to and from the
  # free state (unconstrained parameter space)
  dag <- greta_model$dag


  # simulate values of the free state of the model

  # a progress bar for prior sampling
  pb_prior <- progress::progress_bar$new(
    " prior simulation [:bar] :percent eta: :eta",
    clear = FALSE,
    show_after = 0
  )

  pb_prior$update(
    0
  )

  # loop through the batch sizes simulating
  free_prior_sims_list <- list()
  for (batch in seq_len(n_batches)) {

    # simulate the batch
    free_prior_sims_list[[batch]] <- sample_free_states_from_priors(
      mod = greta_model,
      n = length(batch_index_list[[batch]])
    )

    # update the progress bar
    pb_prior$update(
      batch / n_batches
    )

  }

  free_prior_sims <- do.call(rbind, free_prior_sims_list)

  # finalise and terminate the prior sampling progress bar
  pb_prior$terminate()


  # approximate this distribution with a multivariate normal
  prior_mean <- colMeans(free_prior_sims)
  prior_cov <- cov(free_prior_sims)

  # pre-compute parameters of a computationally efficient MVN approximation to
  # the prior, to quickly evaluate the density
  k <- length(prior_mean)
  const <- k * log(2 * pi)
  prior_U <- chol(prior_cov)
  log_det <- 2 * sum(log(diag(prior_U)))

  # approximate (MVN) prior log prob function for this model, with lexical
  # scoping to avoid recomputing components
  prior_log_prob_mvn <- function(z) {
    tmp <- base::backsolve(prior_U,
                           t(z) - prior_mean,
                           transpose = TRUE)
    rss <- colSums(tmp ^ 2)
    -0.5 * (log_det + const + rss)
  }


  posterior_log_prob_batch <- function(index, z) {
    z_batch <- z[index, ]
    tf_result <- dag$tf_log_prob_function_adjusted(z_batch)
    as.vector(tf_result)
  }

  # 'exact' posterior log prob function for this model, in batches
  posterior_log_prob <- function(z) {
    batch_logprobs <- lapply(batch_index_list,
                             posterior_log_prob_batch,
                             z)
    unlist(batch_logprobs)
  }

  # generate the particles from the prior
  if (n_particles >= n_prior_samples) {
    # if there are enough, re-use the prior samples as the particles
    particles <- free_prior_sims[seq_len(n_particles), ]
  } else {
    # otherwise, simulate from the MVN approximating the prior
    particles <- sim_mvn(n_particles, mu = prior_mean, L = t(prior_U))
  }

  # compute the posterior, approximate prior, and approximate log likelihood for
  # the particles
  prior_mvn_log_dens <- prior_log_prob_mvn(particles)
  posterior_log_dens <- posterior_log_prob(particles)
  log_likelihood <- posterior_log_dens - prior_mvn_log_dens


  # run the SMC algorithm

  # start a progress bar for tempering
  pb_smc <- progress::progress_bar$new(
    " SMC tempering [:bar] :percent eta: :eta",
    clear = FALSE,
    show_after = 0
  )

  pb_smc$update(
    0
  )

  # initialise the main loop, starting the inverse temperature parameter at 0
  stage <- 1
  inverse_temperature <- 0


  # iterate until we hit the final inverse_temperature (samples from the posterior), or
  # time out
  while (inverse_temperature < 1 & stage <= max_stages) {

    # Tune the inverse_temperature increase to meet a specific ESS. Ideally,
    # jump all the way to the end (so start the proposed increase accordingly),
    # though we will successively halve to get sufficient ESS, as determined by
    # relative_ess_target
    inverse_temperature_increase <- 1 - inverse_temperature
    log_ess <- -Inf
    tune_attempts <- 0

    while(log_ess < log_ess_target & tune_attempts < max_tune_attempts) {

      tune_attempts <- tune_attempts + 1

      # # we have these old and new target densities, where the particles are
      # # samples from the old so this is the proposal
      # log_dens_target_old = posterior_log_dens * inverse_temperature_old +
      #   prior_mvn_log_dens * (1 - inverse_temperature_old)
      # log_dens_target_new = posterior_log_dens * inverse_temperature_new +
      #   prior_mvn_log_dens * (1 - inverse_temperature_new)
      #
      # # working with one generic version of this expression to simplify notation:
      # log_dens_target = posterior_log_dens * inverse_temperature +
      #   prior_mvn_log_dens * (1 - inverse_temperature)
      #
      # # we factorise the posterior into the prior and the likelihood
      # posterior_log_dens = log_likelihood + prior_mvn_log_dens
      #
      # # so substitute these in and expand out
      # log_dens_target = (log_likelihood + prior_mvn_log_dens) * inverse_temperature +
      #   prior_mvn_log_dens * (1 - inverse_temperature)
      #
      # log_dens_target = inverse_temperature * log_likelihood +
      #   inverse_temperature * prior_mvn_log_dens +
      #   prior_mvn_log_dens * (1 - inverse_temperature)
      #
      # # factorise the prior until the second inverse_temperature term cancels out
      # log_dens_target = inverse_temperature * log_likelihood +
      #   prior_mvn_log_dens * (inverse_temperature + 1 - inverse_temperature)
      # log_dens_target = inverse_temperature * log_likelihood + prior_mvn_log_dens
      #
      # # put this back into the old/new target distribution form:
      # log_dens_target_old = inverse_temperature_old * log_likelihood + prior_mvn_log_dens
      # log_dens_target_new = inverse_temperature_new * log_likelihood + prior_mvn_log_dens
      #
      # # the (unnormalised) weights are the ratio of these densities, so difference
      # # in log space
      # log_weights = log_dens_target_new - log_dens_target_old
      #
      # # substitute in the simplified term, the prior cancels out and the log
      # # likelihood factorises
      # log_weights = (inverse_temperature_new * log_likelihood + prior_mvn_log_dens) -
      #   (inverse_temperature_old * log_likelihood + prior_mvn_log_dens)
      # log_weights = inverse_temperature_new * log_likelihood + prior_mvn_log_dens -
      #   inverse_temperature_old * log_likelihood - prior_mvn_log_dens
      # log_weights = inverse_temperature_new * log_likelihood -
      #   inverse_temperature_old * log_likelihood
      # log_weights = log_likelihood * (inverse_temperature_new - inverse_temperature_old)
      #
      # # since we control the inverse_temperature difference:
      # inverse_temperature_increase = inverse_temperature_new - inverse_temperature_old
      # # substitute this in
      # log_weights = log_likelihood * inverse_temperature_increase

      # so we just need to compute the log likelihood at the set of particles, and
      # the weights are linear in the inverse_temperature, so we can optimise the
      # inverse_temperature increase
      log_weights <- log_likelihood * inverse_temperature_increase

      # normalise the weights, in log space for numerical stability, and compute
      # the expected (log) effective sample size
      log_sum_weights <- logsumexp(log_weights)
      log_weights_norm <- log_weights - log_sum_weights
      log_ess <- -logsumexp(2 * log_weights_norm)

      # weights_norm <- exp(log_weights_norm)
      # ESS <- 1 / sum(weights_norm ^ 2)
      # stopifnot(max(abs(log(ESS) - log_ess)) < 1e-6)

      # if the ESS is too low, halve the inverse_temperature increase and try again
      if (log_ess < log_ess_target) {
        inverse_temperature_increase <- inverse_temperature_increase / 2
      }

    }

    # record the selected inverse_temperature for this stage
    inverse_temperature <- inverse_temperature + inverse_temperature_increase

    # compute the log density of the target distribution at the current points
    # with the given inverse temperature for this stage, for use in resampling
    # and MCMC
    log_dens_target <- inverse_temperature * log_likelihood + prior_mvn_log_dens

    # use the normalised weights (for this stage's target distribution)
    # evaluated at the last set of particles to estimate the mean and covariance
    # of the new target distribution for this stage from the weighted samples.
    # We do this now, and not with the resampled points, to avoid degeneracy
    # issues.
    weights_norm <- exp(log_weights_norm)
    proposal_parameters <- cov.wt(particles, wt = weights_norm)
    particle_mean <- proposal_parameters$center
    particle_cov <- proposal_parameters$cov

    # add some jitter (ie. a ridge) to the diagonal of the estimated covariance
    # of the target distribution, for shrinkage to improve algorithm stability
    diag(particle_cov) <- diag(particle_cov) + 1e-6

    # set up MVN approximations of the target distribution for the two different
    # sampling options
    if (mh_type == "independent") {
      # if we are doing independent sampling, compute cholesky factors of this
      # for proposing new points
      particle_U <- chol(particle_cov)
      particle_L <- t(particle_U)
    } else if (mh_type == "standard") {
      # if we are doing standard sampling, shrink the marginal standard
      # deviations of the estimated covariance of the target distribution, and
      # rebuild the covariance matrix as a proposal distribution
      particle_cor <- cov2cor(particle_cov)
      particle_sd <- sqrt(diag(particle_cov))
      particle_sd_scaled <- particle_sd * mh_rel_epsilon
      particle_cov_scaled <- particle_cor *
        outer(sqrt(particle_sd_scaled),
              sqrt(particle_sd_scaled))
      particle_L_scaled <- t(chol(particle_cov_scaled))
    }

    # resample the particles according to the normalised weights, and copy over
    # the log densities of the target distribution at these particle locations
    index <- sample.int(n = n_particles,
                        size = n_particles,
                        prob = weights_norm,
                        replace = TRUE)
    particles <- particles[index, ]
    log_dens_target <- log_dens_target[index]


    # Resample with one or more steps of MCMC. For each particle, either run
    # standard Metropolis Hastings, jittering it (with a tuned and shrunk version
    # of the covariance matrix, to respect correlations and marginal variances)
    # and accept or reject based on the density ratio, or run independent
    # Metropolis Hastings, proposing new points from the MVN, independent of the
    # previous point location, and correct the acceptance criterion to ensure
    # detailed balance

    # update the tempering progress bar
    pb_smc$update(
      inverse_temperature
    )

    # if we are at the final temperature, stop the tempering progress bar and
    # create a new one for final sampling
    if (inverse_temperature == 1) {

      pb_smc$terminate()

      # a progress bar for final MCMC sampling
      pb_final <- progress::progress_bar$new(
        " final MCMC [:bar] :percent eta: :eta",
        clear = FALSE,
        show_after = 0
      )
      pb_final$update(
        0
      )
    }

    # run multiple steps to ensure low correlation in the particles
    mh_steps <- 0
    rejuvenating <- TRUE
    final_sampling <- FALSE
    previous_particle_correl <- 2

    # get the parameters from the old particles to efficicently compute the
    # particle cross-correlation with the new rejuvenated particles
    old_particle_corr_params <- particle_corr_params(t(particles))

    # keep taking MCMC steps for as long as we are either rejuvenating
    # (successfully reducing correlation from the resampled pre-MCMC particles
    # and below the maximum steps per stage) or doing the final MCMC sampling on
    # particles
    while(rejuvenating || final_sampling) {

      mh_steps <- mh_steps + 1

      if (mh_type == "independent") {

        # regenerate the particles with an independence MH sampler - proposals
        # from the MVN approximation to the distribution, independent of
        # previous point location. This proposal does not satisfy detailed
        # balance, so we need to adjust the accept reject criterion by computing
        # the forward and reverse steps
        proposed_particles <- sim_mvn(n_particles,
                                      mu = particle_mean,
                                      L = particle_L)


        # compute the log probability of moving to each new point
        forward_proposal_log_prob <- log_prob_mvn(proposed_particles,
                                                  mean = particle_mean,
                                                  U = particle_U)

        # compute the log probability of moving to each old point
        reverse_proposal_log_prob <- log_prob_mvn(particles,
                                                  mean = particle_mean,
                                                  U = particle_U)

        # the difference between these gives the bias due to the proposal. By
        # adding this to the MH criterion, we can eliminate that bias, and
        # ensure thatthe only bias in the kernel is due to the target
        # ditribution

        # evaluate the target density at the proposed particles
        prior_mvn_log_dens_proposed <- prior_log_prob_mvn(proposed_particles)
        posterior_log_dens_proposed <- posterior_log_prob(proposed_particles)
        log_likelihood_proposed <- posterior_log_dens_proposed - prior_mvn_log_dens_proposed
        log_dens_target_proposed <- inverse_temperature * log_likelihood_proposed +
          prior_mvn_log_dens_proposed

        # put together the log MH criterion
        log_mh_criterion <- (log_dens_target_proposed + reverse_proposal_log_prob) -
          (log_dens_target + forward_proposal_log_prob)

      } else if (mh_type == "standard") {

        # For the standard MH version, simulate jitter from the shrunk
        # covariance and add it to the particles to rejuvenate them
        jitter <- sim_mvn(n_particles, mu = 0, L = particle_L_scaled)
        proposed_particles <- particles + jitter

        # compute the target density of the proposed particles
        prior_mvn_log_dens_proposed <- prior_log_prob_mvn(proposed_particles)
        posterior_log_dens_proposed <- posterior_log_prob(proposed_particles)
        log_likelihood_proposed <- posterior_log_dens_proposed - prior_mvn_log_dens_proposed
        log_dens_target_proposed <- inverse_temperature * log_likelihood_proposed +
          prior_mvn_log_dens_proposed

        # compute the standard accept/reject criteria - the proposal is
        # symmetric so cancels out here and does not need to be computed
        log_mh_criterion <- log_dens_target_proposed - log_dens_target

      }

      # now decide wheter to accept or ecte the proposed particles
      u <- runif(n_particles)
      accept <- log(u) < log_mh_criterion

      # update the accepted particles
      particles[accept, ] <- proposed_particles[accept, ]

      # Copy over the log densities of prior, likelihood, and the target
      # distribution at these particles to avoid recomputing them. The accepted
      # ones get the versions for the proposed particles, the rejected ones get
      # the versions for the previous particles. We only use log_dens_target in
      # this MCMC loop, but update the other two so we can re-use them in the
      # next tempering stage
      prior_mvn_log_dens[accept] <- prior_mvn_log_dens_proposed[accept]
      log_likelihood[accept] <- log_likelihood_proposed[accept]
      log_dens_target[accept] <- log_dens_target_proposed[accept]

      # get the cross-correlation between these new particles and the pre-MH
      # resampled particles, to assess how much we have rejuvenated the
      # particles
      current_particle_correl <- particle_crosscor(
        y = t(particles),
        x_params = old_particle_corr_params
      )

      # see if this cross-correlation with the old ones (amount we have innovated)
      # is still reducing, by comparing against the cross-correlations between the
      # particles at the last step and the pre-MH resampled particles
      correl_change <- previous_particle_correl - current_particle_correl
      frac_reducing_correl <- mean(correl_change > mh_correl_threshold)
      reducing_correl <- frac_reducing_correl > 0.9

      # update the last version of the correlations
      previous_particle_correl <- current_particle_correl

      # determine whether we are still doing rejuvenation MCMC, or we are in the
      # final sampling phase
      rejuvenating <- reducing_correl & mh_steps < max_mh_steps
      final_sampling <- inverse_temperature == 1 & mh_steps < final_mh_steps

      if (inverse_temperature == 1) {
        # if we are in the final stages, us the progress bar to let the user
        # track that
        pb_final$update(
          mh_steps / final_mh_steps
        )
      }

    }

    # update the stage counter and continue
    stage <- stage + 1

  }

  if (inverse_temperature != 1) {
    warning("SMC reached 'max_stages' iterations and terminated before ",
            "converging on the posterior density")
  } else{

    # close the last progress bar if we got there
    pb_final$terminate()
  }

  # return the particles and convergence information.
  list(
    particles = particles,
    convergence = inverse_temperature == 1,
    inverse_temperature = inverse_temperature,
    tempering_stages = stage
  )

}
