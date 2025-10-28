logit_icloglog <- function(eta) {
  exp_eta <- exp(eta)
  log1p(-exp(-exp_eta)) + exp_eta
}


# convert log lambda into a logit probability, to evaluate the pa likelihood in
# a more numerically stable way (see ilogit_stable.R for definition and
# explanation)

# don't do this, this results in invalid samples for log_lambda_obs_pa >= 3.7
# # pa_data_response_expected <- icloglog(log_lambda_obs_pa)

# do this instead
# # logit_prob_pa <- logit_icloglog(log_lambda_obs_pa)
# # pa_data_response_expected <- ilogit(logit_prob_pa)
# # distribution(pa_data_response) <- bernoulli(pa_data_response_expected)
