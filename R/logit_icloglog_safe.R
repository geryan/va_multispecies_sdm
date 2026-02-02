# THIS IS THE OLD VERSION
# convert log lambda into a logit probability, to evaluate the pa likelihood in
# a more numerically stable way (see ilogit_stable.R for definition and
# explanation)
# logit_icloglog <- function(eta) {
#   exp_eta <- exp(eta)
#   log1p(-exp(-exp_eta)) + exp_eta
# }

# convert log lambda into a logit probability, to evaluate the pa likelihood in
# a more numerically stable way (see ilogit_stable.R for definition and
# explanation)

# don't do this, this results in invalid samples for log_lambda_obs_pa >= 3.7
# # pa_data_response_expected <- icloglog(log_lambda_obs_pa)

# do this instead
# # logit_prob_pa <- logit_icloglog(log_lambda_obs_pa)
# # pa_data_response_expected <- ilogit(logit_prob_pa)
# # distribution(pa_data_response) <- bernoulli(pa_data_response_expected)

######################
# SAFE VERSION
######################

# given a vector of values x, clamp them to within a specified range. This is
# equivalent to: pmax(pmin(x, clamp_max), clamp_min), but will work with greta
# arrays
  # x <- seq(-50, 50, length.out = 1000)
  # y <- clamp(x,
  #            min = qlogis(.Machine$double.eps),
  #            max = qlogis(1 - .Machine$double.eps))
  # plot(y ~ x,
  #      type = "l")
clamp <- function(x, min, max) {
  invalid_min <- x < min
  invalid_max <- x > max
  valid <- (1 - invalid_min) * (1 - invalid_max)
  min * invalid_min +
    max * invalid_max +
    x * valid
}

# convert log lambda into a logit probability, to evaluate the pa likelihood in
# a more numerically stable way, with clamping of values to prevent numerical
# under/over-flow (see ilogit_stable.R for definition and explanation)
logit_icloglog_safe <- function(
    eta,
    clamp_min = log(.Machine$double.eps),
    clamp_max = log(.Machine$double.xmax)
  ) {
  eta_clamped <- clamp(eta,
                       min = clamp_min,
                       max = clamp_max)
  exp_eta <- exp(eta_clamped)
  log1p(-exp(-exp_eta)) + exp_eta
}

logit_icloglog <- logit_icloglog_safe

# to find clamp values, need to find out the values of eta (log abundance) that
# over/underflow in TFP
#
# library(greta)
# eta_vals <- c(
#   seq(-38, -37, length.out = 100),
#   seq(709.5, 710, length.out = 100)
# )
#
# # eta_vals <- c(
# #   seq(709.7778, 709.78283, length.out = 100)
# # )
# #
# eta <- greta::as_data(eta_vals)
#
# logits <- logit_icloglog(eta)
# vals_logits <- calculate(logits)[[1]]
#
# # calculate TFP log probs for success and failure
# total_count <- 1
#
# tfp <- greta:::tfp
# prob <- tfp$distributions$Binomial(total_count = total_count, logits = vals_logits)
#
# vals_success_tfp <- as.numeric(prob$log_prob(1))
# vals_failure_tfp <- as.numeric(prob$log_prob(0))
#
# cbind(eta = eta_vals,
#       logits = vals_logits,
#       success_tfp = vals_success_tfp,
#       failure_tfp = vals_failure_tfp)
#
# # this underflows at eta < -37.4 (a bit lower than log(.Machine$double.eps))
# # and overflows at eta > 709.75 (where logits also overflows, at log(.Machine$double.xmax))
