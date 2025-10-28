# numerically stable evaluation of the icloglog likelihood

# the inverse complementary loglog link function is very numerically unstable
# due to the double exponentiation. In R and tensorflow (with double precision)
# on my macbook pro, the function overflows to 1 for values of x of 3.7 or
# greater:

x <- 3.7
identical(
  calculate(icloglog(x))[[1]][1, 1],
  1
)
identical(
  1 - exp(-exp(x)),
  1
)

# This is very painful in a modelling situation, since the linear predictor
# could easily exceed this, and a value of 1 is invalid - leading to rejected
# samples and terrible model convergence.

# When passing a probability into a Bernoulli likelihood in greta, we can make
# use of a numerically stable log-likelihood calculation, by passing in the
# logit-probability instead of the probability directly. Therefore, if we can
# convert the linear predictor from our cloglog model to the equivalent logit
# probability, without computing the probability itself, we should be able to
# avoid this numerical overflow and significantly improve sampling.

# we want to model:
#   p = icloglog(eta)
#     = 1 - exp(-exp(eta))
# and need to calculate:
#   z = logit(p)
#     = log(p / (1 - p))
# without ever computing p (since that will result in overflow)

# ie. we want a function:
#   z = logit_icloglog(eta)
# that is a numerically stable equivalent of the above

# We can express some things in terms of logs and log1p (a commonly applied
# numerically stable evaluation of log(1 + x)):
#   z = log(p / (1 - p))
#     = log(p) - log1p(-p)
# and
#   log(p) = log(1 - exp(-exp(eta)))
#          = log1p(-exp(-exp(eta)))
# and
#   log1p(-p) = log(1 + -p)
#    = log(1 + -1 + exp(-exp(eta)))
#    = log(exp(-exp(eta))
#    = -exp(eta)
# which gives us:
#    z = logit_icloglog(eta)
#      = log1p(-exp(-exp(eta))) + exp(eta)

logit_icloglog <- function(eta) {
  exp_eta <- exp(eta)
  log1p(-exp(-exp_eta)) + exp_eta
}


# we can check this numerically, for the range of values supported by the
# unstable version:

eta <- seq(2, 4, length.out = 1000)
z <- logit_icloglog(eta)
plot(z ~ eta, type = "l")


logit_icloglog_bad <- function(eta) {
  p <- 1 - exp(-exp(eta))
  log(p / (1 - p))
}
z_bad <- logit_icloglog_bad(eta)

lines(z_bad ~ eta,
      lty = 3,
      lwd = 3,
      col = "red")


# this new version is numerically stable up to much greater values of eta
logit_icloglog(709)


