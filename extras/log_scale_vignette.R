library(terra)

inv_cloglog <- function(x) {
  1 - exp(-exp(x))
}

f <- system.file("ex/elev.tif", package="terra")
log_lambda <- 0.5 * scale(rast(f)) + 1.5
set.seed(1)

lambda <- exp(log_lambda)
pts <- terra::spatSample(lambda, 50, method = "weights", as.points = TRUE)

par(mfrow = c(1, 3))
plot(lambda,
     main = "lambda")
points(pts,
       pch = 21,
       bg = "white")

plot(inv_cloglog(log_lambda),
     main = "icloglog(log_lambda)",
     range = c(0, 1))
points(pts,
       pch = 21,
       bg = "white")

# find the value of log_scale that maximises the variance
n_sample <- 1000
sample_log_lambda <- terra::spatSample(log_lambda,
                                       n_sample,
                                       na.rm = TRUE)[, 1]
f <- function(log_scale) {
  -var(inv_cloglog(sample_log_lambda + log_scale))
}
o <- optimise(f, c(-100, 100))
log_scale <- round(o$minimum, 2)

plot(inv_cloglog(log_lambda + log_scale),
     main = sprintf("icloglog(log_lambda + %s)",
                    log_scale),
     range = c(0, 1))
points(pts,
       pch = 21,
       bg = "white")

