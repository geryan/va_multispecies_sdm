# Check the adaptive-quadrature Poisson-lognormal against integrate()
src <- readLines(commandArgs(trailingOnly = TRUE)[1])
start <- grep("^# Physicists' Gauss-Hermite", src)
end <- grep("^p0_nb2 <- ", src)
eval(parse(text = src[start:end]))

ref <- function(y, mu, s){
  a <- log(mu) - s^2 / 2
  lg <- function(e) dpois(y, exp(a + e), log = TRUE) + dnorm(e, 0, s, log = TRUE)
  # mode and curvature found independently of ll_pln(), by optimize()
  md <- optimize(lg, c(-40, 40), maximum = TRUE, tol = 1e-12)$maximum
  sd <- 1 / sqrt(exp(a + md) + 1 / s^2)
  gmax <- lg(md)
  f <- function(e) exp(lg(e) - gmax)
  gmax + log(integrate(f, md - 40 * sd, md + 40 * sd, subdivisions = 10000, rel.tol = 1e-12, abs.tol = 0)$value)
}
grid <- expand.grid(
  y = c(0, 1, 3, 17, 200, 1000, 342645),
  mu = c(0.001, 0.05, 1, 20),
  s = c(0.5, 1.5, 3)
)
grid$agh <- mapply(function(y, mu, s) ll_pln(y, mu, s), grid$y, grid$mu, grid$s)
grid$ref <- mapply(ref, grid$y, grid$mu, grid$s)
grid$diff <- grid$agh - grid$ref
cat("max |log pmf difference| (AGH vs integrate):", signif(max(abs(grid$diff)), 3), "\n")
print(grid[order(-abs(grid$diff)), ][1:6, ], digits = 6)
# pmf sums to 1 and has mean mu
for (mu in c(0.05, 3)) for (s in c(1, 2.5)) {
  yy <- 0:20000
  p <- exp(ll_pln(yy, rep(mu, length(yy)), s))
  cat(sprintf("mu %.2f s %.1f: sum %.6f  mean %.4f\n", mu, s, sum(p), sum(yy * p)))
}
