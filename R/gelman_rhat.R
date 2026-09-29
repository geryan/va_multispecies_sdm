#' Per-parameter Gelman-Rubin R-hat without the per-chain covariance matrices
#'
#' A drop-in replacement for `coda::gelman.diag(x, multivariate = FALSE)` that
#' returns the same per-parameter potential scale reduction factors (point
#' estimate and upper confidence limit) without coda's memory cost.
#'
#' coda builds the full parameter x parameter covariance matrix of every chain,
#' `array(sapply(x, var), c(Nvar, Nvar, Nchain))`, even when `multivariate =
#' FALSE`, and the per-parameter statistic then uses only its diagonal. At the
#' ~3,900 parameters and 40 chains of the reparameterised model that array and
#' its copies need about 21 GB. The per-parameter statistic depends only on each
#' parameter's own per-chain means and variances, so this computes those,
#' one chain at a time, and then follows coda's formulas line for line on the
#' diagonals. Memory is Nvar x Nchain numbers plus one chain's draws.
#'
#' Each step uses the same primitives as coda (`mean`, `var`, `cov` on the same
#' numbers in the same order), so the results match coda's exactly; see
#' extras/check_gelman_rhat.R. Not provided: `transform`, and the multivariate
#' `mpsrf`, which genuinely needs the full matrices and is returned as NULL.
#'
#' @param x an mcmc.list (including greta's draws), or anything
#'   `coda::as.mcmc.list()` accepts
#' @param confidence coverage of the upper confidence limit
#' @param autoburnin as in coda: if TRUE, only the second half of each chain is
#'   used
#' @return an object of class "gelman.diag", as coda returns: a list with
#'   `psrf` (one row per parameter, columns "Point est." and "Upper C.I.") and
#'   `mpsrf = NULL`
#' @author geryan
#' @export
gelman_rhat <- function(
    x,
    confidence = 0.95,
    autoburnin = TRUE
){

  x <- coda::as.mcmc.list(x)

  if (coda::nchain(x) < 2) {
    stop("You need at least two chains")
  }

  if (autoburnin && stats::start(x) < stats::end(x) / 2) {
    x <- stats::window(
      x,
      start = stats::end(x) / 2 + 1
    )
  }

  Niter <- coda::niter(x)
  Nchain <- coda::nchain(x)
  Nvar <- coda::nvar(x)
  xnames <- coda::varnames(x)

  # per-chain means and variances of each parameter: the diagonals of coda's
  # xbar and S2, one chain's draws in memory at a time
  xbar <- matrix(
    NA_real_,
    nrow = Nvar,
    ncol = Nchain
  )

  s2 <- xbar

  for (ch in seq_len(Nchain)) {
    m <- as.matrix(x[[ch]])
    xbar[, ch] <- apply(m, 2, mean)
    s2[, ch] <- apply(m, 2, var)
  }

  rm(m)

  # from here, coda's formulas restricted to the diagonal:
  #   diag(W)  = mean over chains of s2       diag(B) = Niter * var over chains of xbar
  #   diag(var(t(s2), t(xbar^2))) = cov(s2[i, ], xbar[i, ]^2), and likewise for xbar
  w <- apply(s2, 1, mean)
  b <- Niter * apply(xbar, 1, var)

  muhat <- apply(xbar, 1, mean)
  var.w <- apply(s2, 1, var) / Nchain
  var.b <- (2 * b^2) / (Nchain - 1)

  xbar2 <- xbar^2

  cov_s2_xbar2 <- vapply(
    seq_len(Nvar),
    function(i) stats::cov(s2[i, ], xbar2[i, ]),
    numeric(1)
  )

  cov_s2_xbar <- vapply(
    seq_len(Nvar),
    function(i) stats::cov(s2[i, ], xbar[i, ]),
    numeric(1)
  )

  cov.wb <- (Niter / Nchain) * (cov_s2_xbar2 - 2 * muhat * cov_s2_xbar)

  V <- (Niter - 1) * w / Niter + (1 + 1 / Nchain) * b / Niter
  var.V <- ((Niter - 1)^2 * var.w + (1 + 1 / Nchain)^2 * var.b +
              2 * (Niter - 1) * (1 + 1 / Nchain) * cov.wb) / Niter^2
  df.V <- (2 * V^2) / var.V
  df.adj <- (df.V + 3) / (df.V + 1)
  B.df <- Nchain - 1
  W.df <- (2 * w^2) / var.w
  R2.fixed <- (Niter - 1) / Niter
  R2.random <- (1 + 1 / Nchain) * (1 / Niter) * (b / w)
  R2.estimate <- R2.fixed + R2.random
  R2.upper <- R2.fixed +
    stats::qf((1 + confidence) / 2, B.df, W.df) * R2.random

  psrf <- cbind(
    sqrt(df.adj * R2.estimate),
    sqrt(df.adj * R2.upper)
  )

  dimnames(psrf) <- list(
    xnames,
    c("Point est.", "Upper C.I.")
  )

  out <- list(
    psrf = psrf,
    mpsrf = NULL
  )

  class(out) <- "gelman.diag"

  out

}
