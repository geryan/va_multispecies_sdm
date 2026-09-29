# Does gelman_rhat() (R/gelman_rhat.R) give the same per-parameter R-hat as
# coda::gelman.diag(multivariate = FALSE)? And how much less memory does it need?
#
# Run from the project root:
#   Rscript extras/check_gelman_rhat.R
#
# A. synthetic draws covering the cases that stress the formula; about a minute
# B. peak memory and time of both at 1,000 parameters x 40 chains
# C. the real fit's draws. Loads the 6.5 GB fit image, so it only runs when
#    RUN_REAL_DRAWS is TRUE -- do not run it alongside a fit
#
# coda's per-parameter statistic uses only that parameter's own chain means and
# variances, so comparing on blocks of parameters (C) is the same as comparing
# on all of them, which coda cannot do within 16 GB. C checks that directly.

suppressPackageStartupMessages(
  library(coda)
)

source("R/gelman_rhat.R")

RUN_REAL_DRAWS <- FALSE
fit_image <- "outputs/images/model_fit_test_source_re_rep.RData"

set.seed(20260930)


############
# helpers
############

make_draws <- function(
    gen,
    nchain = 40,
    niter = 1000,
    nvar = 200,
    start = 1
){
  mcmc.list(
    lapply(
      seq_len(nchain),
      function(ch){
        mcmc(
          gen(niter, nvar),
          start = start
        )
      }
    )
  )
}

take <- function(
    x,
    idx
){
  mcmc.list(
    lapply(
      x,
      function(ch){
        mcmc(
          as.matrix(ch)[, idx, drop = FALSE],
          start = start(ch),
          thin = thin(ch)
        )
      }
    )
  )
}

# warnings are suppressed on both sides alike: the constant-parameter case
# makes qf() return NaN, which coda and gelman_rhat() both warn about
compare <- function(
    x,
    label,
    ...
){

  a <- suppressWarnings(
    gelman.diag(
      x,
      multivariate = FALSE,
      ...
    )$psrf
  )

  b <- suppressWarnings(
    gelman_rhat(
      x,
      ...
    )$psrf
  )

  fin <- is.finite(a) & is.finite(b)

  data.frame(
    scenario = label,
    n_par = nrow(a),
    identical = identical(a, b),
    same_names = identical(dimnames(a), dimnames(b)),
    same_nan_inf = identical(is.nan(a), is.nan(b)) &&
      identical(is.infinite(a), is.infinite(b)),
    max_abs_diff = if (any(fin)) max(abs(a[fin] - b[fin])) else 0,
    max_rel_diff = if (any(fin)) max(abs(a[fin] - b[fin]) / abs(a[fin])) else 0,
    max_point_est = if (any(fin)) max(a[, 1][is.finite(a[, 1])]) else NA
  )

}

peak_gb <- function(expr){
  base_mb <- sum(gc(reset = TRUE)[, 2])
  t0 <- Sys.time()
  force(expr)
  c(
    extra_GB = (sum(gc()[, 7]) - base_mb) / 1024,
    secs = as.numeric(Sys.time() - t0, units = "secs")
  )
}


############
# A. synthetic draws
############

# parameters on very different scales, to test precision across magnitudes
scales <- function(nvar) 10^((seq_len(nvar) %% 7) - 3)

gen <- list(

  converged_normal = function(n, p){
    matrix(rnorm(n * p), n, p)
  },

  # each chain centred somewhere different: R-hat well above 1
  not_converged_offsets = function(n, p){
    matrix(rnorm(n * p), n, p) + rep(rnorm(p, sd = 0.5), each = n)
  },

  # each chain with its own spread
  unequal_chain_scales = function(n, p){
    matrix(rnorm(n * p), n, p) * rep(exp(rnorm(p, sd = 0.5)), each = n)
  },

  mixed_magnitudes = function(n, p){
    matrix(rnorm(n * p), n, p) * rep(scales(p), each = n) +
      rep(scales(p) * 50, each = n)
  },

  heavy_tailed_t3 = function(n, p){
    matrix(rt(n * p, df = 3), n, p)
  },

  skewed_lognormal = function(n, p){
    exp(matrix(rnorm(n * p, sd = 1.5), n, p))
  },

  autocorrelated_ar1 = function(n, p){
    apply(
      matrix(rnorm(n * p), n, p),
      2,
      function(e) as.numeric(stats::filter(e, 0.95, method = "recursive"))
    )
  },

  # columns 1-5 constant and equal across chains (0/0), 6-10 constant but
  # different per chain (b/0), the rest ordinary
  constant_parameters = function(n, p){
    m <- matrix(rnorm(n * p), n, p)
    m[, 1:5] <- 1
    m[, 6:10] <- rep(rnorm(5), each = n)
    m
  }

)

res_a <- do.call(
  rbind,
  c(
    lapply(
      names(gen),
      function(g){
        compare(
          make_draws(gen[[g]]),
          g,
          autoburnin = FALSE
        )
      }
    ),
    list(
      compare(
        make_draws(gen$not_converged_offsets, nchain = 2),
        "two_chains",
        autoburnin = FALSE
      ),
      compare(
        make_draws(gen$not_converged_offsets),
        "autoburnin_TRUE",
        autoburnin = TRUE
      ),
      compare(
        make_draws(gen$not_converged_offsets, start = 2001),
        "autoburnin_TRUE_start_2001",
        autoburnin = TRUE
      ),
      compare(
        make_draws(gen$not_converged_offsets),
        "confidence_0.90",
        autoburnin = FALSE,
        confidence = 0.90
      )
    )
  )
)

cat("\n== A. synthetic draws: coda::gelman.diag() vs gelman_rhat() ==\n\n")
print(res_a, digits = 3, row.names = FALSE)


############
# B. memory and time at 1,000 parameters x 40 chains x 1,000 iterations
############

big <- make_draws(gen$converged_normal, nvar = 1000)

res_b <- rbind(
  coda = peak_gb(
    gelman.diag(
      big,
      autoburnin = FALSE,
      multivariate = FALSE
    )
  ),
  gelman_rhat = peak_gb(
    gelman_rhat(
      big,
      autoburnin = FALSE
    )
  )
)

cat("\n== B. peak extra memory and time, 1,000 parameters x 40 chains ==\n\n")
print(round(res_b, 2))
cat(
  "\ncoda's extra memory grows with the square of the number of parameters;",
  "gelman_rhat()'s with the number of parameters.\n"
)

rm(big)


############
# C. the real fit's draws
############

res_c <- NULL

if (RUN_REAL_DRAWS && file.exists(fit_image)) {

  fit <- new.env()
  load(fit_image, envir = fit)
  draws <- fit$draws
  rm(fit)
  invisible(gc())

  vn <- varnames(draws)

  groups <- list(
    alpha = grep("^alpha", vn),
    gamma_delta = grep("^(gamma|delta)", vn),
    sampling = grep("^sampling", vn),
    zeta_raw = grep("^zeta_raw", vn),
    beta_raw_random_300 = sort(sample(grep("^beta_raw", vn), 300))
  )

  # blocks of at most 300 parameters, which coda can handle
  blocks <- unlist(
    lapply(
      names(groups),
      function(g){
        idx <- groups[[g]]
        chunks <- split(idx, ceiling(seq_along(idx) / 300))
        stats::setNames(chunks, paste0(g, "_", seq_along(chunks)))
      }
    ),
    recursive = FALSE
  )

  res_c <- do.call(
    rbind,
    lapply(
      names(blocks),
      function(b){
        compare(
          take(draws, blocks[[b]]),
          b,
          autoburnin = FALSE
        )
      }
    )
  )

  cat("\n== C. real draws, in blocks: coda::gelman.diag() vs gelman_rhat() ==\n\n")
  print(res_c, digits = 3, row.names = FALSE)

  # a parameter's coda R-hat does not depend on which others it is computed with
  a1 <- gelman.diag(
    take(draws, groups$alpha),
    autoburnin = FALSE,
    multivariate = FALSE
  )$psrf[1, ]

  a2 <- gelman.diag(
    take(draws, c(groups$alpha[1], groups$beta_raw_random_300)),
    autoburnin = FALSE,
    multivariate = FALSE
  )$psrf[1, ]

  cat(
    "\ncoda R-hat of", vn[groups$alpha[1]],
    "the same with the other alphas and with 300 betas:", identical(a1, a2), "\n"
  )

  # and the full set, which coda cannot do in 16 GB
  full <- NULL

  res_full <- peak_gb(
    full <- gelman_rhat(
      draws,
      autoburnin = FALSE
    )
  )

  cat(
    sprintf(
      "\ngelman_rhat() on all %d parameters: %.1f s, %.2f GB extra; max R-hat %.3f, %.1f%% above 1.1\n",
      nrow(full$psrf),
      res_full[["secs"]],
      res_full[["extra_GB"]],
      max(full$psrf[, 1], na.rm = TRUE),
      100 * mean(full$psrf[, 1] > 1.1, na.rm = TRUE)
    )
  )

} else {

  cat("\n== C. skipped: set RUN_REAL_DRAWS <- TRUE to check against the real fit ==\n")

}


############
# verdict
############

all_res <- rbind(res_a, res_c)

verdict <- if (all(all_res$identical)) {
  "IDENTICAL: every R-hat is bit-for-bit the same as coda's"
} else if (all(all_res$same_names & all_res$same_nan_inf) &&
           max(all_res$max_rel_diff) < 1e-12) {
  sprintf(
    "PRACTICALLY IDENTICAL: largest relative difference %.2g",
    max(all_res$max_rel_diff)
  )
} else {
  "DIFFERENT: see the tables above"
}

cat("\n", verdict, "\n", sep = "")
