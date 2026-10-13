#' Posterior draws of each species' slope on each covariate, by bioregion
#'
#' The model's log relative abundance for species s is
#'
#'   alpha_s + sum_a x_a * (beta_as + sum_b z_b * gamma_abs)
#'
#' where beta_as is the main effect of covariate a, gamma_abs its interaction
#' with bioregion b, and z_b the cell's (smoothed, fractional) membership of
#' bioregion b. So each covariate has a slope -- the change in log relative
#' abundance from none of the covariate to all of it -- that varies across
#' Africa with the bioregion mix. This returns that slope three ways:
#'
#' - `bioregion`: in each bioregion, beta_as + gamma_abs, i.e. at z_b = 1
#' - `average`: averaged over the modelled area, beta_as + sum_b w_b gamma_abs,
#'   with w_b the share of the modelled area in bioregion b
#'   (bioregion_area_weights())
#' - `main`: the main effect alone, beta_as, which is the slope only where no
#'   modelled bioregion applies
#'
#' The interaction columns of the design are ordered covariate-major
#' (make_designmat_interactions()): column A + (a - 1) * B + b is covariate a
#' times bioregion b.
#'
#' @param coef_draws list from extract_cv_draws(): `beta` [D, J, S],
#'   `target_covariate_names`, `bioregion_names`, `target_species`
#' @param weights named bioregion area weights summing to 1, from
#'   bioregion_area_weights()
#' @return list of arrays `main` [D, A, S], `bioregion` [D, A, B, S] and
#'   `average` [D, A, S], with dimnames
#' @author geryan
#' @export
covariate_slopes <- function(
    coef_draws,
    weights
){

  beta <- coef_draws$beta
  covs <- coef_draws$target_covariate_names
  bios <- coef_draws$bioregion_names
  spp <- coef_draws$target_species

  n_d <- dim(beta)[1]
  n_a <- length(covs)
  n_b <- length(bios)
  n_s <- length(spp)

  if (dim(beta)[2] != n_a + n_a * n_b || dim(beta)[3] != n_s) {
    stop(
      sprintf(
        "covariate_slopes(): beta is [%s], expected [D, %i, %i]",
        paste(dim(beta), collapse = ", "),
        n_a + n_a * n_b,
        n_s
      ),
      call. = FALSE
    )
  }

  if (!setequal(names(weights), bios)) {
    stop(
      "covariate_slopes(): `weights` must be named for the model's bioregions",
      call. = FALSE
    )
  }

  weights <- weights[bios]

  main <- beta[, seq_len(n_a), , drop = FALSE]

  # [D, B, A, S] as stored (b varies fastest within a), then put A before B
  gamma <- array(
    beta[, n_a + seq_len(n_a * n_b), , drop = FALSE],
    dim = c(n_d, n_b, n_a, n_s)
  ) |>
    aperm(
      c(1, 3, 2, 4)
    )

  bioregion <- gamma +
    array(
      main,
      dim = c(n_d, n_a, n_s, n_b)
    ) |>
      aperm(
        c(1, 2, 4, 3)
      )

  average <- main +
    apply(
      gamma,
      c(1, 2, 4),
      function(g) sum(g * weights)
    )

  dimnames(main) <- list(
    NULL,
    covs,
    spp
  )

  dimnames(bioregion) <- list(
    NULL,
    covs,
    bios,
    spp
  )

  dimnames(average) <- dimnames(main)

  list(
    main = main,
    bioregion = bioregion,
    average = average
  )

}

#' Share of the modelled area in each bioregion
#'
#' The bioregion layers are smoothed fractions, so a cell can belong partly to
#' several. Each bioregion's weight is its summed fraction over the grid,
#' divided by the summed fraction of all of them -- its share of the area the
#' model has bioregions for.
#'
#' @param covariates SpatRaster holding the bioregion layers
#' @param bioregion_names the model's bioregions
#' @return named numeric vector summing to 1
#' @author geryan
#' @export
bioregion_area_weights <- function(
    covariates,
    bioregion_names
){

  totals <- terra::global(
    covariates[[bioregion_names]],
    fun = "sum",
    na.rm = TRUE
  )$sum

  stats::setNames(
    totals / sum(totals),
    bioregion_names
  )

}

#' Summarise draws over the first dimension
#'
#' @param x array with draws in its first dimension
#' @param probs the lower and upper quantiles of the interval
#' @return array of the median, lower and upper quantile, in that order, in
#'   the first dimension
#' @author geryan
#' @export
summarise_draws_array <- function(
    x,
    probs = c(0.05, 0.95)
){

  q <- apply(
    x,
    seq_along(dim(x))[-1],
    stats::quantile,
    probs = c(0.5, probs),
    names = FALSE
  )

  dimnames(q)[[1]] <- c(
    "median",
    "lower",
    "upper"
  )

  q

}
