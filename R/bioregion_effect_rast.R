#' Combined effect of the bioregion terms on each species' relative abundance
#'
#' The covariate x bioregion interactions only act together: there are no
#' bioregion intercepts, and land cover sums to ~1, so one term on its own is
#' weakly identified but their sum in a cell is not. This maps that sum, as a
#' multiplier on relative abundance: exp(X_int %*% gamma), with X_int the
#' cell's interaction columns, built as in predict_lambda_reparam(), and gamma
#' the interaction coefficients' posterior mean. Since the sum is linear in
#' gamma, this is exp of the posterior mean of its contribution to log
#' relative abundance.
#'
#' 1 means the bioregion terms leave the species' abundance where the main
#' effects put it; 2 means they double it.
#'
#' @param coef_draws_file path of the .rds from extract_cv_draws()
#' @param covariates the prediction layer: SpatRaster with the model's
#'   covariates and bioregions
#' @return SpatRaster, one layer per species
#' @author geryan
#' @export
bioregion_effect_rast <- function(
    coef_draws_file,
    covariates
){

  coef_draws <- readRDS(coef_draws_file)

  covs <- coef_draws$target_covariate_names
  bios <- coef_draws$bioregion_names
  spp <- coef_draws$target_species

  layer_values <- terra::values(covariates)
  naidx <- is.na(layer_values[, 1])

  x_interactions <- make_designmat_interactions(
    layer_values[!naidx, covs],
    layer_values[!naidx, bios]
  )

  interaction_idx <- length(covs) + seq_len(length(covs) * length(bios))

  gamma_mean <- colMeans(
    coef_draws$beta[, interaction_idx, , drop = FALSE]
  )

  effect <- matrix(
    NA_real_,
    nrow = length(naidx),
    ncol = length(spp)
  )

  effect[!naidx, ] <- exp(x_interactions %*% gamma_mean)

  r <- terra::rast(
    covariates[[1]],
    nlyrs = length(spp),
    names = spp
  ) |>
    terra::setValues(
      effect
    )

  r

}
