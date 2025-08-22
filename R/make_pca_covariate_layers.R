#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param covariate_rast
#' @param target_covariate_names
#' @return
#' @author geryan
#' @export
make_pca_covariate_layers <- function(
    covariate_rast,
    target_covariate_names,
    model_data_spatial
  ) {

  cov_pca <- covariate_rast |>
    terra::subset(target_covariate_names) |>
    terra::extract(
      model_data_spatial |>
        select(longitude, latitude) |>
        distinct(),
      ID = FALSE
    ) |>
    prcomp(
      center = TRUE,
      scale = TRUE
    )

  sry <- summary(cov_pca)

  # keep pcs explaining at least 90% of the variance
  n_pcs_keep <- min(which(sry$importance["Cumulative Proportion", ] > 0.9))

  # make rasters of them
  pcs_cov <- predict(
    covariate_rast,
    cov_pca
  )

  pcs_cov_keep <- pcs_cov[[seq_len(n_pcs_keep)]]
  names(pcs_cov_keep) <- paste0("covariate_pc_", tolower(names(pcs_cov_keep)))

  pcs_cov_keep

}
