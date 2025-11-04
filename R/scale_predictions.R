#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pred_dist
#' @return
#' @author geryan
#' @export
scale_predictions <- function(
    lambda_file,
    n_sample = 1000
  ) {

  lambda <- rast(lambda_file$count)

  log_lambda <- log(lambda)

  sample_log_lambda <- terra::spatSample(
    log_lambda,
    n_sample,
    na.rm = TRUE
  )

  inv_cloglog <- function(x) {
    1 - exp(-exp(x))
  }

  f <- function(log_scale, sample_log_lambda) {
    -var(inv_cloglog(sample_log_lambda + log_scale))
  }

  optx <- function(x, interval){
    o <- optimise(
      f,
      interval = c(-100, 100),
      sample_log_lambda = x
    )

    round(o$minimum, 2)
  }

  log_scale <- apply(
    X = sample_log_lambda,
    FUN = optx,
    MARGIN = 2,
    interval = c(-100, 100)
  )


  scaled_rast <- inv_cloglog(log_lambda + log_scale)

  fn <- sub(
    pattern = "\\.tif",
    replacement = "_scaled.tif",
    x = lambda_file$pa
  )

  writeRaster(
    scaled_rast,
    filename = fn,
    overwrite = TRUE
  )

  rast(fn)

}
