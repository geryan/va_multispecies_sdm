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
    lambda_file = NULL,
    lambda_rast = NULL,
    n_sample = 1000
  ) {

  if(!is.null(lambda_file) & !is.null(lambda_rast)){
    stop("provide either lambda file or raster only, not both")
  }

  if(is.null(lambda_file) & is.null(lambda_rast)){
    stop("must provide either lambda file or raster")
  }

  if(!is.null(lambda_file)){

    lambda <- rast(lambda_file)

  } else if (!is.null(lambda_rast)){

    lambda <- lambda_rast

  }



  log_lambda <- log(lambda)
  log_lambda[is.infinite(log_lambda)] <- NA

  # sample_log_lambda <- terra::spatSample(
  #   log_lambda,
  #   n_sample,
  #   na.rm = TRUE
  # )

  sample_log_lambda <- sapply(
    X = log_lambda,
    FUN = terra::spatSample,
    size = n_sample,
    na.rm = TRUE,
    simplify = TRUE
  ) |>
    as.data.frame()


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

  scaled_result <- lapply(
    X = scaled_rast,
    FUN = function(x, y){

      z <- x

      naidx <- is.na(values(x))

      z[naidx] <- 0

      mask(z, y)

    },
    y = lambda[[1]]
  ) |>
    rast()


  fn <- sub(
    pattern = "\\.tif",
    replacement = "_scaled.tif",
    x = lambda_file
  )

  writeRaster(
    scaled_result,
    filename = fn,
    overwrite = TRUE
  )

  rast(fn)

}
