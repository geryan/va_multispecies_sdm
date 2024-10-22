poisson_to_prob <- function(
  r,
  filename
){

  # converts poisson rate to probability of one or more individuals present
  rr <- 1 - exp(scale(-r, center = FALSE))

  writereadrast(
    rr,
    filename = filename,
    overwrite = TRUE
  )
}
