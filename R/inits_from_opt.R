inits_from_opt <- function(
    x,
    n_chains = 1
  ){

  if("greta.model" %in% class(x)){
    x <- opt(x)
  }

  inits <- x$par

  inits <- lapply(inits, greta:::as_2d_array)

  class(inits) <- c("initials", class(inits))


  replicate(
    n = n_chains,
    expr = inits,
    simplify = FALSE
  )

}
