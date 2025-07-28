inits <- function(
  n_chains = 1,
  nsp = 4, # number of species / alpha values
  ncv = 10, # nuber of covariates of abundance/presence / beta/alpha covariates
  #ncb = 1, # number of bias layers / delta values
  ina = -15e-1, # mean initial value for alpha
  inb = 1e-1, # mean initial value for beta
  ing = -1, # mean initial value for gamma
  ind = 1e-4, # mean initial value for delta
  sda = 1e-2,
  sdb = 3e-1,
  sdg = 1e-4,
  sdd = 1e-4
){

  # todo:
  # this function still needs to be able to dynamically cope
  # with >1 bias layer

  n_a <- nsp
  n_b <- nsp * ncv
  n_g <- nsp
  n_d <- 1

  inits_internal <- function(
    nsp,
    ncv,
    ina,
    inb,
    ing,
    ind,
    sda,
    sdb,
    sdg,
    sdd
  ) {
    list(
      initials(
        alpha = rnorm(
          n = n_a,
          mean = ina,
          sd = sda
        ),
        beta = matrix(
          data = rnorm(
            n = n_b,
            mean = inb,
            sd = sdb
          ),
          ncol = ,
          nrow = ncv
        ),
        gamma = rnorm(
          n = n_g,
          mean = ing,
          sd = sdg
        ),
        delta = abs(
          rnorm(
            n = n_d,
            mean = ind,
            sd = sdd
          )
        ) # this taking of the absolute value is a hack to ensure it's >= zero, per the truncated normal in the model
      )
    )
  }

  replicate(
    n = n_chains,
    expr = inits_internal(
      nsp,
      ncv,
      ina,
      inb,
      ing,
      ind,
      sda,
      sdb,
      sdg,
      sdd
    )
  )
}
