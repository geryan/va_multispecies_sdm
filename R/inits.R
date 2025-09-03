# inits <- function(
#   n_chains = 1,
#   nsp = 4, # number of species / alpha values
#   ncv = 1, # nuber of covariates of abundance/presence / beta/alpha covariates
#   #ncb = 1, # number of bias layers / delta values
#   ina = -4, # mean initial value for alpha
#   inb = -0.5, # mean initial value for beta
#   ing = 0,
#   ind = 1e-4,
#   sda = 0,
#   sdb = 0,
#   sdg = 0,
#   sdd = 0
# ){
#
#   # todo:
#   # this function still needs to be able to dynamically cope
#   # with >1 bias layer
#
#   n_a <- nsp
#   n_b <- nsp * ncv
#   n_g <- nsp
#   n_d <- 1
#
#   inits_internal <- function(
#     nsp,
#     ncv,
#     ina,
#     inb,
#     ing,
#     ind,
#     sda,
#     sdb,
#     sdg,
#     sdd
#   ) {
#     list(
#       initials(
#         alpha = rnorm(
#           n = n_a,
#           mean = ina,
#           sd = sda
#         ),
#         # beta = matrix(
#         #   data = rnorm(
#         #     n = n_b,
#         #     mean = inb,
#         #     sd = sdb
#         #   ),
#         #   ncol = ,
#         #   nrow = ncv
#         # ),
#         gamma = rnorm(
#           n = n_g,
#           mean = ing,
#           sd = sdg
#         )#,
#         # delta = abs(
#         #   rnorm(
#         #     n = n_d,
#         #     mean = ind,
#         #     sd = sdd
#         #   )
#         # ) # this taking of the absolute value is a hack to ensure it's >= zero, per the truncated normal in the model
#       )
#     )
#   }
#
#   replicate(
#     n = n_chains,
#     expr = inits_internal(
#       nsp,
#       ncv,
#       ina,
#       inb,
#       ing,
#       ind,
#       sda,
#       sdb,
#       sdg,
#       sdd
#     )
#   )
# }

inits <- function(
    n_chains = 1,
    nsp = 4, # number of species / alpha values
    ncv = 1, # nuber of covariates of abundance/presence / beta/alpha covariates
    #ncb = 1, # number of bias layers / delta values
    nsm = 10, # n sampling methods
    ina = -4, # mean initial value for alpha
    inb = 0, # mean initial value for beta
    ing = 0,
    inre = 1e-4,
    inresd = 1e-4,
    ind = 1e-4,
    sda = 0,
    sdb = 0,
    sdg = 0,
    sdd = 0,
    sdre = 0,
    sdsdre = 0
){

  # todo:
  # this function still needs to be able to dynamically cope
  # with >1 bias layer

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
    sdd,
    nsm,
    inre,
    inresd,
    sdre,
    sdsdre
  ) {

    n_a <- nsp
    n_b <- nsp * ncv
    n_g <- nsp
    n_d <- 1

    list(
      initials(
        alpha = rnorm(
          n = n_a,
          mean = ina,
          sd = sda
        ),
        # beta = matrix(
        #   data = rnorm(
        #     n = n_b,
        #     mean = inb,
        #     sd = sdb
        #   ),
        #   ncol = ,
        #   nrow = ncv
        # ),
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
        )#, # this taking of the absolute value is a hack to ensure it's >= zero, per the truncated normal in the model
        # sampling_re_raw = rnorm(
        #   n = nsm,
        #   mean = inre,
        #   sd = sdre
        # ),
        # sampling_re_sd = abs(
        #   rnorm(
        #     n = 1,
        #     mean = inresd,
        #     sd = sdsdre
        #   )
        # )
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
      sdd,
      nsm,
      inre,
      inresd,
      sdre,
      sdsdre
    )
  )
}

