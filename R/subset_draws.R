#' Keep only some parameters of an mcmc.list
#'
#' For handing a plotting function only the parameters it will show. bayesplot
#' converts the whole of an mcmc.list to an array before it applies
#' `regex_pars`, which for the ~3,900 parameters of the reparameterised model is
#' 1.2 GB per call. Keeps the parameters matching any of `regex`, the same union
#' `regex_pars` selects, in their original order.
#'
#' @param draws an mcmc.list, e.g. greta's draws
#' @param regex one or more regular expressions matched against the parameter
#'   names
#' @return an mcmc.list with only the matching parameters
#' @author geryan
#' @export
subset_draws <- function(
    draws,
    regex
){

  keep <- sort(
    unique(
      unlist(
        lapply(
          regex,
          grep,
          x = coda::varnames(draws)
        )
      )
    )
  )

  coda::mcmc.list(
    lapply(
      draws,
      function(ch){
        coda::mcmc(
          as.matrix(ch)[, keep, drop = FALSE],
          start = stats::start(ch),
          thin = coda::thin(ch)
        )
      }
    )
  )

}
