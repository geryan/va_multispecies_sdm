#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pred_dist
#' @param colscheme
#' @return
#' @author geryan
#' @export
distplotlist <- function(
    pred_dist,
    colscheme =  c(
      "va",
      "mako",
      "rb",
      "magma",
      "rocket",
      "mono",
      "orchid",
      "brick"
    ),
    guide = c(
      "prob",
      "none",
      "abundance",
      "cv"
    ),
    log_scale = FALSE
  ) {

  colscheme <- match.arg(colscheme)

  guide <- match.arg(guide)

  # one scale for every species, so maps can be compared by colour and a
  # multi-panel figure needs only one legend
  limits <- switch(
    guide,
    none = NULL,
    prob = c(0, 1),
    abundance = abundance_limits(
      pred_dist,
      log_scale = log_scale
    ),
    cv = c(0, 1)
  )

  sapply(
    X = names(pred_dist),
    FUN = function(
    x,
    pred_dist,
    colscheme
    ){
      distplot(
        pred_dist,
        x,
        colscheme,
        guide = guide,
        limits = limits,
        log_scale = log_scale
      )
    },
    pred_dist,
    colscheme,
    simplify = FALSE)

}
