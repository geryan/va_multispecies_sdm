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
    )
  ) {

  colscheme <- match.arg(colscheme)

  sapply(
    X = names(pred_dist),
    FUN = function(
    x,
    pred_dist,
    colscheme
    ){
      distplot(pred_dist, x, colscheme)
    },
    pred_dist,
    colscheme,
    simplify = FALSE)

}
