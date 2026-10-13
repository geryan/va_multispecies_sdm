#' Colour-scale limits for predicted abundance
#'
#' Linear: from 0 to the largest per-layer 99th percentile, rounded up to a
#' pretty number. The 99th percentile rather than the maximum, because a few
#' cells sit far above the rest -- on the 2026-09-28 fit, 99% of melas cells
#' are below 0.16 but the maximum is 64 -- and scaling to the maximum puts
#' almost every cell in the palest colour.
#'
#' Log: to the maximum across all layers, not rounded. The log scale has room
#' for the whole range, so nothing needs capping at the top.
#'
#' Given a multi-layer raster, the result is one scale that fits every layer.
#'
#' @param r SpatRaster, one layer per species
#' @param prob quantile taken as the top of the linear scale
#' @param log_scale whether the limits are for a log scale
#' @return length-2 numeric, c(0, upper). distplot() replaces the 0 with its
#'   log floor
#' @author geryan
#' @export
abundance_limits <- function(
    r,
    prob = 0.99,
    log_scale = FALSE
){

  top <- if (log_scale) {
    max(
      terra::global(
        r,
        fun = "max",
        na.rm = TRUE
      )$max
    )
  } else {
    q <- apply(
      terra::values(r),
      2,
      stats::quantile,
      probs = prob,
      na.rm = TRUE
    )
    max(pretty(c(0, max(q))))
  }

  if (!is.finite(top) || top <= 0) {
    top <- 1
  }

  c(
    0,
    top
  )

}
