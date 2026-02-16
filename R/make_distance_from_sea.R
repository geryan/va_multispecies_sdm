#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param project_mask_5
#' @param project_mask_5_outline
#' @return
#' @author geryan
#' @export
make_distance_from_sea <- function(
    project_mask_5,
    project_mask_5_outline
  ) {


  r <- project_mask_5_outline #|>
    #aggregate(fact = 2)
  idx <- is.na(values(r))
  r[idx] <- NA
  r[!idx] <- 1


  eroded_mask <- focal(
    r,
    w = matrix(1,3,3),
    fun = min,
    na.rm = TRUE
  )

  internal_NA <- is.na(r) & (eroded_mask == 1)

  to_fill <- classify(
    r,
    cbind(NA, -9999)
  )

  filled <- focal(
    to_fill,
    w = matrix(1,3,3),
    fun = function(x, ...) {
      center <- x[5] # center pixel
      if(center == -9999) {
        mean(x[x != -9999 & !is.na(x)], na.rm=TRUE)
      } else {
        center
      }
    },
    na.policy="omit"
  )

  r[internal_NA] <- filled[internal_NA]


  rnaidx <- which(is.na(values(r)))
  ridx  <- which(!is.na(values(r)))
  dfs <- r
  dfs[rnaidx] <- 0
  dfs[ridx] <- NA

  dist_from_sea <- distance(dfs) |>
    mask(project_mask_5) |>
    set_layer_names(
      layernames = "distance_from_sea"
    ) |>
    scale()

  dist_from_sea

}
