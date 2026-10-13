#' Binary ranges from per-species probability thresholds
#'
#' 1 where a species is predicted present, 0 where absent, NA where the
#' probability is NA. Present means above the species' threshold (at or above,
#' for a threshold of 0) -- predicted_present(), the rule maxsss_thresholds()
#' chose the threshold under. Thresholds are matched to layers by species name.
#'
#' @param pred_p SpatRaster of probability of occurrence, one layer per species
#' @param thresholds tibble with `species` and `threshold`
#' @return SpatRaster of 0 / 1, one layer per species
#' @author geryan
#' @export
binarise_by_threshold <- function(
    pred_p,
    thresholds
){

  th <- thresholds$threshold[match(names(pred_p), thresholds$species)]

  if (anyNA(th)) {
    warning(
      sprintf(
        "binarise_by_threshold(): no threshold for %s; left NA",
        paste(names(pred_p)[is.na(th)], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  r <- terra::rast(
    lapply(
      seq_along(th),
      function(i){
        if (is.na(th[i])) {
          pred_p[[i]] * NA
        } else {
          predicted_present(
            pred_p[[i]],
            th[i]
          )
        }
      }
    )
  ) * 1

  names(r) <- names(pred_p)

  r

}
