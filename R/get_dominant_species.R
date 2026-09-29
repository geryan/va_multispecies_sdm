#' Most abundant species in each cell
#'
#' Returns a categorical raster naming the species with the highest value in
#' each cell. Cells where no species is above `threshold` get their own
#' category, ID 0: which.max() alone would give them to the first species,
#' because in the masked cells every species is exactly 0 and a tie goes to
#' the first layer. At the default of 0 that category is "None", the masked
#' cells. Above 0 it is "All <= threshold", which also takes cells where the
#' top species is too rare for "most abundant" to mean much.
#'
#' @title
#' @param pred_lambda_mean SpatRaster, one layer per species, in the order of
#'   `target_species`
#' @param target_species species names, one per layer
#' @param threshold cells whose top value is at or below this get category 0
#' @return categorical SpatRaster, ID 0 for no species and 1..n for the species
#' @author geryan
#' @export
get_dominant_species <- function(
    pred_lambda_mean,
    target_species,
    threshold = 0
) {

  if (!identical(names(pred_lambda_mean), as.character(target_species))) {
    stop(
      "get_dominant_species(): layers are not in the order of target_species",
      call. = FALSE
    )
  }

  r <- terra::ifel(
    max(pred_lambda_mean) <= threshold,
    0,
    terra::which.max(pred_lambda_mean)
  )

  none <- if (threshold == 0) {
    "None"
  } else {
    paste0("All ≤ ", threshold)
  }

  levs <- data.frame(
    ID = c(0, seq_along(target_species)),
    category = c(none, target_species)
  )

  levels(r) <- levs

  r

}
