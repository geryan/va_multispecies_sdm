#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param predfilelist
#' @param expert_offset_maps
#' @return
#' @author geryan
#' @export
add_expert_offset <- function(
    predfilelist,
    expert_offset_maps
  ) {

  predraw <- rast(predfilelist$pa)

  # species_lookup <- list(
  #   arabiensis = "arabiensis",
  #   gambiae = c("coluzzii", "gambiae", "gambiae_coluzzii", "gambiae_complex"),
  #   funestus = c("funestus", "funestus_complex"),
  #   melas = "melas",
  #   merus = "merus",
  #   moucheti = "moucheti",
  #   nili = "nili"
  # )
  #


  r <- c(
    terra::subset(predraw, "arabiensis") * expert_offset_maps$arabiensis,
    terra::subset(predraw, "coluzzii") * expert_offset_maps$gambiae,
    terra::subset(predraw, "funestus") * expert_offset_maps$funestus,
    terra::subset(predraw, "gambiae") * expert_offset_maps$gambiae,
    terra::subset(predraw, "melas") * expert_offset_maps$melas,
    terra::subset(predraw, "merus") * expert_offset_maps$merus,
    terra::subset(predraw, "moucheti") * expert_offset_maps$moucheti,
    terra::subset(predraw, "nili") * expert_offset_maps$nili
  )

  filename <- sub(
    pattern = "\\.tif",
    replacement = "_expoff.tif",
    x = predfilelist$pa
  )

  writeRaster(
    x = r,
    filename = filename,
    overwrite = TRUE
  )

}
