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

  r <- sapp(
    x = predraw,
    fun = function(x, expert_offset_maps){
      sp <- names(x)

      if(sp %in% c("coluzzii", "gambiae", "gambiae_coluzzii", "gambiae_complex")){
        terra::subset(x, sp) * terra::subset(expert_offset_maps, "gambiae")
      } else if (sp %in% c("funestus", "funestus_complex")){
        terra::subset(x, sp) * terra::subset(expert_offset_maps, "funestus")
      } else {
        terra::subset(x, sp) * terra::subset(expert_offset_maps, sp)
      }
    },
    expert_offset_maps
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

  filename

}
