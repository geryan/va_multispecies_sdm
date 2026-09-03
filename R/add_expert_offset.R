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
    predraw,
    expert_offset_maps
) {


  r <- sapp(
    x = predraw,
    fun = function(x, expert_offset_maps){
      sp <- names(x)

      if(sp %in% c("coluzzii", "gambiae", "gambiae_coluzzii", "gambiae_complex")){
        terra::subset(x, sp) * terra::subset(expert_offset_maps, "gambiae")
      } else if (sp %in% c("funestus", "funestus_complex")){
        terra::subset(x, sp) * terra::subset(expert_offset_maps, "funestus")
      } else if (sp %in% names(expert_offset_maps)) {
        terra::subset(x, sp) * terra::subset(expert_offset_maps, sp)
      } else {
        terra::subset(x, sp)
      }
    },
    expert_offset_maps
  )

  r

}
