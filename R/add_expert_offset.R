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

      # coluzzii has its own map (Wiebe et al. 2017) since 2026-11-23, so it
      # takes its own layer below rather than the gambiae one
      if(sp %in% c("gambiae", "gambiae_coluzzii", "gambiae_complex")){
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
