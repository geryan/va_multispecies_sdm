#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param expert_offset_maps
#' @param non_expert_offset_maps
#' @return
#' @author geryan
#' @export
combine_range_maps <- function(
    expert_offset_maps,
    non_expert_offset_maps,
    target_species,
    project_mask
  ) {




  r <- c(
    expert_offset_maps |>
      crop(project_mask) |>
      mask(project_mask),
    non_expert_offset_maps |>
      crop(project_mask) |>
      mask(project_mask)
  )

}
