mask_landcover_and_expert_offset <- function(
    p = pred_dist_not_masked,
    expert = expert_offset_maps_10,
    bare = landcover_bare_10
){

  r <- p |>
    add_expert_offset(
      expert_offset_maps = expert
    )

  r * (1 - bare)

}
