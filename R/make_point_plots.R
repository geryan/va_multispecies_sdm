make_point_plots <- function(
    plot_points,
    expert_maps,
    new_mask
){



  plot_points_expert_map(
    sp = "gambiae",
    new_mask,
    plot_points
  )

  plot_points_expert_map(
    sp = "coluzzii",
    new_mask,
    plot_points
  )

}
