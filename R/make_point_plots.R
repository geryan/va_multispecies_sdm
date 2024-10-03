make_point_plots <- function(
    plot_points,
    expert_maps,
    new_mask
){

  ggplot() +
    geom_spatraster(
      data = new_mask
    ) +
    geom_spatvector(
      data = expert_maps |> filter(species == "nili"),
      aes(
        alpha = 0.6
      )
    ) +
    geom_point(
      data = plot_points |> filter(species == "nili"),
      aes(
        x = lon,
        y = lat,
        col = type
      )
    )

}
