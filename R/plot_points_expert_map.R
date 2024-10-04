plot_points_expert_map <- function(
  sp,
  new_mask,
  plot_points
){

  spname <- paste0("Anopheles ", sp)

  ggplot() +
    geom_spatraster(
      data = new_mask
    ) +
    geom_spatvector(
      data = expert_maps |> filter(species == sp),
      aes(
        alpha = 0.6
      )
    ) +
    geom_point(
      data = plot_points |> filter(species == sp),
      aes(
        x = lon,
        y = lat,
        col = type
      ),
      alpha = 0.7
    ) +
    scale_colour_viridis_d() +
    scale_fill_viridis_c(
      option = "G",
      begin = 1,
      end = 0.8,
      na.value = "transparent"
    ) +
    theme_void() +
    labs(
      title = expression(italic(spname))
    )

}
