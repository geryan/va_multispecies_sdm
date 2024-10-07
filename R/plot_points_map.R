plot_points_map <- function(
    sp,
    new_mask,
    plot_points
){

  spname <- paste0("Anopheles ", sp)


  ggplot() +
    geom_spatraster(
      data = new_mask
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
    scale_id_continuous( # from idpalette, necessary bc tidyterra don't play nice
      cols = "grey90",
      aesthetics = "fill"
    ) +
    theme_void() +
    labs(
      title = bquote(italic(.(spname))),
      subtitle = "Occurrence data",
      col = "Data type"
    ) +
    guides(
      fill = "none",
      alpha = "none"
    )

}
