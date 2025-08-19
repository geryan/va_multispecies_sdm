plot_points_map <- function(
    sp,
    new_mask,
    data,
    expert_map = NULL
){

  spname <- paste0("Anopheles ", sp)


  if(is.null(expert_map)){
    p <- ggplot() +
      geom_spatraster(
        data = new_mask
      ) +
      geom_point(
        data = data |>
          mutate(
            detected = case_when(
              presence == 1 ~ "Detected",
              presence == 0 ~ "Undetected"
            )
          ),
        aes(
          x = longitude,
          y = latitude,
          shape = data_type,
          col = detected
        ),
        alpha = 0.7
      ) +
      scale_colour_viridis_d() +
      scale_id_continuous( # from idpalette, necessary bc tidyterra don't play nice
        cols = "grey70",
        aesthetics = "fill"
      ) +
      theme_void() +
      labs(
        title = bquote(italic(.(spname))),
        subtitle = "Occurrence data",
        col = "Occurrence",
        shape = "Data type"
      ) +
      guides(
        fill = "none",
        alpha = "none"
      )
  } else {
    p <- ggplot() +
      geom_spatraster(
        data = new_mask
      ) +
      geom_spatvector(
        data = expert_map,
        aes(
          alpha = 0.6
        )
      ) +
      geom_point(
        data = data |>
          mutate(
            detected = case_when(
              presence == 1 ~ "Detected",
              presence == 0 ~ "Undetected"
            )
          ),
        aes(
          x = longitude,
          y = latitude,
          shape = data_type,
          col = detected
        ),
        alpha = 0.7
      ) +
      scale_colour_viridis_d() +
      scale_id_continuous( # from idpalette, necessary bc tidyterra don't play nice
        cols = "grey70",
        aesthetics = "fill"
      ) +
      theme_void() +
      labs(
        title = bquote(italic(.(spname))),
        subtitle = "Occurrence data with black outline showing Sinka et al. 2010 expert map boundary.",
        col = "Occurrence",
        shape = "Data type"
      ) +
      guides(
        fill = "none",
        alpha = "none"
      )
  }

  p

}
