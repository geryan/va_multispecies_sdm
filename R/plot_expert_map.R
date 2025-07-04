plot_expert_map <- function(
  new_mask,
  expert_map
){

  spname <- paste0("Anopheles ", expert_map$species[1])


  ggplot() +
    geom_spatraster(
      data = new_mask
    ) +
    geom_spatvector(
      data = expert_map,
      aes(
        alpha = 0.6
      ),
      fill = "springgreen"
    ) +
    scale_id_continuous( # from idpalette, necessary bc tidyterra don't play nice
      cols = "grey90",
      aesthetics = "fill"
    ) +
    theme_void() +
    labs(
      title = bquote(italic(.(spname))),
      subtitle = "Expert map boundary per Sinka et al. 2010",
      col = "Data type"
    ) +
    guides(
      fill = "none",
      alpha = "none"
    )

}
