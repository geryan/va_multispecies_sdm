make_offset_pa_plots <- function(
  mod_dat_spat,
  target_species,
  last_year_offset
  ){

  for (i in 1:length(target_species)){
    for (j in 1:12){
      point_dat <- mod_dat_spat |>
        mutate(month = month(model_date)) |>
        filter(species == target_species[i]) |>
        filter(month == j) |>
        select(longitude, latitude, presence)

      ggplot() +
        geom_spatraster(data = last_year_offset[[j]]) +
        geom_point(
          data = point_dat |>
            mutate(presence = as.factor(presence)),
          aes(
            x = longitude,
            y = latitude,
            col = presence
          )
        ) +
        scale_id_continuous(
          cols = c("grey97", "springgreen2"),
          guide = "none"
        )+
        scale_colour_manual(
          values = c("yellow", "deeppink")#,
          #guide = "none"
        ) +
        theme_void() +
        labs(title = target_species[i])

      ggsave(
        filename = sprintf(
          "outputs/figures/offset/%s_%s.png",
          target_species[i],
          j
        )
      )

    }
  }

}
