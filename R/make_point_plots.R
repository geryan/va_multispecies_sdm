make_point_plots <- function(
    model_data_spatial,
    expert_maps,
    new_mask
){

  alsp <- unique(plot_points$species)
  exsp <- expert_maps$species

  map_type <- case_when(
    alsp %in% exsp ~ "exp",
    "gambiae" %in% exsp & alsp %in% c("coluzzii", "gambiae_complex") ~ "exp",
    TRUE ~ "plain"
  )

  map_exp_sp <- ifelse(
    alsp %in% c("coluzzii", "gambiae_complex"),
    "gambiae",
    alsp
  )

  mapply(
    FUN = function(
      alsp,
      map_type,
      map_exp_sp,
      plot_points,
      expert_maps,
      new_mask
    ){
      if(map_type == "exp"){
        p <- plot_points_expert_map(
          alsp,
          new_mask,
          plot_points,
          expert_map = expert_maps |>
            filter(species == map_exp_sp)
        )
      } else {
        p <- plot_points_map(
          alsp,
          new_mask,
          plot_points
        )
      }

      ggsave(
        filename = sprintf(
          "outputs/figures/point_plots/points_%s.png",
          alsp
        ),
        plot = p,
        width = 3600,
        height = 2000,
        units = "px"
      )

    },
    alsp,
    map_type,
    map_exp_sp,
    MoreArgs = list(
      plot_points,
      expert_maps,
      new_mask
    )
  )




}
