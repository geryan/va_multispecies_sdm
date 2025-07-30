make_point_plots <- function(
    model_data_spatial,
    expert_maps,
    new_mask
){

  alsp <- model_data_spatial$species |>
    na.omit() |>
    unique()

  plot_data <- model_data_spatial |>
    filter(data_type != "bg") |>
    select(
      species,
      latitude,
      longitude,
      sampling_method,
      data_type,
      presence,
      count
    ) |>
    distinct()

  npd <- plot_data |>
    select(
      -sampling_method,
      -count
    ) |>
    nest_by(species)

  mapply(
    FUN = function(data, sp, new_mask){

      p <- plot_points_map(
        sp,
        new_mask,
        data
      )

      ggsave(
        filename = sprintf(
          "outputs/figures/point_plots/points_%s.png",
          sp
        ),
        plot = p,
        width = 3600,
        height = 2000,
        units = "px"
      )

      sp

    },
    data = npd$data,
    sp = npd$species,
    MoreArgs = list(
      new_mask = new_mask
    ),
    SIMPLIFY = FALSE
  )



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
