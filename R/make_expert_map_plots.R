make_expert_map_plots <- function(
    expert_maps,
    new_mask
){

  allspp <- expert_maps$species


  mapply(
    FUN = function(
    allspp,
    expert_map,
    new_mask
    ){

      p <- plot_expert_map(
        expert_map,
        new_mask
      )

      ggsave(
        filename = sprintf(
          "outputs/figures/expert_map_plots/expert_map_%s.png",
          allspp
        ),
        plot = p,
        width = 2000,
        height = 2000,
        units = "px"
      )

    },
    allspp,
    expert_maps,
    MoreArgs = list(
      new_mask
    )
  )




}
