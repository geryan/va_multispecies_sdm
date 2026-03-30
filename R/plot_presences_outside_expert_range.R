#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param presences_outside_expert_range
#' @param expert_maps
#' @return
#' @author geryan
#' @export
plot_presences_outside_expert_range <- function(
    presences_outside_expert_range,
    expert_maps
  ) {

  mspp <- unique(presences_outside_expert_range$species)

  ggplot() +
    geom_spatvector(
      data = expert_maps |>
        filter(species %in% mspp)
    ) +
    geom_spatvector(
      data = presences_outside_expert_range
    ) +
    facet_wrap(~species)

  ggsave("outputs/figures/presences_outside_expert_range.png")

}
