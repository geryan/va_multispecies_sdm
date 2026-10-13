#' Map of the most abundant species in each cell
#'
#' Plots the categorical raster from get_dominant_species(), one colour per
#' species. Colours come from the "Polychrome 36" qualitative palette, less
#' its two greys, and are assigned by position in the raster's category table
#' -- that is, by the order of `target_species` -- so each species keeps its
#' colour across refits. Grey is kept for the cells where no species is above
#' the threshold. The legend lists only the categories that appear on the map.
#'
#' @param dominant categorical SpatRaster from get_dominant_species()
#' @param plot_dir directory to write to
#' @param filename file name within `plot_dir`
#' @param width,height figure size, in inches
#' @return path of the file written
#' @author geryan
#' @export
make_dominant_species_plot <- function(
    dominant,
    plot_dir = "outputs/figures/distribution_plots/",
    filename = "dominant_species.png",
    width = 10,
    height = 8
) {

  if (!dir.exists(plot_dir)) {
    dir.create(
      plot_dir,
      recursive = TRUE
    )
  }

  # by position, not name: the ID column is "ID" when the raster is made but
  # comes back from the targets store as "value"
  lev <- terra::levels(dominant)[[1]]
  ids <- lev[[1]]
  labs <- lev[[2]]

  spp <- labs[ids > 0]
  none <- labs[ids == 0]

  pal <- grDevices::palette.colors(
    palette = "Polychrome 36"
  )[-(1:2)]

  if (length(spp) > length(pal)) {
    stop(
      sprintf(
        "make_dominant_species_plot(): %d species but only %d colours",
        length(spp),
        length(pal)
      ),
      call. = FALSE
    )
  }

  cols <- c(
    stats::setNames(
      unname(pal[seq_along(spp)]),
      spp
    ),
    stats::setNames(
      "grey90",
      none
    )
  )

  p <- ggplot() +
    geom_spatraster(
      data = dominant
    ) +
    scale_fill_manual(
      values = cols,
      breaks = c(spp, none),
      labels = function(x){
        as.expression(
          lapply(
            x,
            function(s){
              if (s %in% spp) {
                bquote(italic(.(paste("An.", s))))
              } else {
                s
              }
            }
          )
        )
      },
      na.value = "transparent",
      na.translate = FALSE,
      name = "Most abundant\nspecies"
    ) +
    theme_void()

  file <- file.path(
    plot_dir,
    filename
  )

  ggsave(
    filename = file,
    plot = p,
    width = width,
    height = height,
    dpi = 300,
    units = "in",
    bg = "white"
  )

  file

}
