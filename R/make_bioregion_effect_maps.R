#' Panel of maps of the bioregion terms' combined effect, every species
#'
#' Maps bioregion_effect_rast() on a shared diverging log scale centred on 1:
#' red where the bioregion terms raise a species' relative abundance above
#' what its main effects give, blue where they lower it. Values beyond
#' `limits` take the end colours.
#'
#' Writes `bioregion_effect_maps.png` into `plot_dir`.
#'
#' @param bioregion_effect SpatRaster from bioregion_effect_rast(), one layer
#'   per species
#' @param plot_dir directory to write to
#' @param limits lower and upper ends of the colour scale
#' @param ncol number of panel columns
#' @param panel_size width and height of each panel, in inches
#' @return path of the file written
#' @author geryan
#' @export
make_bioregion_effect_maps <- function(
    bioregion_effect,
    plot_dir,
    limits = c(0.25, 4),
    ncol = 6,
    panel_size = 2.3
){

  if (!dir.exists(plot_dir)) {
    dir.create(
      plot_dir,
      recursive = TRUE
    )
  }

  breaks <- 2^seq(
    log2(limits[1]),
    log2(limits[2])
  )

  break_labels <- format(
    breaks,
    drop0trailing = TRUE,
    trim = TRUE
  )
  break_labels[1] <- paste0("≤", break_labels[1])
  break_labels[length(breaks)] <- paste0("≥", break_labels[length(breaks)])

  p <- ggplot() +
    geom_spatraster(
      data = bioregion_effect
    ) +
    facet_wrap(
      ~ lyr,
      ncol = ncol,
      labeller = labeller(
        lyr = function(x) paste("An.", x)
      )
    ) +
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "grey97",
      high = "#B2182B",
      midpoint = 1,
      transform = "log",
      limits = limits,
      oob = scales::squish,
      breaks = breaks,
      labels = break_labels,
      na.value = "transparent",
      name = "Abundance\nmultiplier from\nbioregion terms"
    ) +
    theme_void(
      base_size = 9
    ) +
    theme(
      strip.text = element_text(
        face = "italic"
      )
    )

  f <- file.path(
    plot_dir,
    "bioregion_effect_maps.png"
  )

  ggsave(
    filename = f,
    plot = p,
    width = ncol * panel_size + 1.5,
    height = ceiling(terra::nlyr(bioregion_effect) / ncol) * panel_size,
    dpi = 300,
    units = "in",
    bg = "white"
  )

  f

}
