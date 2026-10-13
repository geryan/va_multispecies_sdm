#' Survey records beside travel time from research facilities
#'
#' Two maps side by side:
#'
#' - left: every observed record, one point per species, location and outcome.
#'   Colour is whether the species was detected; shape is the species. Inferred
#'   zeros and background points are left out, so these are the records as
#'   collected. Undetected points are drawn first, so detections sit on top.
#' - right: travel time from research facilities, in hours, as drawn by
#'   plot_traveltime(), from the ~1 km layer aggregated by `fact`
#'
#' The `featured` species get the four filled shapes; the rest get open and
#' line shapes. Eighteen species is the most there are distinct shapes for.
#'
#' @param model_data_spatial the model data
#' @param project_mask SpatRaster drawn as the grey land behind the records;
#'   the travel-time layer is cropped to its extent
#' @param traveltime SpatRaster, travel time in minutes
#' @param file path of the PNG to write
#' @param featured species drawn with filled shapes
#' @param fact aggregation factor for the travel-time layer
#' @param width,height figure size in inches
#' @return `file`
#' @author geryan
#' @export
make_records_traveltime_plot <- function(
    model_data_spatial,
    project_mask,
    traveltime,
    file = "outputs/figures/records_traveltime.png",
    featured = c(
      "arabiensis",
      "coluzzii",
      "funestus",
      "gambiae"
    ),
    fact = 5,
    width = 15,
    height = 7.5
){

  records <- model_data_spatial |>
    filter(
      data_type != "bg",
      !inferred
    ) |>
    distinct(
      species,
      latitude,
      longitude,
      presence
    ) |>
    arrange(
      presence
    ) |>
    mutate(
      detected = factor(
        if_else(
          presence == 1,
          "Detected",
          "Undetected"
        ),
        levels = c(
          "Detected",
          "Undetected"
        )
      )
    )

  spp <- sort(unique(records$species))
  others <- setdiff(spp, featured)

  filled <- c(16, 17, 15, 18)
  open <- c(1, 2, 0, 5, 6, 3, 4, 8, 7, 9, 10, 11, 12, 13, 14)

  if (length(intersect(featured, spp)) > length(filled) ||
      length(others) > length(open)) {
    stop(
      "make_records_traveltime_plot(): more species than distinct shapes",
      call. = FALSE
    )
  }

  shapes <- c(
    stats::setNames(
      filled[seq_along(intersect(featured, spp))],
      intersect(featured, spp)
    ),
    stats::setNames(
      open[seq_along(others)],
      others
    )
  )[spp]

  p_records <- ggplot() +
    tidyterra::geom_spatraster(
      data = project_mask
    ) +
    scale_fill_gradient(
      low = "grey85",
      high = "grey85",
      na.value = "transparent",
      guide = "none"
    ) +
    geom_point(
      data = records,
      aes(
        x = longitude,
        y = latitude,
        shape = species,
        colour = detected
      ),
      size = 1.2,
      stroke = 0.4,
      alpha = 0.8
    ) +
    scale_shape_manual(
      values = shapes,
      breaks = spp,
      labels = function(x){
        as.expression(
          lapply(
            x,
            function(s){
              bquote(italic(.(paste("An.", s))))
            }
          )
        )
      },
      name = "Species"
    ) +
    scale_colour_viridis_d(
      name = "Occurrence"
    ) +
    guides(
      colour = guide_legend(
        order = 1,
        override.aes = list(
          shape = 16,
          size = 3,
          alpha = 1
        )
      ),
      shape = guide_legend(
        order = 2,
        override.aes = list(
          size = 2.5,
          alpha = 1
        )
      )
    ) +
    theme_void() +
    labs(
      title = "Survey records"
    )

  tt <- traveltime |>
    terra::crop(
      y = project_mask
    ) |>
    terra::aggregate(
      fact = fact,
      fun = "mean",
      na.rm = TRUE
    )

  p <- patchwork::wrap_plots(
    p_records,
    plot_traveltime(tt),
    ncol = 2
  )

  dir.create(
    dirname(file),
    recursive = TRUE,
    showWarnings = FALSE
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
