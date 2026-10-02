#' Map of travel time from research facilities, in hours
#'
#' The travel-time map used on the country atlas pages, the continental atlas
#' page and the records figure. The scale is capped at the 99th percentile of
#' the layer, rounded up (abundance_limits()), with the top label marked ">=".
#' A layer with no data at all (some island states) gives a note instead of an
#' empty map. A layer too large for the page is averaged down with
#' raster_for_pdf() after the cap is taken, and every cell is drawn. For a PDF,
#' save it through sharpen_raster_grobs().
#'
#' @param traveltime SpatRaster, travel time in minutes
#' @param outline optional SpatVector drawn over the map, e.g. a country border
#' @param title plot title
#' @param place name used in the no-data note
#' @return ggplot
#' @author geryan
#' @export
plot_traveltime <- function(
    traveltime,
    outline = NULL,
    title = "Travel time from research facilities",
    place = "this area"
){

  tt <- traveltime / 60
  names(tt) <- "travel_time_hours"

  has_traveltime <- terra::global(
    tt,
    fun = "notNA"
  )$notNA > 0

  if (!has_traveltime) {
    return(
      ggplot() +
        annotate(
          "text",
          x = 0,
          y = 0,
          label = sprintf(
            "No travel-time data for %s.",
            place
          ),
          size = 6
        ) +
        theme_void()
    )
  }

  cap <- abundance_limits(tt)[2]
  breaks <- pretty(c(0, cap))
  breaks <- breaks[breaks <= cap]

  outline_layer <- if (is.null(outline)) {
    NULL
  } else {
    tidyterra::geom_spatvector(
      data = outline,
      fill = NA,
      colour = "grey30",
      linewidth = 0.3
    )
  }

  ggplot() +
    tidyterra::geom_spatraster(
      data = raster_for_pdf(tt),
      maxcell = Inf
    ) +
    scale_fill_viridis_c(
      option = "mako",
      direction = -1,
      limits = c(0, cap),
      oob = scales::squish,
      breaks = breaks,
      labels = function(x){
        lab <- as.character(signif(x, 2))
        top <- !is.na(x) & x >= cap * (1 - 1e-9)
        lab[top] <- paste0("≥", lab[top])
        lab
      },
      na.value = "transparent",
      name = "Hours"
    ) +
    outline_layer +
    theme_void() +
    labs(
      title = title
    )

}

#' Continental travel-time page for the country atlas
#'
#' Crops the ~1 km travel-time layer to `extent` and aggregates it by `fact`
#' before drawing it with plot_traveltime(), so the 99th-percentile cap is not
#' taken over ~100 million cells.
#'
#' @param traveltime SpatRaster, travel time in minutes
#' @param extent anything terra::crop() takes, e.g. the project mask
#' @param file path of the PDF to write
#' @param fact aggregation factor
#' @param width,height page size in inches
#' @return `file`
#' @author geryan
#' @export
make_traveltime_page <- function(
    traveltime,
    extent,
    file,
    fact = 5,
    width = 8,
    height = 7
){

  tt <- traveltime |>
    terra::crop(
      y = extent
    ) |>
    terra::aggregate(
      fact = fact,
      fun = "mean",
      na.rm = TRUE
    )

  dir.create(
    dirname(file),
    recursive = TRUE,
    showWarnings = FALSE
  )

  ggsave(
    filename = file,
    plot = sharpen_raster_grobs(
      plot_traveltime(
        tt,
        place = "Africa"
      )
    ),
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf
  )

  file

}
