#' Distribution map of one species with country borders overlaid
#'
#' The per-species map from `distplot()` with country outlines drawn on top,
#' saved by `saveplotlist()` (3200 x 3200 px, 300 dpi) as
#' `<prefix>_<species>.png` in `plot_dir`.
#'
#' The boundaries are cropped to the prediction raster, so islands outside it
#' (Cabo Verde, Mauritius, most of the Seychelles) don't widen the frame and the
#' map lines up with the borderless one.
#'
#' @param pred_dist SpatRaster with one layer per species, e.g. `pred_p`
#' @param species name of the layer to map
#' @param boundaries SpatVector of countries, e.g. `atlas_country_boundaries`
#' @param plot_dir directory to write to
#' @param colscheme,guide,log_scale,maxcell passed to `distplot()`
#' @param border_col,border_lwd colour and width (mm) of the borders
#' @param prefix start of the file name
#' @return path of the PNG written
#' @author geryan
#' @export
make_distribution_plot_borders <- function(
    pred_dist,
    species,
    boundaries,
    plot_dir = "outputs/figures/distribution_plots/",
    colscheme = "va",
    guide = "prob",
    log_scale = FALSE,
    maxcell = 5e5,
    border_col = "grey20",
    border_lwd = 0.15,
    prefix = "distribution_borders"
) {

  borders <- terra::crop(
    boundaries,
    terra::ext(pred_dist)
  )

  p <- distplot(
    pred_dist,
    species,
    colscheme = colscheme,
    guide = guide,
    log_scale = log_scale,
    maxcell = maxcell
  ) +
    tidyterra::geom_spatvector(
      data = borders,
      fill = NA,
      colour = border_col,
      linewidth = border_lwd
    )

  saveplotlist(
    plots = setNames(
      list(p),
      species
    ),
    dir = plot_dir,
    prefix = prefix
  )

  sprintf(
    "%s/%s_%s.png",
    plot_dir,
    prefix,
    species
  )

}
