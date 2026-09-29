#' All-species panel figures of a prediction raster
#'
#' The one-figure counterpart of make_distribution_plots(): same arguments,
#' same panels -- built by distplotlist() and add_pa_points_list(), so each
#' panel is the plot make_distribution_plots() writes for that species -- but
#' laid out as one grid of every species instead of one file each. Legends that
#' are identical across panels (the 0-1 probability scale, the detected /
#' undetected points) are collected into one.
#'
#' Writes `panel_<prefix>.png`, plus `panel_distpoints.png` if `distpoints`,
#' into `plot_dir`, and returns their paths.
#'
#' @param pred_dist SpatRaster, one layer per species
#' @param model_data_spatial the model data, for the detection points
#' @param plot_dir directory to write to
#' @param colp,cola point colours for detected / undetected
#' @param colscheme,guide,log_scale passed to distplotlist()
#' @param distpoints whether to also write the version with points
#' @param prefix names the file, as in make_distribution_plots()
#' @param ncol number of panel columns; rows are however many that needs
#' @param panel_size width and height of each panel, in inches
#' @return paths of the files written
#' @author geryan
#' @export
make_distribution_panels <- function(
    pred_dist,
    model_data_spatial,
    plot_dir = "outputs/figures/distribution_plots/",
    colp = "cadetblue2",
    cola = "yellow",
    colscheme = "va",
    distpoints = FALSE,
    guide = c("prob", "none", "abundance", "cv"),
    prefix = "distribution",
    ncol = 6,
    panel_size = 2.5,
    log_scale = FALSE
) {

  guide <- match.arg(guide)

  if (!dir.exists(plot_dir)) {
    dir.create(
      plot_dir,
      recursive = TRUE
    )
  }

  dist_plots <- distplotlist(
    pred_dist,
    colscheme = colscheme,
    guide = guide,
    log_scale = log_scale
  )

  panels <- list(dist_plots)
  names(panels) <- prefix

  if (distpoints) {
    panels$distpoints <- add_pa_points_list(
      dist_plots,
      model_data_spatial,
      colp = colp,
      cola = cola
    )
  }

  nrow <- ceiling(length(dist_plots) / ncol)

  files <- file.path(
    plot_dir,
    sprintf(
      "panel_%s.png",
      names(panels)
    )
  )

  for (i in seq_along(panels)) {

    has_legend <- guide != "none" || names(panels)[i] == "distpoints"

    # titles shrunk so the longest name (quadriannulatus) does not run into
    # the next panel's
    p <- patchwork::wrap_plots(
      panels[[i]],
      ncol = ncol,
      nrow = nrow,
      guides = "collect"
    ) &
      theme(
        plot.title = element_text(size = 9)
      )

    ggsave(
      filename = files[i],
      plot = p,
      width = ncol * panel_size + if (has_legend) 1.5 else 0,
      height = nrow * panel_size,
      dpi = 300,
      units = "in",
      bg = "white"
    )

  }

  files

}
