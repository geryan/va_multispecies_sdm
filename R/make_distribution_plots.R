#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param pred_dist
#' @param model_data_spatial
#' @param plot_dir description
#' @return
#' @author geryan
#' @export
make_distribution_plots <- function(
    pred_dist,
    model_data_spatial,
    plot_dir = "outputs/figures/distribution_plots/"
) {


  dist_plots_va <- distplotlist(
    pred_dist,
    colscheme = "va"
  )

  saveplotlist(
    dist_plots_va,
    dir = plot_dir,
    prefix = "distribution"
  )

  dist_plots_rb <- distplotlist(
    pred_dist,
    colscheme = "rb"
  )

  saveplotlist(
    dist_plots_rb,
    dir = plot_dir,
    prefix = "distribution_rb"
  )


  dist_plots_va_points <- add_pa_points_list(
    dist_plots_va,
    model_data_spatial
  )

  saveplotlist(
    dist_plots_va_points,
    dir = plot_dir,
    prefix = "distpoints"
  )


  dist_plots_rb_points <- add_pa_points_list(
    dist_plots_rb,
    model_data_spatial,
    colp = "yellow"
  )

  saveplotlist(
    dist_plots_rb_points,
    dir = plot_dir,
    prefix = "distpoints_rb"
  )


}
