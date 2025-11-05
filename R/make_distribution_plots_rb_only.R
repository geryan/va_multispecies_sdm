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
make_distribution_plots_rb_only <- function(
    pred_dist,
    model_data_spatial,
    plot_dir = "outputs/figures/distribution_plots/"
) {


  dist_plots_rb <- distplotlist(
    pred_dist,
    colscheme = "rb"
  )

  saveplotlist(
    dist_plots_rb,
    dir = plot_dir,
    prefix = "distribution_rb"
  )


}
