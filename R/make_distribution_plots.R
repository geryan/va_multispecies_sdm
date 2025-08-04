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


  dist_plots_mako <- distplotlist(
    pred_dist,
    colscheme = "mako"
  )

  saveplotlist(
    dist_plots_mako,
    dir = "outputs/figures/distribution_plots/distn_20250804",
    prefix = "distribution"
  )


}
