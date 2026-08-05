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
    plot_dir = "outputs/figures/distribution_plots/",
    colp = "cadetblue2",
    cola = "yellow",
    colscheme = "va",
    distpoints = TRUE,
    guide = c("prob", "none")
) {

  dist_plots_va <- distplotlist(
    pred_dist,
    colscheme = colscheme,
    guide = guide
  )

  saveplotlist(
    dist_plots_va,
    dir = plot_dir,
    prefix = "distribution"
  )

  if(distpoints){

    dist_plots_va_points <- add_pa_points_list(
      dist_plots_va,
      model_data_spatial,
      colp = colp,
      cola = cola
    )

    saveplotlist(
      dist_plots_va_points,
      dir = plot_dir,
      prefix = "distpoints"
    )

  }


  NULL

}
