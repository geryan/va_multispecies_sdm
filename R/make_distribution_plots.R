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


  eo_plots <- sapply(
    X = names(expert_offset_preds_mspp),
    FUN = function(x, expert_offset_preds_mspp){
      distplot(expert_offset_preds_mspp, x)
    },
    expert_offset_preds_mspp
  )


}
