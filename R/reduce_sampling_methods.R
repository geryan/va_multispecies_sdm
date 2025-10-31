#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param sampling_method
#' @return
#' @author geryan
#' @export
reduce_sampling_methods <- function(sampling_method) {

  ifelse(
    sampling_method %in% c(
      "human_baited_net_unk",
      "window_exit",
      "odour",
      "human_baited_net_ind",
      "animal_baited_net",
      "tent_trap",
      "human_baited_net_out",
      "larvae"
    ),
    "other",
    sampling_method
  )

}
