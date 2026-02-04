#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param offsets_raw
#' @return
#' @author geryan
#' @export
clean_offsets <- function(
    offsets_raw,
    project_mask_5
  ) {

  app(
    offsets_raw,
    fun = function(x){
      ifelse(is.na(x), 0, x)
    }
  ) * project_mask_5

}
