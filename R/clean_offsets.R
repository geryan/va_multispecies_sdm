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
    project_mask_5,
    replacement = 0
  ) {

  app(
    offsets_raw,
    fun = function(x){
      ifelse(is.na(x), replacement, x)
    }
  ) * project_mask_5

}
