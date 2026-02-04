#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param offsets
#' @return
#' @author geryan
#' @export
make_mask_from_offsets <- function(offsets_raw) {

  mask01 <- any(!is.na(offsets_raw))

  project_mask <- app(
    x = mask01,
    fun = function(x){
      ifelse(x == 0, NA_integer_, 1)
    }
  )

  names(project_mask) <- project_mask

  project_mask

}
