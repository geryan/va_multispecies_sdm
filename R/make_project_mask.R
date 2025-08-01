#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param covariate_rast
#' @return
#' @author geryan
#' @export
make_project_mask <- function(covariate_rast) {

  r <- app(
    x = covariate_rast[[1]],
    fun = function(x){
      ifelse(!is.na(x), 1, NA)
    }
  )

  names(r) <- "project_mask"

  r

}
