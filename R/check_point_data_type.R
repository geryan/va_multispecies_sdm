#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param x
#' @return
#' @author geryan
#' @export
check_point_data_type <- function(x) {

  case_when(
    x == "admin unit centroid" ~ FALSE,
    x == "gps coords" ~ TRUE,
    x == "large polygon" ~ FALSE,
    x == "large polyon" ~ FALSE,
    x == "multi-point" ~ TRUE,
    x == "multiple points" ~ TRUE,
    x == "multiple-points" ~ TRUE,
    x == "point" ~ TRUE,
    x == "Point" ~ TRUE,
    x == "small polygon" ~ FALSE,
    x == "wide area" ~ TRUE,
    x == "within 5km" ~ FALSE,
    .default = TRUE
  )

}
