#' .. content for \description{} (no empty lines) ..
#'
#' .. content for \details{} ..
#'
#' @title
#' @param species_id_1
#' @param species_id_2
#' @return
#' @author geryan
#' @export
check_genetic_id <- function(
    species_id_1,
    species_id_2

  ) {

  any(
    c(
      species_id_1,
      species_id_2
    ) %in%
      c(
        "DNA",
        "genetic_molecular",
        "genetic/molecular"
      )
  )

}
