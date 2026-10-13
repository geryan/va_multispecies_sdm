#' The species atlas pages, on continental scales
#'
#' render_species_pages() with the continental colour scales: abundance shared
#' across species (abundance_limits()), CV 0-1. A function of its own so the
#' same pages can make the species atlas and open the country atlas
#' (make_atlas_pdf(continental_pages = )) without being drawn twice.
#'
#' @param abundance,distribution,cv SpatRaster, one layer per species
#' @param model_data_spatial the model data, for the survey records
#' @param outdir directory to write the pages to
#' @param species species to include; all by default
#' @return paths of the pages written, species by species
#' @author geryan
#' @export
render_species_atlas_pages <- function(
    abundance,
    distribution,
    cv,
    model_data_spatial,
    outdir,
    species = names(distribution)
){

  render_species_pages(
    abundance = abundance,
    distribution = distribution,
    cv = cv,
    model_data_spatial = model_data_spatial,
    outdir = outdir,
    limits = list(
      abundance = abundance_limits(abundance),
      cv = c(0, 1)
    ),
    species = species
  )

}
