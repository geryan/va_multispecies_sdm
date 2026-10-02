#' Species atlas: every species' four maps, one per page, in one PDF
#'
#' Draws each species' abundance, distribution, CV and survey-points maps with
#' render_species_pages(), on the same continental scales as the existing
#' figures -- abundance shared across species (abundance_limits()), CV and
#' probability 0-1 -- then binds them with make_atlas_pdf(). The page files
#' are kept beside the atlas, in `<file without .pdf>_pages/`.
#'
#' @param abundance,distribution,cv SpatRaster, one layer per species
#' @param model_data_spatial the model data, for the survey records
#' @param file path of the atlas to write
#' @param title,subtitle for the title page
#' @param species species to include; all by default
#' @return `file`
#' @author geryan
#' @export
make_species_atlas <- function(
    abundance,
    distribution,
    cv,
    model_data_spatial,
    file,
    title = "Predicted distribution and abundance of Anopheles species in Africa",
    subtitle = NULL,
    species = names(distribution)
){

  pages <- render_species_atlas_pages(
    abundance = abundance,
    distribution = distribution,
    cv = cv,
    model_data_spatial = model_data_spatial,
    outdir = sub("\\.pdf$", "_pages", file),
    species = species
  )

  make_atlas_pdf(
    pages = pages,
    file = file,
    type = "species",
    title = title,
    subtitle = subtitle
  )

}

#' The species atlas pages, on continental scales
#'
#' render_species_pages() with the continental colour scales: abundance shared
#' across species (abundance_limits()), CV 0-1. Split out of
#' make_species_atlas() so the same pages can also open the country atlas
#' (make_atlas_pdf(continental_pages = )) without being drawn twice.
#'
#' @inheritParams make_species_atlas
#' @param outdir directory to write the pages to
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
