#' Atlas PDF for one country
#'
#' Binds one country's pages from make_country_pages() into an atlas of their
#' own with make_atlas_pdf(): a title page naming the country, contents
#' listing its species, then the four maps per species and travel time, on
#' the country's colour scales. Written as `<outputdir>/<ISO3>_<Country>.pdf`,
#' with the name made file-safe (Côte d'Ivoire -> CIV_Cote_d_Ivoire.pdf).
#'
#' @param pages the country's page paths, from make_country_pages()
#' @param iso3 three-letter country code
#' @param countries tibble with `iso3` and `country`
#' @param outputdir directory to write to
#' @param subtitle for the title page
#' @return path of the atlas written
#' @author geryan
#' @export
make_single_country_atlas <- function(
    pages,
    iso3,
    countries,
    outputdir,
    subtitle = NULL
){

  country <- countries$country[countries$iso3 == iso3]

  if (length(country) != 1) {
    stop(
      sprintf(
        "make_single_country_atlas(): %s is not in `countries` exactly once",
        iso3
      ),
      call. = FALSE
    )
  }

  others <- !startsWith(
    basename(pages),
    paste0(iso3, "__")
  )

  if (any(others)) {
    stop(
      sprintf(
        "make_single_country_atlas(): pages not from %s: %s",
        iso3,
        paste(basename(pages)[others], collapse = ", ")
      ),
      call. = FALSE
    )
  }

  file_name <- stringi::stri_trans_general(
    country,
    "Latin-ASCII"
  ) |>
    gsub(
      pattern = "[^A-Za-z0-9]+",
      replacement = "_"
    ) |>
    gsub(
      pattern = "^_|_$",
      replacement = ""
    )

  make_atlas_pdf(
    pages = pages,
    file = file.path(
      outputdir,
      sprintf(
        "%s_%s.pdf",
        iso3,
        file_name
      )
    ),
    type = "country",
    title = sprintf(
      "Predicted distribution and abundance of Anopheles species: %s",
      country
    ),
    subtitle = subtitle,
    countries = countries,
    toc_depth = 2
  )

}
