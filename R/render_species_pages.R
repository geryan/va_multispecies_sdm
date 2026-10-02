#' Draw the four atlas maps for each species, one PDF file per map
#'
#' Shared by the species atlas (continental) and the country atlas (one call
#' per country). For each species, in this order:
#'
#' - abundance: distplot(colscheme = "orchid", guide = "abundance")
#' - distribution: distplot(colscheme = "va", guide = "prob"), 0-1
#' - cv: distplot(colscheme = "brick", guide = "cv")
#' - points: the distribution map with the species' survey records, as
#'   add_pa_points_list() draws them for the distpoints_*.png figures
#'
#' which are the same maps, colours and legends as the existing figures. Every
#' cell is drawn (tidyterra would otherwise thin the continental maps), and the
#' map image is enlarged with sharpen_raster_grobs() so small countries print
#' as sharp cells rather than a blur. Files
#' are named `<prefix>__<species>__<layer>.pdf` (without `<prefix>__` if it is
#' NULL), which is what make_atlas_pdf() reads the atlas structure from.
#'
#' @param abundance,distribution,cv SpatRaster, one layer per species
#' @param model_data_spatial records to draw on the points map
#' @param outdir directory to write the pages to
#' @param limits list of `abundance` and `cv` colour-scale limits, shared by
#'   every species drawn. NULL for either scales each species separately
#' @param outline optional SpatVector drawn over each map, e.g. a country border
#' @param prefix optional first part of each file name, e.g. a country code
#' @param species species to draw; all layers of `distribution` by default
#' @param width,height page size in inches
#' @return paths of the pages written, species by species
#' @author geryan
#' @export
render_species_pages <- function(
    abundance,
    distribution,
    cv,
    model_data_spatial,
    outdir,
    limits = list(
      abundance = NULL,
      cv = c(0, 1)
    ),
    outline = NULL,
    prefix = NULL,
    species = names(distribution),
    width = 8,
    height = 7
){

  dir.create(
    outdir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  outline_layer <- if (is.null(outline)) {
    NULL
  } else {
    tidyterra::geom_spatvector(
      data = outline,
      fill = NA,
      colour = "grey30",
      linewidth = 0.3
    )
  }

  save_page <- function(p, sp, layer){

    f <- file.path(
      outdir,
      paste0(
        paste(c(prefix, sp, layer), collapse = "__"),
        ".pdf"
      )
    )

    ggsave(
      filename = f,
      plot = sharpen_raster_grobs(
        p + outline_layer
      ),
      width = width,
      height = height,
      units = "in",
      device = grDevices::cairo_pdf
    )

    f

  }

  unlist(
    lapply(
      species,
      function(sp){

        p_dist <- distplot(
          distribution,
          sp,
          colscheme = "va",
          guide = "prob",
          limits = c(0, 1),
          maxcell = Inf
        )

        p_points <- add_pa_points_list(
          stats::setNames(list(p_dist), sp),
          model_data_spatial,
          colp = "cadetblue2",
          cola = "yellow"
        )[[sp]]

        c(
          save_page(
            distplot(
              abundance,
              sp,
              colscheme = "orchid",
              guide = "abundance",
              limits = limits$abundance,
              maxcell = Inf
            ),
            sp,
            "abundance"
          ),
          save_page(
            p_dist,
            sp,
            "distribution"
          ),
          save_page(
            distplot(
              cv,
              sp,
              colscheme = "brick",
              guide = "cv",
              limits = limits$cv,
              maxcell = Inf
            ),
            sp,
            "cv"
          ),
          save_page(
            p_points,
            sp,
            "points"
          )
        )

      }
    )
  )

}
