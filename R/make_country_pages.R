#' Draw one country's atlas pages from its country rasters
#'
#' Reads the four files write_country_rasters() wrote for the country and
#' draws, in `<outputdir>/<ISO3>/`:
#'
#' - the four maps for each species, via render_species_pages(), with the
#'   country border drawn over them and only the survey records inside the
#'   country on the points map
#' - a travel-time page, in hours
#'
#' Colour scales are per country, shared by every species within it:
#' abundance and CV run from 0 to the 99th percentile across species, rounded
#' up (abundance_limits()), with the top labelled ">="; probability is 0-1;
#' travel time is capped the same way.
#'
#' A country with no prediction cells (Cabo Verde, west of the prediction grid)
#' gets a single "outside the prediction area" page in place of the species
#' pages, then its travel-time page.
#'
#' Files are named `<ISO3>__<species>__<layer>.pdf`, `<ISO3>__traveltime.pdf`
#' and `<ISO3>__nodata.pdf`, which is what make_atlas_pdf() reads the atlas
#' structure from.
#'
#' @param country_files the four paths from write_country_rasters()
#' @param iso3 three-letter country code, matched to `boundaries$GID_0`
#' @param boundaries SpatVector of countries, from geodata::gadm(level = 0)
#' @param model_data_spatial the model data, for the survey records
#' @param outputdir root directory; one subdirectory per country
#' @param species species to draw; all by default
#' @param width,height page size in inches
#' @return paths of the pages written, in atlas order
#' @author geryan
#' @export
make_country_pages <- function(
    country_files,
    iso3,
    boundaries,
    model_data_spatial,
    outputdir,
    species = NULL,
    width = 8,
    height = 7
){

  files <- stats::setNames(
    country_files,
    tools::file_path_sans_ext(basename(country_files))
  )

  needed <- c(
    "abundance",
    "distribution",
    "cv",
    "traveltime"
  )

  if (!all(needed %in% names(files))) {
    stop(
      sprintf(
        "make_country_pages(): %s is missing %s",
        iso3,
        paste(setdiff(needed, names(files)), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  v <- boundaries[boundaries$GID_0 == iso3]

  outdir <- file.path(
    outputdir,
    iso3
  )

  dir.create(
    outdir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  abundance <- terra::rast(files[["abundance"]])
  distribution <- terra::rast(files[["distribution"]])
  cv <- terra::rast(files[["cv"]])

  if (is.null(species)) {
    species <- names(distribution)
  }

  has_predictions <- any(
    terra::global(
      distribution,
      fun = "notNA"
    )$notNA > 0
  )

  pages <- if (has_predictions) {

    render_species_pages(
      abundance = abundance,
      distribution = distribution,
      cv = cv,
      model_data_spatial = country_points(
        model_data_spatial,
        v
      ),
      outdir = outdir,
      limits = list(
        abundance = abundance_limits(abundance),
        cv = abundance_limits(cv)
      ),
      outline = v,
      prefix = iso3,
      species = species,
      width = width,
      height = height
    )

  } else {

    f <- file.path(
      outdir,
      paste0(iso3, "__nodata.pdf")
    )

    ggsave(
      filename = f,
      plot = ggplot() +
        annotate(
          "text",
          x = 0,
          y = 0,
          label = sprintf(
            "No predictions for %s:\nit lies outside the model's prediction area.",
            v$COUNTRY
          ),
          size = 6
        ) +
        theme_void(),
      width = width,
      height = height,
      units = "in",
      device = grDevices::cairo_pdf
    )

    f

  }

  # travel time, in hours, capped at the country's 99th percentile. The layer
  # was made country by country and does not cover some island states (Cabo
  # Verde, Seychelles), which get a note instead of an empty map
  tt <- terra::rast(files[["traveltime"]]) / 60
  names(tt) <- "travel_time_hours"

  has_traveltime <- terra::global(
    tt,
    fun = "notNA"
  )$notNA > 0

  cap <- abundance_limits(tt)[2]
  breaks <- pretty(c(0, cap))
  breaks <- breaks[breaks <= cap]

  p_tt <- if (!has_traveltime) {
    ggplot() +
      annotate(
        "text",
        x = 0,
        y = 0,
        label = sprintf(
          "No travel-time data for %s.",
          v$COUNTRY
        ),
        size = 6
      ) +
      theme_void()
  } else {
    ggplot() +
      tidyterra::geom_spatraster(
        data = tt
      ) +
      scale_fill_viridis_c(
        option = "mako",
        direction = -1,
        limits = c(0, cap),
        oob = scales::squish,
        breaks = breaks,
        labels = function(x){
          lab <- as.character(signif(x, 2))
          top <- !is.na(x) & x >= cap * (1 - 1e-9)
          lab[top] <- paste0("\u2265", lab[top])
          lab
        },
        na.value = "transparent",
        name = "Hours"
      ) +
      tidyterra::geom_spatvector(
        data = v,
        fill = NA,
        colour = "grey30",
        linewidth = 0.3
      ) +
      theme_void() +
      labs(
        title = "Travel time from research facilities"
      )
  }

  f_tt <- file.path(
    outdir,
    paste0(iso3, "__traveltime.pdf")
  )

  ggsave(
    filename = f_tt,
    plot = p_tt,
    width = width,
    height = height,
    units = "in",
    device = grDevices::cairo_pdf
  )

  c(
    pages,
    f_tt
  )

}

# records with a species that fall inside the country
country_points <- function(
    model_data_spatial,
    v
){

  e <- terra::ext(v)

  d <- model_data_spatial |>
    dplyr::filter(
      !is.na(species),
      longitude >= e$xmin,
      longitude <= e$xmax,
      latitude >= e$ymin,
      latitude <= e$ymax
    )

  if (nrow(d) == 0) {
    return(d)
  }

  pts <- terra::vect(
    as.data.frame(d[, c("longitude", "latitude")]),
    geom = c("longitude", "latitude"),
    crs = terra::crs(v)
  )

  d[terra::is.related(pts, v, "intersects"), ]

}
