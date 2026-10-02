#' Shrink a raster that is larger than a printed map needs
#'
#' A raster with a long side over `target_px` cells, such as the ~1 km travel
#' time for a large country, is averaged down to fit. Left alone, tidyterra
#' thins anything over 500,000 cells by dropping rows and columns. Anything
#' smaller is returned as it is. Draw the result with
#' geom_spatraster(maxcell = Inf), so tidyterra does not thin it again.
#'
#' @param r SpatRaster
#' @param target_px longest side to allow, in cells
#' @return SpatRaster
#' @author geryan
#' @export
raster_for_pdf <- function(
    r,
    target_px = 1500
){

  long_side <- max(dim(r)[1:2])

  if (long_side <= target_px) {
    return(r)
  }

  terra::aggregate(
    r,
    fact = ceiling(long_side / target_px),
    fun = "mean",
    na.rm = TRUE
  )

}

#' Enlarge a plot's map images so they print sharply in a PDF
#'
#' A raster map goes into a PDF as an image with one pixel per cell. A small
#' country at 10 km is only ~25 pixels across, stretched over the page, and
#' PDF viewers (macOS Preview, browsers) smooth small images whatever the PDF's
#' no-interpolation flag says, so every cell blurs into its neighbours.
#'
#' This builds the plot, then repeats each pixel of every raster image in its
#' panels as a block of identical pixels, so the long side is about
#' `target_px`. The image covers the same area, the colours are unchanged, and
#' the viewer's smoothing is confined to the cell edges. Doing it on the built
#' image, rather than by splitting the raster's cells before plotting, keeps
#' ggplot from handling millions of duplicated cells. The legend's colour bar
#' is left alone.
#'
#' @param p ggplot
#' @param target_px long side to aim for, in pixels; images already at least
#'   half this are left as they are
#' @return gtable, for ggsave()
#' @author geryan
#' @export
sharpen_raster_grobs <- function(
    p,
    target_px = 2000
){

  enlarge <- function(grob){

    if (inherits(grob, "rastergrob")) {

      m <- as.matrix(grob$raster)
      fact <- floor(target_px / max(dim(m)))

      if (fact >= 2) {
        grob$raster <- grDevices::as.raster(
          m[
            rep(seq_len(nrow(m)), each = fact),
            rep(seq_len(ncol(m)), each = fact),
            drop = FALSE
          ]
        )
      }

      return(grob)

    }

    if (inherits(grob, "gTree")) {
      grob$children[] <- lapply(
        grob$children,
        enlarge
      )
    }

    grob

  }

  g <- ggplotGrob(p)

  panels <- grep(
    "^panel",
    g$layout$name
  )

  g$grobs[panels] <- lapply(
    g$grobs[panels],
    enlarge
  )

  g

}
