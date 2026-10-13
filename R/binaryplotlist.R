#' Binary range maps, one ggplot per species
#'
#' Predicted present in navy and absent in light grey, the two ends of the
#' "va" scale the distribution maps use, with the maxSSS threshold in the
#' subtitle. Used for the per-species files (make_binary_range_plots()) and,
#' via its `plots` argument, the featured panels
#' (make_distribution_panels_featured()).
#'
#' @param pred_binary SpatRaster of 0 / 1, one layer per species
#' @param thresholds tibble from maxsss_thresholds()
#' @param subtitle "full" (threshold, sensitivity, specificity and cell
#'   counts), "short" (threshold only, for small panels) or "none"
#' @return named list of ggplots
#' @author geryan
#' @export
binaryplotlist <- function(
    pred_binary,
    thresholds,
    subtitle = c("full", "short", "none")
){

  subtitle <- match.arg(subtitle)

  sapply(
    X = names(pred_binary),
    FUN = function(sp){

      x <- terra::subset(pred_binary, sp)

      levels(x) <- data.frame(
        ID = c(0, 1),
        range = c("Absent", "Present")
      )

      th <- thresholds[thresholds$species == sp, ]

      sub <- switch(
        subtitle,
        full = sprintf(
          "maxSSS threshold %.3f: sensitivity %.2f, specificity %.2f (%d presence, %d absence cells)",
          th$threshold,
          th$sensitivity,
          th$specificity,
          th$presence_cells,
          th$absence_cells
        ),
        short = sprintf(
          "threshold %s",
          signif(th$threshold, 2)
        ),
        none = NULL
      )

      ggplot() +
        geom_spatraster(
          data = x
        ) +
        scale_fill_manual(
          values = c(
            Absent = grey(0.9),
            Present = "navy"
          ),
          na.value = "transparent",
          na.translate = FALSE,
          drop = FALSE,
          name = "Predicted\nrange"
        ) +
        theme_void() +
        labs(
          title = bquote(italic(.(paste0("Anopheles ", sp)))),
          subtitle = sub
        )

    },
    simplify = FALSE
  )

}
