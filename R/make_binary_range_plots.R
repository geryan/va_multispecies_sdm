#' Write the binary range map of each species, and the thresholds used
#'
#' The binary counterpart of make_distribution_plots(): one
#' `<prefix>_<species>.png` per species, the same size as the other
#' per-species figures, plus `<prefix>_thresholds_maxsss.csv` with each
#' species' threshold, sensitivity, specificity and cell counts.
#'
#' @param pred_binary SpatRaster of 0 / 1, one layer per species
#' @param thresholds tibble from maxsss_thresholds()
#' @param plot_dir directory to write to
#' @param prefix start of each file name
#' @return paths of the files written
#' @author geryan
#' @export
make_binary_range_plots <- function(
    pred_binary,
    thresholds,
    plot_dir,
    prefix = "binary"
){

  dir.create(
    plot_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  plots <- binaryplotlist(
    pred_binary,
    thresholds,
    subtitle = "full"
  )

  files <- file.path(
    plot_dir,
    sprintf(
      "%s_%s.png",
      prefix,
      names(plots)
    )
  )

  for (i in seq_along(plots)) {
    ggsave(
      filename = files[i],
      plot = plots[[i]],
      width = 3200,
      height = 3200,
      dpi = 300,
      units = "px",
      bg = "white"
    )
  }

  csv <- file.path(
    plot_dir,
    sprintf(
      "%s_thresholds_maxsss.csv",
      prefix
    )
  )

  readr::write_csv(
    thresholds,
    csv
  )

  c(
    files,
    csv
  )

}
