#' Panel figure with a few featured species shown large
#'
#' A variant of make_distribution_panels(). The `featured` species get large
#' maps at the top, twice the width and height of the others, laid out
#' `featured_per_row` to a row. The remaining species fill rows of
#' 2 x featured_per_row small maps underneath, and the colour bar sits in the
#' empty cells at the end of the last row -- or to the right, if the last row
#' is full. Panels come from distplotlist(), so each figure has the scale shared
#' by every species, as in the other figures.
#'
#' With the four default species: `featured_per_row = 4` (the default, all in
#' one row) is landscape, the large maps across the top half over rows of 8;
#' `featured_per_row = 2` is portrait, the large maps 2 x 2 in the top half over
#' rows of 4.
#'
#' Writes `panel_<prefix>_<suffix>.png` into `plot_dir` and returns its path.
#'
#' @param pred_dist SpatRaster, one layer per species
#' @param plot_dir directory to write to
#' @param colscheme,guide,log_scale passed to distplotlist()
#' @param prefix names the file, as in make_distribution_panels()
#' @param featured species to show large, left to right then top to bottom
#' @param featured_per_row how many large maps to a row
#' @param suffix names the file, after `prefix`
#' @param small_size width and height of each small map, in inches
#' @return path of the file written
#' @author geryan
#' @export
make_distribution_panels_featured <- function(
    pred_dist,
    plot_dir = "outputs/figures/distribution_plots/",
    colscheme = "va",
    guide = c("prob", "abundance", "cv"),
    prefix = "distribution",
    featured = c(
      "arabiensis",
      "coluzzii",
      "funestus",
      "gambiae"
    ),
    featured_per_row = length(featured),
    suffix = "featured",
    small_size = 2,
    log_scale = FALSE
) {

  guide <- match.arg(guide)

  not_found <- setdiff(featured, names(pred_dist))

  if (length(not_found)) {
    stop(
      sprintf(
        "make_distribution_panels_featured(): not in pred_dist: %s",
        paste(not_found, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  if (!dir.exists(plot_dir)) {
    dir.create(
      plot_dir,
      recursive = TRUE
    )
  }

  plots <- distplotlist(
    pred_dist,
    colscheme = colscheme,
    guide = guide,
    log_scale = log_scale
  )

  others <- setdiff(names(plots), featured)

  n_featured <- length(featured)
  ncol <- 2 * featured_per_row
  n_featured_rows <- ceiling(n_featured / featured_per_row)
  n_small_rows <- ceiling(length(others) / ncol)
  n_empty <- n_small_rows * ncol - length(others)

  # the layout as a patchwork design: one letter per map, the featured maps
  # 2 x 2 cells each, then the small maps, then the colour bar in whatever the
  # last row leaves empty. Upper case only, because patchwork orders areas by
  # sort(), and a locale-aware sort interleaves "a" with "A"
  n_areas <- n_featured + length(others) + (n_empty > 0)

  if (n_areas > length(LETTERS)) {
    stop(
      "make_distribution_panels_featured(): more than 26 panels",
      call. = FALSE
    )
  }

  area <- LETTERS[seq_len(n_areas)]

  # each row of featured maps is written twice, as they are two cells tall; a
  # short last row is padded with empty cells ("#")
  featured_rows <- vapply(
    split(
      area[seq_len(n_featured)],
      ceiling(seq_len(n_featured) / featured_per_row)
    ),
    function(a){
      paste(
        c(
          rep(
            a,
            each = 2
          ),
          rep(
            "#",
            ncol - 2 * length(a)
          )
        ),
        collapse = ""
      )
    },
    character(1)
  )

  small_cells <- c(
    area[n_featured + seq_along(others)],
    rep(
      area[n_areas],
      n_empty
    )
  )

  small_rows <- vapply(
    split(
      small_cells,
      ceiling(seq_along(small_cells) / ncol)
    ),
    paste,
    character(1),
    collapse = ""
  )

  design <- paste(
    c(
      rep(
        featured_rows,
        each = 2
      ),
      small_rows
    ),
    collapse = "\n"
  )

  # titles sized to the maps, so the longest name (quadriannulatus) fits a
  # small one
  panels <- c(
    lapply(
      plots[featured],
      function(p){
        p +
          theme(
            plot.title = element_text(size = 12)
          )
      }
    ),
    lapply(
      plots[others],
      function(p){
        p +
          theme(
            plot.title = element_text(size = 8)
          )
      }
    )
  )

  if (n_empty > 0) {
    panels <- c(
      panels,
      list(
        patchwork::guide_area()
      )
    )
  }

  p <- patchwork::wrap_plots(
    panels,
    design = design,
    guides = "collect"
  )

  file <- file.path(
    plot_dir,
    sprintf(
      "panel_%s_%s.png",
      prefix,
      suffix
    )
  )

  ggsave(
    filename = file,
    plot = p,
    width = ncol * small_size + if (n_empty > 0) 0 else 1.5,
    height = (2 * n_featured_rows + n_small_rows) * small_size,
    dpi = 300,
    units = "in",
    bg = "white"
  )

  file

}
