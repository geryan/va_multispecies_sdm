#' Panel figure of the model covariates, with the bioregions shown large
#'
#' One small map per covariate, all on one shared 0-1 colour scale, grey95 to
#' darkgreen -- the land cover fractions, the scaled human footprint and
#' proximity to sea all run 0-1 -- and a map of the One Earth bioregions twice
#' the width and height of the others, in the top-left corner, on the same grid
#' as the covariates. The bioregions the model has interaction terms for are
#' coloured; the rest are grey. The model uses smoothed (fractional) versions
#' of these bioregions, not the hard boundaries drawn.
#'
#' The small maps fill the `ncol - 2` cells beside the bioregion map, then rows
#' of `ncol` underneath. With 11 covariates, `ncol = 5` and `ncol = 3` fill the
#' grid exactly; `ncol = 4` leaves one cell, which `bar_in_gap` fills with the
#' colour bar.
#'
#' The figure is saved at `ncol * cell_size` inches wide (plus the legend, if
#' it is on the right), so to place it at its printed size -- with the text at
#' `base_size` points -- set `cell_size` to the text width over `ncol`.
#'
#' Writes `covariate_panels_<suffix>.png` into `plot_dir` and returns its path.
#'
#' @param covariates SpatRaster holding the covariate layers
#' @param covariate_names layers of `covariates` to draw, in order
#' @param bioregions SpatVector of One Earth bioregions, with a `bioregion`
#'   field
#' @param bioregion_names the bioregions in the model
#' @param plot_dir directory to write to
#' @param ncol number of columns of small maps
#' @param suffix names the file
#' @param legend_position "right" or "bottom"
#' @param cell_size width and height of each small map, in inches
#' @param base_size font size of the panel titles and legend text, in points;
#'   the bioregion title is 3 points larger and the legend titles 2
#' @param key_ncol columns in the bioregion key; by default 1 with the legend
#'   on the right and 2 with it at the bottom
#' @param bar_in_gap TRUE to draw the colour bar, upright, in the first empty
#'   cell of the grid rather than with the bioregion key
#' @param legend_space inches added for the legend: to the width with it on the
#'   right, to the height with it at the bottom. By default 2.75 and 3.75
#' @param labels named vector of panel titles; layers not in it keep their name
#' @param label_width bioregion names in the key are wrapped to this many
#'   characters
#' @param bioregion_chroma the bioregion colours' chroma is multiplied by
#'   this, and their lightness squeezed into 45-85, to soften the palette
#'   while keeping its hue and lightness differences; 1 leaves the chroma as is
#' @return path of the file written
#' @author geryan
#' @export
make_covariate_panels <- function(
    covariates,
    covariate_names,
    bioregions,
    bioregion_names,
    plot_dir = "outputs/figures",
    ncol = 5,
    suffix = "landscape",
    legend_position = c("right", "bottom"),
    cell_size = 2.5,
    base_size = 9,
    key_ncol = NULL,
    bar_in_gap = FALSE,
    legend_space = NULL,
    labels = covariate_labels(),
    label_width = 28,
    bioregion_chroma = 0.5
){

  legend_position <- match.arg(legend_position)

  if (ncol < 3) {
    stop(
      "make_covariate_panels(): ncol must be at least 3",
      call. = FALSE
    )
  }

  if (is.null(key_ncol)) {
    key_ncol <- if (legend_position == "bottom") 2 else 1
  }

  if (is.null(legend_space)) {
    legend_space <- if (legend_position == "right") 2.75 else 3.75
  }

  if (!dir.exists(plot_dir)) {
    dir.create(
      plot_dir,
      recursive = TRUE
    )
  }

  title <- function(nm){
    if (nm %in% names(labels)) labels[[nm]] else nm
  }

  # sizes scale with base_size, so a figure saved at its printed size has
  # print-sized text
  legend_title <- element_text(size = base_size + 2)
  legend_text <- element_text(size = base_size)

  colour_bar <- function(direction, length){
    guide_colourbar(
      order = 2,
      direction = direction,
      theme = if (direction == "horizontal") {
        theme(
          legend.key.width = unit(length, "in")
        )
      } else {
        theme(
          legend.key.height = unit(length, "in")
        )
      }
    )
  }

  small_map <- function(nm, guide){
    ggplot() +
      tidyterra::geom_spatraster(
        data = covariates[[nm]]
      ) +
      scale_fill_gradient(
        low = "grey95",
        high = "darkgreen",
        limits = c(0, 1),
        na.value = "transparent",
        name = "Value",
        guide = guide
      ) +
      theme_void() +
      theme(
        plot.title = element_text(size = base_size)
      ) +
      labs(
        title = title(nm)
      )
  }

  # landscape: the colour bar runs horizontally under the bioregion key, both
  # to the right of the maps; portrait: it stands upright beside the key, both
  # under the maps -- or, with bar_in_gap, in the grid's empty cell
  small <- lapply(
    covariate_names,
    small_map,
    guide = if (bar_in_gap) {
      "none"
    } else if (legend_position == "right") {
      colour_bar(
        "horizontal",
        0.64 * cell_size
      )
    } else {
      colour_bar(
        "vertical",
        0.48 * cell_size
      )
    }
  )

  modelled <- sort(bioregion_names)
  not_modelled <- "Not in model"

  # less its two greys, which would read as "Not in model", and one of its
  # three greens, which with alphabetical assignment would put three greens
  # side by side in central Africa
  pal <- setdiff(
    grDevices::palette.colors(
      palette = "Alphabet"
    ),
    c(
      "#565656",
      "#E2E2E2",
      "#1CBE4F"
    )
  )

  if (length(modelled) > length(pal)) {
    stop(
      sprintf(
        "make_covariate_panels(): %d bioregions but only %d colours",
        length(modelled),
        length(pal)
      ),
      call. = FALSE
    )
  }

  cols <- c(
    stats::setNames(
      soften_colours(
        unname(pal[seq_along(modelled)]),
        chroma = bioregion_chroma
      ),
      modelled
    ),
    stats::setNames(
      "grey85",
      not_modelled
    )
  )

  bioregions$bioregion_plot <- ifelse(
    bioregions$bioregion %in% modelled,
    bioregions$bioregion,
    not_modelled
  )

  # drawn as a raster on the covariate grid and masked to it, not as the
  # polygons, which run out to sea around the coast and the islands
  land <- covariates[[covariate_names[1]]]

  bioregion_rast <- terra::rasterize(
    bioregions,
    land,
    field = "bioregion_plot"
  ) |>
    terra::mask(
      mask = land
    )

  big <- ggplot() +
    tidyterra::geom_spatraster(
      data = bioregion_rast
    ) +
    scale_fill_manual(
      values = cols,
      breaks = names(cols),
      labels = scales::label_wrap(label_width),
      na.value = "transparent",
      na.translate = FALSE,
      name = "Bioregion"
    ) +
    guides(
      fill = guide_legend(
        order = 1,
        ncol = key_ncol
      )
    ) +
    theme_void() +
    theme(
      plot.title = element_text(size = base_size + 3),
      legend.key.size = unit(0.4 * base_size / 9, "cm")
    ) +
    labs(
      title = "Bioregions"
    )

  # the layout as a patchwork design: "A" is the bioregion map, 2 x 2 cells in
  # the top-left; the small maps fill the cells beside it and then the rows
  # below, and with bar_in_gap the colour bar takes the first cell left over.
  # Upper case only, because patchwork orders areas by sort(), and a
  # locale-aware sort interleaves "a" with "A"
  n_small <- length(small)

  if (n_small + 2 > length(LETTERS)) {
    stop(
      "make_covariate_panels(): more than 24 covariates",
      call. = FALSE
    )
  }

  area <- LETTERS[seq_len(n_small) + 1]
  n_beside <- 2 * (ncol - 2)
  n_below_rows <- ceiling(max(n_small - n_beside, 0) / ncol)
  n_empty <- n_beside + n_below_rows * ncol - n_small

  if (bar_in_gap && n_empty == 0) {
    stop(
      sprintf(
        "make_covariate_panels(): bar_in_gap, but %d covariates fill %d columns exactly",
        n_small,
        ncol
      ),
      call. = FALSE
    )
  }

  cells <- c(
    area,
    if (bar_in_gap) LETTERS[n_small + 2],
    rep(
      "#",
      n_empty - bar_in_gap
    )
  )

  beside <- cells[seq_len(n_beside)]
  below <- cells[-seq_len(n_beside)]

  rows <- c(
    paste(
      c(
        "AA",
        beside[seq_len(ncol - 2)]
      ),
      collapse = ""
    ),
    paste(
      c(
        "AA",
        beside[ncol - 2 + seq_len(ncol - 2)]
      ),
      collapse = ""
    ),
    vapply(
      split(
        below,
        ceiling(seq_along(below) / ncol)
      ),
      paste,
      character(1),
      collapse = ""
    )
  )

  design <- paste(
    rows,
    collapse = "\n"
  )

  # the colour bar on its own, as a plot of the gradient with its scale on
  # the right, wrapped into the panel area of its cell so it sits centred
  # where a map would be. Not `full =`: in the bottom row the full area runs
  # on down into the collected legend
  bar <- if (bar_in_gap) {

    breaks <- seq(
      0,
      1,
      by = 0.25
    )

    bar_plot <- ggplot(
      data.frame(
        value = seq(
          0,
          1,
          length.out = 200
        )
      ),
      aes(
        x = 0,
        y = value,
        fill = value
      )
    ) +
      geom_raster() +
      scale_fill_gradient(
        low = "grey95",
        high = "darkgreen",
        limits = c(0, 1),
        guide = "none"
      ) +
      scale_x_continuous(
        expand = c(0, 0)
      ) +
      scale_y_continuous(
        breaks = breaks,
        labels = format(breaks),
        position = "right",
        expand = c(0, 0)
      ) +
      theme_void() +
      theme(
        aspect.ratio = 8,
        plot.title = element_text(
          size = base_size + 2,
          margin = margin(b = 6)
        ),
        axis.text.y.right = element_text(
          size = base_size,
          margin = margin(l = 2)
        ),
        axis.ticks.y.right = element_line(
          colour = "white",
          linewidth = 0.3
        ),
        axis.ticks.length.y.right = unit(-0.05, "in"),
        plot.margin = margin(
          0.06 * cell_size,
          0,
          0.06 * cell_size,
          0,
          unit = "in"
        )
      ) +
      labs(
        title = "Value"
      )

    list(
      patchwork::wrap_elements(
        panel = bar_plot
      )
    )

  }

  p <- patchwork::wrap_plots(
    c(
      list(big),
      small,
      bar
    ),
    design = design,
    guides = "collect"
  ) &
    theme(
      legend.position = legend_position,
      legend.box = if (legend_position == "right") "vertical" else "horizontal",
      legend.box.just = "left",
      legend.title.position = "top",
      legend.title = legend_title,
      legend.text = legend_text,
      legend.key.spacing.y = unit(4 * base_size / 9, "pt")
    )

  # a margin round the whole figure, so a legend along the bottom does not sit
  # on the edge
  p <- p +
    patchwork::plot_annotation(
      theme = theme(
        plot.margin = margin(
          10,
          10,
          20,
          10
        )
      )
    )

  file <- file.path(
    plot_dir,
    sprintf(
      "covariate_panels_%s.png",
      suffix
    )
  )

  ggsave(
    filename = file,
    plot = p,
    width = ncol * cell_size +
      if (legend_position == "right") legend_space else 0,
    height = length(rows) * cell_size +
      if (legend_position == "bottom") legend_space else 0,
    dpi = 300,
    units = "in",
    bg = "white"
  )

  file

}

# lower the chroma of a palette by a factor and squeeze its lightness into
# `lightness`, in HCL space, so it reads softer while keeping the differences
# in hue, and the relative differences in chroma and lightness, that tell the
# colours apart
soften_colours <- function(
    x,
    chroma = 0.5,
    lightness = c(45, 85)
){

  hcl <- farver::convert_colour(
    farver::decode_colour(x),
    from = "rgb",
    to = "hcl"
  )

  l <- hcl[, "l"]

  hcl[, "c"] <- hcl[, "c"] * chroma
  hcl[, "l"] <- lightness[1] +
    (l - min(l)) / diff(range(l)) * diff(lightness)

  farver::encode_colour(
    hcl,
    from = "hcl"
  )

}
