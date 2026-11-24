# Unique sources per country, by region, with every country's bar the same
# width
#
# Alternatives to a figure made outside this repo, which drew the five regions
# as panels of equal height, so a country's bar was thinner the more countries
# its region had. Here each panel's height is its number of countries times
# one fixed slot, `slot_cm`, set through ggplot2's `panel.heights` (>= 3.5.0).
# A country gets the same slot in every panel, column and version. The script
# measures the built plots and stops if any slot differs.
#
# Counts are distinct `source_id` per `country` in the stored `raw_data`
# target. Regions are the UN M49 sub-regions, with Middle Africa called
# Central, as in the original. On the log axis a country with one source has a
# bar of zero length, as in the original.
#
# Writes to outputs/figures/sources_by_country/:
#   sources_1col.png         one column
#   sources_2col.png         Eastern + Central | Western + Northern + Southern
#   sources_2col_colour.png  the same, bars coloured by region
#   sources_3col.png         Eastern | Western | Central + Northern + Southern
#   sources_colour.png       one panel, each region a coloured block under its
#                            name
#   sources_vertical.png     vertical bars, the regions side by side; 17.9 cm
#                            wide at a 0.3 cm slot, so a landscape option
#   sources_by_country.csv   the counts plotted
#
# Run from the project root: Rscript extras/plot_sources_by_country.R

library(targets)
library(dplyr)
library(stringr)
library(readr)
library(ggplot2)
library(countrycode)
library(gtable)
library(grid)

# wehere plots are saved to
plot_dir <- "outputs/figures/sources_by_country"

# each country's share of the axis, in cm, in every version
slot_cm <- 0.3

# 7 pt text at printed size, as the manuscript figures
base_size <- 7

dpi <- 600

# Checked with the dataviz palette validator. Regions next to each other pass
# the colour-vision-deficiency and normal-vision checks; not every pair does,
# so these are only used with each region's countries kept together and named.
region_colours <- c(
  Central = "#2a78d6",
  Eastern = "#eb6834",
  Northern = "#1baf7a",
  Southern = "#eda100",
  Western = "#e87ba4"
)

dir.create(
  plot_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# text is measured on the device ggsave() will draw on
measuring_file <- tempfile(fileext = ".png")

ragg::agg_png(
  measuring_file,
  width = 10,
  height = 10,
  units = "cm",
  res = dpi
)


# data -------------------------------------------------------------------------

sources <- tar_read(raw_data) |>
  distinct(
    country,
    source_id
  ) |>
  count(
    country,
    name = "n_sources"
  ) |>
  mutate(
    label = countrycode(
      country,
      origin = "country.name",
      destination = "cldr.short.en",
      custom_match = c("cabo verde" = "Cabo Verde")
    ),
    # the intermediate region is NA for Northern Africa, which is a sub-region
    region = coalesce(
      countrycode(
        country,
        origin = "country.name",
        destination = "un.regionintermediate.name",
        warn = FALSE
      ),
      countrycode(
        country,
        origin = "country.name",
        destination = "un.regionsub.name"
      )
    ) |>
      str_replace(
        "Middle Africa",
        "Central Africa"
      ) |>
      str_remove(" Africa$")
  )

stopifnot(
  !anyNA(sources$label),
  all(sources$region %in% names(region_colours))
)

# most sources at the top of each region (a discrete axis draws its first level
# at the bottom)
sources <- sources |>
  arrange(
    region,
    desc(n_sources),
    label
  ) |>
  mutate(
    region = factor(
      region,
      levels = names(region_colours)
    ),
    label = factor(
      label,
      levels = rev(label)
    )
  )

write_csv(
  sources |>
    select(
      region,
      country = label,
      raw_data_country = country,
      n_sources
    ),
  file.path(
    plot_dir,
    "sources_by_country.csv"
  )
)


# shared pieces -----------------------------------------------------------------

# the same log axis everywhere, starting at 1 so the bars start there
sources_axis <- function(scale_fun = scale_x_log10) {
  scale_fun(
    breaks = c(
      1,
      10,
      100
    ),
    limits = c(
      1,
      max(sources$n_sources)
    ),
    expand = expansion(
      mult = c(
        0,
        0.05
      )
    )
  )
}

# The default padding on a discrete axis is 0.6 of a slot at each end, so a
# panel of n countries spans n + 0.2 slots and a short panel's bars come out
# wider. Half a slot at each end makes it n exactly.
slot_expand <- expansion(add = 0.5)

# all text at base_size: theme_minimal() shrinks tick labels, strips and legend
# text to 0.8 of it
theme_sources <- theme_minimal(base_size = base_size) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = rel(1)),
    legend.text = element_text(size = rel(1)),
    strip.text = element_text(
      face = "bold",
      size = rel(1)
    ),
    panel.spacing = unit(
      slot_cm,
      "cm"
    )
  )

grob_cm <- function(g) {
  c(
    width = convertWidth(
      sum(g$widths),
      "cm",
      valueOnly = TRUE
    ),
    height = convertHeight(
      sum(g$heights),
      "cm",
      valueOnly = TRUE
    )
  )
}

# Builds once with 1 cm panels to measure everything that isn't panel (labels,
# strips, margins), then again with the panel width that brings the total to
# `width_cm`. `build` takes a panel width in cm and returns a ggplot.
fit_to_width <- function(
    build,
    width_cm
) {
  trial <- ggplotGrob(build(1))

  ggplotGrob(
    build(width_cm - (grob_cm(trial)[["width"]] - 1))
  )
}

# each panel's size along the countries' axis, divided by its number of
# countries
slot_sizes <- function(
    g,
    n_per_panel,
    along,
    version
) {
  panels <- g$layout |>
    filter(
      str_detect(
        name,
        "^panel"
      )
    ) |>
    arrange(
      t,
      l
    )

  size <- if (along == "height") {
    convertHeight(
      g$heights[panels$t],
      "cm",
      valueOnly = TRUE
    )
  } else {
    convertWidth(
      g$widths[panels$l],
      "cm",
      valueOnly = TRUE
    )
  }

  tibble(
    version = version,
    panel = seq_along(size),
    n = n_per_panel,
    slot_cm = size / n_per_panel
  )
}

save_grob <- function(
    g,
    file
) {
  size <- grob_cm(g)

  ggsave(
    file.path(
      plot_dir,
      file
    ),
    plot = g,
    width = size[["width"]],
    height = size[["height"]],
    units = "cm",
    dpi = dpi,
    bg = "white"
  )

  size
}


# horizontal bars in columns of stacked regions ---------------------------------

# one column: the regions stacked, each panel as tall as its countries
plot_column <- function(
    regions,
    panel_width_cm,
    fill_by_region = FALSE
) {
  data <- sources |>
    filter(region %in% regions) |>
    mutate(
      region = factor(
        region,
        levels = regions
      )
    )

  n_per_panel <- data |>
    count(region) |>
    pull(n)

  bars <- if (fill_by_region) {
    list(
      geom_col(
        aes(fill = region),
        width = 0.8,
        show.legend = FALSE
      ),
      scale_fill_manual(values = region_colours)
    )
  } else {
    geom_col(
      fill = "grey35",
      width = 0.8
    )
  }

  ggplot(
    data,
    aes(
      x = n_sources,
      y = label
    )
  ) +
    bars +
    facet_wrap(
      vars(region),
      ncol = 1,
      scales = "free_y"
    ) +
    sources_axis() +
    scale_y_discrete(expand = slot_expand) +
    labs(
      x = "Unique sources",
      y = NULL
    ) +
    theme_sources +
    theme(
      panel.heights = unit(
        n_per_panel * slot_cm,
        "cm"
      ),
      panel.widths = unit(
        panel_width_cm,
        "cm"
      )
    )
}

# Columns side by side, top-aligned, sharing `width_cm` so their panels are
# equally wide. Returns the combined figure and the slot check.
plot_columns <- function(
    columns,
    width_cm,
    version,
    fill_by_region = FALSE
) {
  trial <- lapply(
    columns,
    \(regions) ggplotGrob(
      plot_column(
        regions,
        panel_width_cm = 1,
        fill_by_region = fill_by_region
      )
    )
  )

  not_panel_cm <- sapply(
    trial,
    \(g) grob_cm(g)[["width"]] - 1
  )

  panel_width_cm <- (width_cm - sum(not_panel_cm)) / length(columns)

  grobs <- lapply(
    columns,
    \(regions) ggplotGrob(
      plot_column(
        regions,
        panel_width_cm = panel_width_cm,
        fill_by_region = fill_by_region
      )
    )
  )

  check <- Map(
    \(g, regions) slot_sizes(
      g,
      n_per_panel = sources |>
        filter(region %in% regions) |>
        mutate(
          region = factor(
            region,
            levels = regions
          )
        ) |>
        count(region) |>
        pull(n),
      along = "height",
      version = version
    ),
    grobs,
    columns
  ) |>
    bind_rows()

  sizes <- sapply(
    grobs,
    grob_cm
  )

  # a stretchy row at the bottom of each column takes up its spare height, so
  # shorter columns sit at the top
  grobs <- lapply(
    grobs,
    \(g) gtable_add_rows(
      g,
      unit(
        1,
        "null"
      )
    )
  )

  figure <- gtable(
    widths = unit(
      sizes["width", ],
      "cm"
    ),
    heights = unit(
      max(sizes["height", ]),
      "cm"
    )
  ) |>
    gtable_add_grob(
      grobs,
      t = 1,
      l = seq_along(grobs),
      name = paste0(
        "column-",
        seq_along(grobs)
      )
    )

  list(
    figure = figure,
    check = check
  )
}


# one panel, each region a coloured block under its name ------------------------

plot_blocks <- function(width_cm) {
  # a row for each region's name, then one per country, numbered from the top
  # (`sources` is already in that order)
  blocks <- sources |>
    mutate(
      row = row_number() + as.integer(region)
    )

  headers <- blocks |>
    group_by(region) |>
    summarise(row = min(row) - 1)

  n_rows <- max(blocks$row)

  blocks <- blocks |>
    mutate(y = n_rows + 1 - row)

  headers <- headers |>
    mutate(y = n_rows + 1 - row)

  build <- function(panel_width_cm) {
    ggplot(
      blocks,
      aes(
        x = n_sources,
        y = y
      )
    ) +
      geom_col(
        aes(fill = region),
        orientation = "y",
        width = 0.8
      ) +
      geom_text(
        data = headers,
        aes(
          x = 1,
          y = y,
          label = region
        ),
        hjust = 0,
        fontface = "bold",
        size = base_size,
        size.unit = "pt"
      ) +
      sources_axis() +
      scale_y_continuous(
        breaks = blocks$y,
        labels = as.character(blocks$label),
        expand = slot_expand
      ) +
      scale_fill_manual(
        values = region_colours,
        name = NULL
      ) +
      labs(
        x = "Unique sources",
        y = NULL
      ) +
      theme_sources +
      theme(
        legend.position = "top",
        legend.key.size = unit(
          slot_cm,
          "cm"
        ),
        panel.heights = unit(
          n_rows * slot_cm,
          "cm"
        ),
        panel.widths = unit(
          panel_width_cm,
          "cm"
        )
      )
  }

  figure <- fit_to_width(
    build,
    width_cm
  )

  list(
    figure = figure,
    check = slot_sizes(
      figure,
      n_per_panel = n_rows,
      along = "height",
      version = "colour"
    )
  )
}


# vertical bars, the regions side by side ---------------------------------------

plot_vertical <- function(panel_height_cm) {
  # most sources on the left of each region
  data <- sources |>
    mutate(
      label = factor(
        label,
        levels = rev(levels(label))
      )
    )

  n_per_panel <- data |>
    count(region) |>
    pull(n)

  figure <- ggplotGrob(
    ggplot(
      data,
      aes(
        x = label,
        y = n_sources
      )
    ) +
      geom_col(
        fill = "grey35",
        width = 0.8
      ) +
      facet_wrap(
        vars(region),
        nrow = 1,
        scales = "free_x"
      ) +
      sources_axis(scale_y_log10) +
      scale_x_discrete(expand = slot_expand) +
      labs(
        x = NULL,
        y = "Unique sources"
      ) +
      theme_sources +
      theme(
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(),
        axis.text.x = element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5
        ),
        panel.widths = unit(
          n_per_panel * slot_cm,
          "cm"
        ),
        panel.heights = unit(
          panel_height_cm,
          "cm"
        )
      )
  )

  list(
    figure = figure,
    check = slot_sizes(
      figure,
      n_per_panel = n_per_panel,
      along = "width",
      version = "vertical"
    )
  )
}


# build, check and save --------------------------------------------------------

versions <- list(
  `1col` = plot_columns(
    columns = list(
      names(region_colours)
    ),
    width_cm = 10,
    version = "1col"
  ),
  `2col` = plot_columns(
    columns = list(
      c(
        "Eastern",
        "Central"
      ),
      c(
        "Western",
        "Northern",
        "Southern"
      )
    ),
    width_cm = 16,
    version = "2col"
  ),
  `2col_colour` = plot_columns(
    columns = list(
      c(
        "Eastern",
        "Central"
      ),
      c(
        "Western",
        "Northern",
        "Southern"
      )
    ),
    width_cm = 16,
    version = "2col_colour",
    fill_by_region = TRUE
  ),
  `3col` = plot_columns(
    columns = list(
      "Eastern",
      "Western",
      c(
        "Central",
        "Northern",
        "Southern"
      )
    ),
    width_cm = 16,
    version = "3col"
  ),
  colour = plot_blocks(width_cm = 10),
  vertical = plot_vertical(panel_height_cm = 4)
)

checks <- lapply(
  versions,
  \(v) v$check
) |>
  bind_rows()

print(
  checks,
  n = Inf
)

# every country's slot, in every panel of every version, is slot_cm
stopifnot(
  isTRUE(
    all.equal(
      checks$slot_cm,
      rep(
        slot_cm,
        nrow(checks)
      )
    )
  )
)

sizes <- Map(
  \(v, name) save_grob(
    v$figure,
    paste0(
      "sources_",
      name,
      ".png"
    )
  ),
  versions,
  names(versions)
)

invisible(dev.off())

unlink(measuring_file)

message(
  "Every country's slot is ",
  slot_cm,
  " cm. Figures (width x height, cm):\n",
  paste0(
    "  sources_",
    names(sizes),
    ".png: ",
    sapply(
      sizes,
      \(s) sprintf(
        "%.1f x %.1f",
        s[["width"]],
        s[["height"]]
      )
    ),
    collapse = "\n"
  )
)


# the one-column figure, simply ------------------------------------------------
#
# The same figure as sources_1col.png above, written over it, without the
# helpers.

tar_load(raw_data)

sources_by_country <- raw_data |>
  distinct(
    country,
    source_id
  ) |>
  count(
    country,
    name = "n_sources"
  ) |>
  mutate(
    country_name = countrycode(
      country,
      origin = "country.name",
      destination = "cldr.short.en",
      custom_match = c("cabo verde" = "Cabo Verde")
    ),
    # UN regions. Northern Africa has no intermediate region, only a sub-region
    region = coalesce(
      countrycode(
        country,
        origin = "country.name",
        destination = "un.regionintermediate.name",
        warn = FALSE
      ),
      countrycode(
        country,
        origin = "country.name",
        destination = "un.regionsub.name"
      )
    ),
    region = region |>
      str_remove(" Africa") |>
      str_replace(
        "Middle",
        "Central"
      ),
    # most sources at the top
    country_name = reorder(
      country_name,
      n_sources
    )
  )

sources_by_country

plot_sources_1col <- ggplot(
  sources_by_country,
  aes(
    x = n_sources,
    y = country_name
  )
) +
  geom_col(
    fill = "grey35",
    width = 0.8
  ) +
  # each panel's height follows its number of countries, so every bar is the
  # same width
  facet_grid(
    rows = vars(region),
    scales = "free_y",
    space = "free_y"
  ) +
  scale_x_log10(
    breaks = c(
      1,
      10,
      100
    )
  ) +
  # half a bar of space at each end of a panel, as between bars; the default
  # adds a little more per panel, so short panels' bars come out wider
  scale_y_discrete(
    expand = expansion(add = 0.5)
  ) +
  labs(
    x = "Unique sources",
    y = NULL
  ) +
  theme_minimal(base_size = 7) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    strip.text.y = element_text(
      angle = 0,
      hjust = 0,
      face = "bold"
    )
  )


sources_by_country |>
  mutate(
    country = reorder(
      country,
      n_sources
    )
  ) |>
  ggplot() +
  geom_col(
    aes(
      x = n_sources,
      y = country,
      fill = region
    )
  ) +
  facet_grid(
    rows = vars(region),
    scales = "free_y",
    space = "free_y"
  ) +
  scale_x_log10()


ggsave(
  "outputs/figures/sources_by_country/sources_1col.png",
  plot_sources_1col,
  width = 10,
  height = 18,
  units = "cm",
  dpi = 600,
  bg = "white"
)
