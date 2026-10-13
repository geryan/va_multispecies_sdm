#' Heatmaps of each species' covariate slopes by bioregion
#'
#' One figure per species, three heatmaps side by side, covariates in rows:
#'
#' - the slope averaged over the modelled area, with its value printed
#' - the slope in each bioregion (beta + gamma_b; covariate_slopes())
#' - each bioregion's departure from the average slope, dotted where its 90%
#'   interval excludes 0
#'
#' Cells show posterior medians. A slope is the change in log relative
#' abundance from none of a covariate to all of it.
#'
#' The departures are the interaction terms. They are shrunk hard towards 0
#' (sd 0.1), and with no bioregion intercepts and land cover summing to ~1 they
#' are weakly identified one by one, so read the dots, not the shades. The
#' departure scale is set per species but is never narrower than
#' +/- `dev_floor`, so a species whose departures are all near 0 shows as pale
#' rather than amplified noise.
#'
#' Writes `bioregion_slopes_<species>.png` into `plot_dir`.
#'
#' @param coef_draws_file path of the .rds from extract_cv_draws()
#' @param covariates SpatRaster holding the bioregion layers, for the area
#'   weights (bioregion_area_weights())
#' @param plot_dir directory to write to
#' @param labels covariate display names
#' @param probs lower and upper quantiles of the intervals
#' @param dev_floor smallest half-width of the departure colour scale
#' @return paths of the files written
#' @author geryan
#' @export
make_bioregion_slope_heatmaps <- function(
    coef_draws_file,
    covariates,
    plot_dir,
    labels = covariate_labels(),
    probs = c(0.05, 0.95),
    dev_floor = 0.2
){

  coef_draws <- readRDS(coef_draws_file)

  covs <- coef_draws$target_covariate_names
  bios <- coef_draws$bioregion_names
  spp <- coef_draws$target_species

  slopes <- covariate_slopes(
    coef_draws,
    weights = bioregion_area_weights(
      covariates,
      bios
    )
  )

  # average slope broadcast to [D, A, B, S], to subtract from each bioregion's
  n_d <- dim(slopes$bioregion)[1]
  average_by_bioregion <- array(
    slopes$average,
    dim = c(
      n_d,
      length(covs),
      length(spp),
      length(bios)
    )
  ) |>
    aperm(
      c(1, 2, 4, 3)
    )

  q_slope <- summarise_draws_array(
    slopes$bioregion,
    probs = probs
  )

  q_dev <- summarise_draws_array(
    slopes$bioregion - average_by_bioregion,
    probs = probs
  )

  q_avg <- summarise_draws_array(
    slopes$average,
    probs = probs
  )

  # covariates top to bottom in model order
  cov_levels <- rev(labels[covs])

  cells <- tidyr::expand_grid(
    ci = seq_along(covs),
    bi = seq_along(bios),
    si = seq_along(spp)
  ) |>
    mutate(
      covariate = factor(
        labels[covs[ci]],
        levels = cov_levels
      ),
      bioregion = factor(
        bios[bi],
        levels = bios
      ),
      species = spp[si],
      slope = q_slope["median", , , ][cbind(ci, bi, si)],
      dev = q_dev["median", , , ][cbind(ci, bi, si)],
      clear = q_dev["lower", , , ][cbind(ci, bi, si)] > 0 |
        q_dev["upper", , , ][cbind(ci, bi, si)] < 0
    )

  averages <- tidyr::expand_grid(
    ci = seq_along(covs),
    si = seq_along(spp)
  ) |>
    mutate(
      covariate = factor(
        labels[covs[ci]],
        levels = cov_levels
      ),
      species = spp[si],
      slope = q_avg["median", , ][cbind(ci, si)]
    )

  diverging <- function(limit, name){
    scale_fill_gradient2(
      low = "#2166AC",
      mid = "grey97",
      high = "#B2182B",
      limits = c(-limit, limit),
      oob = scales::squish,
      name = name
    )
  }

  heatmap_theme <- theme_minimal(
    base_size = 9
  ) +
    theme(
      panel.grid = element_blank(),
      axis.text.x = element_text(
        angle = 90,
        hjust = 1,
        vjust = 0.5,
        size = 6.5
      )
    )

  bioregion_axis <- scale_x_discrete(
    labels = scales::label_wrap(26)
  )

  interval <- sprintf(
    "%g%%",
    100 * diff(probs)
  )

  if (!dir.exists(plot_dir)) {
    dir.create(
      plot_dir,
      recursive = TRUE
    )
  }

  vapply(
    spp,
    function(s){

      d <- cells |>
        filter(
          species == s
        )

      a <- averages |>
        filter(
          species == s
        )

      slope_limit <- max(
        abs(
          c(
            d$slope,
            a$slope
          )
        )
      )

      dev_limit <- max(
        dev_floor,
        abs(d$dev)
      )

      p_avg <- ggplot(
        a,
        aes(
          x = "Average",
          y = covariate,
          fill = slope
        )
      ) +
        geom_tile(
          colour = "white"
        ) +
        geom_text(
          aes(
            label = sprintf(
              "%.1f",
              slope
            )
          ),
          size = 2.6
        ) +
        diverging(
          slope_limit,
          "Slope"
        ) +
        heatmap_theme +
        theme(
          legend.position = "none",
          axis.text.x = element_text(
            angle = 0,
            hjust = 0.5
          )
        ) +
        labs(
          x = NULL,
          y = NULL
        )

      p_slope <- ggplot(
        d,
        aes(
          x = bioregion,
          y = covariate,
          fill = slope
        )
      ) +
        geom_tile(
          colour = "white"
        ) +
        diverging(
          slope_limit,
          "Slope\n(log relative\nabundance)"
        ) +
        bioregion_axis +
        heatmap_theme +
        theme(
          axis.text.y = element_blank()
        ) +
        labs(
          x = NULL,
          y = NULL,
          title = "Slope in each bioregion"
        )

      p_dev <- ggplot(
        d,
        aes(
          x = bioregion,
          y = covariate,
          fill = dev
        )
      ) +
        geom_tile(
          colour = "white"
        ) +
        geom_point(
          data = d |>
            filter(
              clear
            ),
          size = 0.8
        ) +
        diverging(
          dev_limit,
          "Departure\nfrom average"
        ) +
        bioregion_axis +
        heatmap_theme +
        theme(
          axis.text.y = element_blank()
        ) +
        labs(
          x = NULL,
          y = NULL,
          title = sprintf(
            "Departure from the average slope (dot: %s interval excludes 0)",
            interval
          )
        )

      p <- (p_avg | p_slope | p_dev) +
        patchwork::plot_layout(
          widths = c(1, 10, 10)
        ) +
        patchwork::plot_annotation(
          title = bquote(italic(.(paste("Anopheles", s)))),
          subtitle = "Posterior medians. Slope: change in log relative abundance from none of the covariate to all of it"
        )

      f <- file.path(
        plot_dir,
        sprintf(
          "bioregion_slopes_%s.png",
          s
        )
      )

      ggsave(
        filename = f,
        plot = p,
        width = 15,
        height = 6.5,
        dpi = 300,
        units = "in",
        bg = "white"
      )

      f

    },
    character(1)
  )

}
