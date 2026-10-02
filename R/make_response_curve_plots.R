#' Response plots: each species' main effects, against the average surveyed
#' location
#'
#' One figure per species, from the main effects, which are the slopes in the
#' average bioregion. The bioregion terms are shown by
#' make_bioregion_multiplier_plots() instead: at a species' own locations they
#' mostly shift its abundance level, spread over whichever covariates are
#' present there, so slopes taken there would mix the two.
#'
#' - Land cover: relative abundance when a class is `step` higher (10
#'   percentage points by default) than in the average surveyed location, every
#'   other class, bare and sparse included, scaled down by the same factor so
#'   the cell still sums to 1. Land cover is compositional and bare and sparse
#'   are the classes left out of the model, so a class's slope on its own
#'   compares a cell entirely of that class with one entirely of bare and sparse
#'   ground, which almost no surveyed location is. This compares each class with
#'   a mix that is surveyed instead.
#' - Every other covariate: relative abundance across its range at the surveyed
#'   locations, against its median there (dashed line).
#'
#' The average surveyed location is the same for every species: the mean land
#' cover mix and the median of each other covariate over the distinct surveyed
#' locations (every record but the background points). The model is
#' log-linear, so neither result depends on alpha or on where the other
#' covariates are held.
#'
#' Writes `response_<species>.png` into `plot_dir`.
#'
#' @param coef_draws_file path of the .rds from extract_cv_draws()
#' @param model_data_spatial the data the model was fitted to
#' @param landcover_classes the land cover covariates (model_landcover_classes)
#' @param plot_dir directory to write to
#' @param labels covariate display names
#' @param probs lower and upper quantiles of the intervals
#' @param step increase in a land cover class, as a proportion of the cell
#' @param n_x points along each curve
#' @return paths of the files written
#' @author geryan
#' @export
make_response_curve_plots <- function(
    coef_draws_file,
    model_data_spatial,
    landcover_classes,
    plot_dir,
    labels = covariate_labels(),
    probs = c(0.05, 0.95),
    step = 0.1,
    n_x = 101
){

  coef_draws <- readRDS(coef_draws_file)

  covs <- coef_draws$target_covariate_names
  spp <- coef_draws$target_species

  lc <- intersect(
    covs,
    landcover_classes
  )

  if (length(lc) != length(landcover_classes)) {
    stop(
      "make_response_curve_plots(): not every land cover class is a model covariate",
      call. = FALSE
    )
  }

  other <- setdiff(
    covs,
    lc
  )

  # main effects [D, A, S]: the first A columns of the design
  main <- coef_draws$beta[, seq_along(covs), , drop = FALSE]
  dimnames(main) <- list(
    NULL,
    covs,
    spp
  )

  surveyed <- model_data_spatial |>
    filter(
      data_type != "bg"
    ) |>
    distinct(
      latitude,
      longitude,
      model_date,
      .keep_all = TRUE
    )

  # the average surveyed location. Bare and sparse take whatever the modelled
  # classes leave, with a slope of 0
  mix <- colMeans(
    as.matrix(
      surveyed[, lc]
    )
  )
  mix <- c(
    mix,
    bare_sparse = 1 - sum(mix)
  )

  x_ref <- vapply(
    other,
    function(a) stats::median(surveyed[[a]]),
    numeric(1)
  )

  detected <- model_data_spatial |>
    filter(
      data_type != "bg",
      data_type == "po" | n > 0
    ) |>
    distinct(
      species,
      latitude,
      longitude,
      model_date,
      .keep_all = TRUE
    )

  class_labels <- c(
    labels[lc],
    bare_sparse = "Bare & sparse (dropped)"
  )

  multiplier_labels <- function(x){
    format(
      x,
      scientific = FALSE,
      drop0trailing = TRUE,
      trim = TRUE,
      big.mark = ","
    )
  }

  ink <- "grey15"
  band <- "grey50"

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

      slopes <- cbind(
        main[, lc, s],
        bare_sparse = 0
      )

      # raising class a by `step` scales every other class k by
      # (1 - mix_a - step) / (1 - mix_a), so log abundance changes by
      # step * (slope_a - sum_k mix_k * slope_k / (1 - mix_a))
      log_ratio <- vapply(
        names(mix),
        function(a){
          k <- setdiff(
            names(mix),
            a
          )
          step * (slopes[, a] - drop(slopes[, k] %*% mix[k]) / (1 - mix[[a]]))
        },
        numeric(nrow(slopes))
      )

      q <- apply(
        exp(log_ratio),
        2,
        stats::quantile,
        probs = c(0.5, probs)
      )

      classes <- tibble::tibble(
        label = factor(
          class_labels[names(mix)],
          levels = rev(class_labels)
        ),
        median = q[1, ],
        lower = q[2, ],
        upper = q[3, ]
      )

      p_lc <- ggplot(
        classes,
        aes(
          y = label,
          x = median
        )
      ) +
        geom_vline(
          xintercept = 1,
          colour = "grey60",
          linewidth = 0.3
        ) +
        geom_linerange(
          aes(
            xmin = lower,
            xmax = upper
          ),
          colour = ink,
          linewidth = 0.7
        ) +
        geom_point(
          colour = ink,
          size = 2
        ) +
        scale_x_log10(
          breaks = scales::breaks_log(
            n = 7
          ),
          labels = multiplier_labels
        ) +
        theme_minimal() +
        theme(
          panel.grid.minor = element_blank(),
          plot.title.position = "plot"
        ) +
        labs(
          title = "Land cover",
          subtitle = stringr::str_wrap(
            sprintf(
              "Relative abundance when land cover type is %g percentage points higher than the average surveyed location",
              100 * step
            ),
            width = 66
          ),
          x = "Relative abundance (log scale)",
          y = NULL
        )

      p_other <- lapply(
        other,
        function(a){

          x <- seq(
            min(surveyed[[a]]),
            max(surveyed[[a]]),
            length.out = n_x
          )

          qa <- apply(
            exp(outer(main[, a, s], x - x_ref[[a]])),
            2,
            stats::quantile,
            probs = c(0.5, probs)
          )

          curve <- tibble::tibble(
            x = x,
            median = qa[1, ],
            lower = qa[2, ],
            upper = qa[3, ]
          )

          ggplot(
            curve,
            aes(
              x = x
            )
          ) +
            geom_hline(
              yintercept = 1,
              colour = "grey60",
              linewidth = 0.3
            ) +
            geom_vline(
              xintercept = x_ref[[a]],
              colour = "grey40",
              linetype = 2,
              linewidth = 0.3
            ) +
            geom_ribbon(
              aes(
                ymin = lower,
                ymax = upper
              ),
              fill = band,
              alpha = 0.3
            ) +
            geom_line(
              aes(
                y = median
              ),
              colour = ink,
              linewidth = 0.7
            ) +
            geom_rug(
              data = detected |>
                filter(
                  species == s
                ),
              aes(
                x = .data[[a]]
              ),
              sides = "b",
              alpha = 0.08,
              length = unit(0.03, "npc"),
              inherit.aes = FALSE
            ) +
            scale_y_log10(
              breaks = scales::breaks_log(
                n = 6
              ),
              labels = multiplier_labels
            ) +
            theme_minimal() +
            theme(
              panel.grid.minor = element_blank()
            ) +
            labs(
              title = labels[[a]],
              x = NULL,
              y = "Relative abundance"
            )

        }
      )

      p <- patchwork::wrap_plots(
        p_lc,
        patchwork::wrap_plots(
          p_other,
          ncol = 1
        ),
        widths = c(1.2, 1)
      ) +
        patchwork::plot_annotation(
          title = bquote(italic(.(paste("Anopheles", s)))),
          subtitle = sprintf(
            "Main effects, against the average surveyed location (dashed line on footprint and proximity to sea plots). Posterior median and %s interval.\nRug: locations where the species was recorded.",
            interval
          )
        )

      f <- file.path(
        plot_dir,
        sprintf(
          "response_%s.png",
          s
        )
      )

      ggsave(
        filename = f,
        plot = p,
        width = 10,
        height = max(5.5, 2.75 * length(other)),
        dpi = 300,
        units = "in",
        bg = "white"
      )

      f

    },
    character(1)
  )

}
