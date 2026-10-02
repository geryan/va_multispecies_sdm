#' Each bioregion's multiplier on relative abundance, one figure per species
#'
#' The covariate x bioregion terms act together, mostly as a shift in each
#' bioregion's abundance level; one term on its own is weakly identified (see
#' bioregion_effect_rast()). This shows that shift for each bioregion b: the
#' combined terms evaluated at b's average covariates,
#'
#'   sum_a xbar_ab * gamma_abs,
#'
#' with xbar_ab the mean of covariate a over the grid, cells weighted by their
#' membership of b. It is a multiplier on relative abundance against the main
#' effects alone, i.e. against the average bioregion.
#'
#' A row per bioregion: posterior median and interval, and the number of
#' distinct locations where the species was recorded in that bioregion
#' (membership > `min_membership`), hollow where it never was. The intervals
#' narrow only modestly with data -- on the 2026-10 `_cx` fit, from about the
#' prior's width where the species was never recorded to about 0.7 of it where
#' it was recorded at over 100 locations -- so read them with the counts.
#'
#' Writes `bioregion_multipliers_<species>.png` into `plot_dir`.
#'
#' @param coef_draws_file path of the .rds from extract_cv_draws()
#' @param covariates SpatRaster holding the model's covariates and bioregions
#' @param model_data_spatial the data the model was fitted to, for the record
#'   counts
#' @param plot_dir directory to write to
#' @param probs lower and upper quantiles of the intervals
#' @param min_membership bioregion membership above which a record counts
#'   towards that bioregion
#' @return paths of the files written
#' @author geryan
#' @export
make_bioregion_multiplier_plots <- function(
    coef_draws_file,
    covariates,
    model_data_spatial,
    plot_dir,
    probs = c(0.05, 0.95),
    min_membership = 0.5
){

  coef_draws <- readRDS(coef_draws_file)

  covs <- coef_draws$target_covariate_names
  bios <- coef_draws$bioregion_names
  spp <- coef_draws$target_species

  n_a <- length(covs)
  n_b <- length(bios)

  v <- terra::values(
    covariates[[c(covs, bios)]]
  )
  v <- v[stats::complete.cases(v), , drop = FALSE]

  z <- v[, bios, drop = FALSE]

  # [B, A]: each bioregion's average covariates, cells weighted by membership
  xbar <- crossprod(
    z,
    v[, covs, drop = FALSE]
  ) / colSums(z)

  # interaction columns are covariate-major (make_designmat_interactions()):
  # column A + (a - 1) * B + b is covariate a times bioregion b
  gamma_idx <- n_a + outer(
    seq_len(n_b),
    (seq_len(n_a) - 1) * n_b,
    FUN = "+"
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

  # [S, B]
  n_detected <- vapply(
    bios,
    function(b){
      as.integer(
        table(
          factor(
            detected$species[detected[[b]] > min_membership],
            levels = spp
          )
        )
      )
    },
    integer(length(spp))
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
    seq_along(spp),
    function(i){

      level <- vapply(
        seq_len(n_b),
        function(b){
          drop(coef_draws$beta[, gamma_idx[b, ], i] %*% xbar[b, ])
        },
        numeric(dim(coef_draws$beta)[1])
      )

      q <- apply(
        exp(level),
        2,
        stats::quantile,
        probs = c(0.5, probs)
      )

      d <- tibble::tibble(
        bioregion = factor(
          bios,
          levels = rev(sort(bios))
        ),
        median = q[1, ],
        lower = q[2, ],
        upper = q[3, ],
        n_det = n_detected[i, ],
        data = factor(
          ifelse(
            n_det > 0,
            "recorded there",
            "never recorded there"
          ),
          levels = c(
            "recorded there",
            "never recorded there"
          )
        )
      )

      p <- ggplot(
        d,
        aes(
          y = bioregion,
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
            xmax = upper,
            colour = data
          ),
          linewidth = 0.6
        ) +
        geom_point(
          aes(
            colour = data,
            shape = data
          ),
          size = 1.8,
          fill = "white"
        ) +
        geom_text(
          aes(
            x = Inf,
            label = n_det
          ),
          hjust = 1.1,
          size = 2.6,
          colour = "grey40"
        ) +
        scale_shape_manual(
          values = c(19, 21),
          name = NULL,
          drop = FALSE
        ) +
        scale_colour_manual(
          values = c("grey15", "grey60"),
          name = NULL,
          drop = FALSE
        ) +
        scale_x_log10(
          labels = function(x){
            format(
              x,
              drop0trailing = TRUE,
              trim = TRUE
            )
          },
          expand = expansion(
            mult = c(0.04, 0.16)
          )
        ) +
        scale_y_discrete(
          labels = scales::label_wrap(40)
        ) +
        theme_minimal(
          base_size = 9
        ) +
        theme(
          legend.position = "bottom",
          panel.grid.minor = element_blank(),
          plot.title.position = "plot"
        ) +
        labs(
          title = bquote(italic(.(paste("Anopheles", spp[i])))),
          subtitle = sprintf(
            "Bioregion multiplier at each bioregion's average covariates, against the main effects alone.\nPosterior median and %s interval.\nNumbers on the right: locations where the species was recorded in that bioregion (membership > %g).",
            interval,
            min_membership
          ),
          x = "Multiplier on relative abundance (log scale)",
          y = NULL
        )

      f <- file.path(
        plot_dir,
        sprintf(
          "bioregion_multipliers_%s.png",
          spp[i]
        )
      )

      ggsave(
        filename = f,
        plot = p,
        width = 7.5,
        height = 6,
        dpi = 300,
        units = "in",
        bg = "white"
      )

      f

    },
    character(1)
  )

}
