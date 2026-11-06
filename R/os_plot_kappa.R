# The offset-strength ("os") model's per-species offset power kappa. Written 2026-10-19.
#
# kappa = 1 is the current model; < 1, the species is less limited than An. gambiae where
# the gambiae offset is low; > 1, more limited. Writes to `output_dir`:
#
#   kappa.csv             per species: median, 90% interval, P(kappa < 1)
#   kappa.png             the same as dots and intervals, greyscale
#   kappa_offset_curves.png  per species, the offset's effect offset^kappa against the
#                         offset (median and 90% band), with the current model's
#                         offset^1 dashed; both axes log scale
#
# Returns the three paths.
os_plot_kappa <- function(
    draws_file,
    output_dir
){

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  fit <- readRDS(draws_file)

  kappa <- fit$kappa
  colnames(kappa) <- fit$target_species

  kappa_tbl <- tibble::tibble(
    species = fit$target_species,
    median = apply(kappa, 2, stats::median),
    q05 = apply(kappa, 2, stats::quantile, 0.05),
    q95 = apply(kappa, 2, stats::quantile, 0.95),
    p_below_1 = colMeans(kappa < 1)
  ) |>
    arrange(median)

  csv_file <- file.path(output_dir, "kappa.csv")

  readr::write_csv(
    kappa_tbl,
    csv_file
  )

  p_kappa <- kappa_tbl |>
    mutate(
      species = factor(species, levels = species)
    ) |>
    ggplot(
      aes(
        x = median,
        y = species
      )
    ) +
    geom_vline(
      xintercept = 1,
      linetype = "dashed",
      colour = "grey50"
    ) +
    geom_linerange(
      aes(
        xmin = q05,
        xmax = q95
      ),
      colour = "grey30"
    ) +
    geom_point(
      size = 2
    ) +
    scale_x_log10() +
    labs(
      x = "offset power kappa (median and 90% interval; 1 = current model)",
      y = NULL,
      title = "Per-species strength of the An. gambiae offset",
      subtitle = sprintf(
        "Prior: log kappa ~ Student-t(3, 0, %s)",
        format(fit$kappa_scale)
      )
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.y = element_text(face = "italic")
    )

  kappa_file <- file.path(output_dir, "kappa.png")

  ggsave(
    kappa_file,
    p_kappa,
    width = 6,
    height = 5,
    dpi = 200,
    bg = "white"
  )

  # offset^kappa over the range of the offset, per species
  offset_grid <- 10 ^ seq(-6, log10(3), length.out = 100)

  curves <- lapply(
    fit$target_species,
    function(sp){
      eff <- outer(
        log(offset_grid),
        kappa[, sp]
      ) |>
        exp()
      tibble::tibble(
        species = sp,
        offset = offset_grid,
        median = apply(eff, 1, stats::median),
        q05 = apply(eff, 1, stats::quantile, 0.05),
        q95 = apply(eff, 1, stats::quantile, 0.95)
      )
    }
  ) |>
    bind_rows()

  p_curves <- curves |>
    ggplot(
      aes(
        x = offset
      )
    ) +
    geom_ribbon(
      aes(
        ymin = q05,
        ymax = q95
      ),
      fill = "grey80"
    ) +
    geom_line(
      aes(
        y = offset
      ),
      linetype = "dashed",
      colour = "grey40"
    ) +
    geom_line(
      aes(
        y = median
      )
    ) +
    facet_wrap(
      ~species,
      ncol = 6
    ) +
    scale_x_log10() +
    scale_y_log10() +
    labs(
      x = "An. gambiae offset",
      y = "offset ^ kappa",
      title = "The offset's effect on each species (dashed: current model)"
    ) +
    theme_minimal(base_size = 9) +
    theme(
      strip.text = element_text(face = "italic")
    )

  curves_file <- file.path(output_dir, "kappa_offset_curves.png")

  ggsave(
    curves_file,
    p_curves,
    width = 12,
    height = 7,
    dpi = 200,
    bg = "white"
  )

  c(
    csv_file,
    kappa_file,
    curves_file
  )

}
