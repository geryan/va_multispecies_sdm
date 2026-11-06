# Maps of how the offset-strength ("os") model's relative abundance differs from the
# current model's. Written 2026-10-21.
#
# Both inputs are the masked abundance rasters (pred_lambda, pred_lambda_os): each
# includes its offset (the 2024 mean; offset^kappa for os) and takes the same expert
# and bare-ground masks, so they compare directly.
#
# The difference is the ratio os / current, drawn on a log2 scale: +1 is twice the
# current model's abundance, -1 half. A ratio is the natural measure because
# abundance spans orders of magnitude, but it is large and meaningless where both
# models predict next to nothing, so cells where both are below `low_frac` of the
# current model's 95th percentile (over cells it doesn't mask to 0) are drawn grey
# rather than coloured. Cells masked to 0 in both are a paler grey.
#
# Writes, to `output_dir`:
# - abundance_difference_<species>.png: current, os (shared log scale) and the ratio;
# - abundance_ratio_all.png: the ratio for every species, a 3 x 6 grid.
# Returns the paths.
os_plot_abundance_difference <- function(
    pred_lambda,
    pred_lambda_os,
    target_species,
    output_dir,
    low_frac = 0.01,
    ratio_limit = 6,
    log_range = 1e4
){

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  col_masked <- "grey92"
  col_low <- "grey75"

  # one layer per species: the log2 ratio, NA where masked or both low; and a
  # background of 1 (masked) or 2 (both low)
  layers <- lapply(
    target_species,
    function(sp){

      current <- pred_lambda[[sp]]
      os <- pred_lambda_os[[sp]]

      v <- terra::values(
        current,
        mat = FALSE
      )

      low <- low_frac * stats::quantile(
        v[is.finite(v) & v > 0],
        0.95,
        names = FALSE
      )

      masked <- current == 0 & os == 0
      both_low <- current < low & os < low

      ratio <- log2(os / current)
      ratio[both_low] <- NA

      background <- terra::ifel(
        masked,
        1,
        terra::ifel(
          both_low,
          2,
          NA
        )
      )

      list(
        ratio = ratio,
        background = background,
        low = low
      )

    }
  ) |>
    setNames(target_species)

  ratio_breaks <- seq(
    -ratio_limit,
    ratio_limit,
    by = 2
  )

  ratio_labels <- ifelse(
    ratio_breaks >= 0,
    sprintf("×%g", 2^ratio_breaks),
    sprintf("×1/%g", 2^-ratio_breaks)
  )

  # the colour scale for the ratio; values past the limits take the end colours
  ratio_scale <- scale_fill_distiller(
    palette = "BrBG",
    direction = 1,
    limits = c(-ratio_limit, ratio_limit),
    breaks = ratio_breaks,
    labels = ratio_labels,
    oob = scales::squish,
    na.value = "transparent",
    name = "os / current"
  )

  background_layer <- function(background){
    list(
      tidyterra::geom_spatraster(
        data = background,
        maxcell = Inf,
        show.legend = FALSE
      ),
      scale_fill_gradient(
        low = col_masked,
        high = col_low,
        limits = c(1, 2),
        na.value = "transparent",
        guide = "none"
      ),
      ggnewscale::new_scale_fill()
    )
  }

  map_theme <- theme_void(base_size = 10) +
    theme(
      plot.title = element_text(hjust = 0.5),
      legend.position = "bottom",
      legend.key.width = unit(1.2, "cm"),
      legend.key.height = unit(0.3, "cm"),
      legend.title.position = "top",
      legend.title = element_text(hjust = 0.5)
    )

  species_files <- sapply(
    target_species,
    function(sp){

      current <- pred_lambda[[sp]]
      os <- pred_lambda_os[[sp]]

      top <- max(
        terra::global(current, "max", na.rm = TRUE)[[1]],
        terra::global(os, "max", na.rm = TRUE)[[1]]
      )

      limits <- c(
        top / log_range,
        top
      )

      abundance_panel <- function(r, title){

        r[r == 0] <- NA

        ggplot() +
          background_layer(
            terra::ifel(
              layers[[sp]]$background == 1,
              1,
              NA
            )
          ) +
          tidyterra::geom_spatraster(
            data = r,
            maxcell = Inf
          ) +
          scale_fill_gradient(
            low = "grey97",
            high = "darkorchid4",
            trans = "log10",
            limits = limits,
            oob = scales::squish,
            labels = function(x) signif(x, 2),
            na.value = "transparent",
            name = "Relative abundance (log scale)"
          ) +
          labs(title = title) +
          map_theme

      }

      ratio_panel <- ggplot() +
        background_layer(layers[[sp]]$background) +
        tidyterra::geom_spatraster(
          data = layers[[sp]]$ratio,
          maxcell = Inf
        ) +
        ratio_scale +
        labs(title = "Ratio, os / current") +
        map_theme

      p <- (
        abundance_panel(current, "Current model") |
          abundance_panel(os, "Offset strength (os)") |
          ratio_panel
      ) +
        plot_annotation(
          title = bquote(italic("An." ~ .(sp)) ~ ": relative abundance, current model against offset strength (os)"),
          caption = sprintf(
            paste(
              "Both include the offset (2024 mean) and the expert and bare-ground masks.",
              "Ratio: green, os higher; brown, current higher; beyond ×%g the end colours.",
              "Pale grey: masked in both. Mid grey: both below %.2g (%g%% of the current model's 95th percentile)."
            ),
            2^ratio_limit,
            layers[[sp]]$low,
            100 * low_frac
          ),
          theme = theme(
            plot.title = element_text(size = 13),
            plot.caption = element_text(hjust = 0)
          )
        )

      f <- file.path(
        output_dir,
        sprintf("abundance_difference_%s.png", sp)
      )

      ggsave(
        f,
        p,
        width = 15,
        height = 6.5,
        dpi = 200,
        bg = "white"
      )

      f

    }
  ) |>
    unname()

  # all species, the ratio only
  ratio_all <- terra::rast(
    lapply(
      layers,
      `[[`,
      "ratio"
    )
  )
  names(ratio_all) <- target_species

  background_all <- terra::rast(
    lapply(
      layers,
      `[[`,
      "background"
    )
  )
  names(background_all) <- target_species

  p_all <- ggplot() +
    background_layer(background_all) +
    tidyterra::geom_spatraster(
      data = ratio_all,
      maxcell = Inf
    ) +
    ratio_scale +
    facet_wrap(
      ~lyr,
      ncol = 6,
      labeller = labeller(
        lyr = function(x) paste("An.", x)
      )
    ) +
    labs(
      title = "Relative abundance, offset strength (os) model against the current model",
      caption = sprintf(
        paste(
          "Green: os higher; brown: current higher; beyond ×%g the end colours.",
          "Pale grey: masked in both. Mid grey: both below %g%% of the current model's 95th percentile for that species."
        ),
        2^ratio_limit,
        100 * low_frac
      )
    ) +
    theme_void(base_size = 11) +
    theme(
      strip.text = element_text(face = "italic"),
      legend.position = "bottom",
      legend.key.width = unit(2, "cm"),
      legend.key.height = unit(0.35, "cm"),
      legend.title.position = "top",
      legend.title = element_text(hjust = 0.5),
      plot.caption = element_text(hjust = 0)
    )

  f_all <- file.path(
    output_dir,
    "abundance_ratio_all.png"
  )

  ggsave(
    f_all,
    p_all,
    width = 16,
    height = 10,
    dpi = 200,
    bg = "white"
  )

  c(
    species_files,
    f_all
  )

}
