# Per-species maps of where the offset-strength ("os") model's binary range differs from
# the current model's. Written 2026-10-19.
#
# Each model's masked probability is cut at its own maxSSS threshold. Cells are coloured
# by which models show the species present: both, current only, os only. Presence
# records are drawn on top. One PNG per species, range_change_<species>.png, in
# `output_dir`. Returns the paths.
os_plot_range_change <- function(
    pred_p,
    pred_p_os,
    thresholds,
    thresholds_os,
    model_data_records,
    target_species,
    output_dir
){

  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )

  presences <- model_data_records |>
    filter(
      presence == 1,
      species %in% target_species
    ) |>
    distinct(
      species,
      latitude,
      longitude
    )

  classes <- c("neither", "current only", "os only", "both")

  cols <- c(
    "neither" = "grey92",
    "current only" = "#d95f02",
    "os only" = "#1b9e77",
    "both" = "grey45"
  )

  sapply(
    target_species,
    function(sp){

      thr_current <- thresholds$threshold[thresholds$species == sp]
      thr_os <- thresholds_os$threshold[thresholds_os$species == sp]

      present_current <- pred_p[[sp]] >= thr_current
      present_os <- pred_p_os[[sp]] >= thr_os

      change <- present_current + 2 * present_os

      levels(change) <- data.frame(
        value = 0:3,
        class = classes
      )

      n_cells <- table(
        factor(
          terra::values(change)[, 1],
          levels = 0:3,
          labels = classes
        )
      )

      pts <- presences |>
        filter(species == sp) |>
        terra::vect(
          geom = c("longitude", "latitude"),
          crs = terra::crs(pred_p)
        )

      p <- ggplot() +
        tidyterra::geom_spatraster(
          data = change,
          maxcell = Inf
        ) +
        tidyterra::geom_spatvector(
          data = pts,
          size = 0.3,
          colour = "black"
        ) +
        scale_fill_manual(
          values = cols,
          na.value = "transparent",
          na.translate = FALSE,
          drop = FALSE,
          name = NULL
        ) +
        labs(
          title = bquote(italic("An." ~ .(sp)) ~ ": binary range, current model against offset strength (os)"),
          subtitle = sprintf(
            "Thresholds: current %.3g, os %.3g. 10 km cells: current only %i, os only %i, both %i. Points: presence records",
            thr_current,
            thr_os,
            n_cells[["current only"]],
            n_cells[["os only"]],
            n_cells[["both"]]
          )
        ) +
        theme_minimal(base_size = 10) +
        theme(
          axis.text = element_blank(),
          panel.grid = element_blank()
        )

      f <- file.path(
        output_dir,
        sprintf("range_change_%s.png", sp)
      )

      ggsave(
        f,
        p,
        width = 8,
        height = 8,
        dpi = 200,
        bg = "white"
      )

      f

    }
  ) |>
    unname()

}
