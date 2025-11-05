rm(list = ls())

library(targets)
tar_load_globals()
tar_load_everything()

# load in predictions from old data without mechanistic offset

pred_multisp_pp <- rast("outputs/rasters/multisp_pp.tif")
pred_multisp_pp

# get expert maps
expert_maps <- get_expert_maps(
  sp = c(
    "arabiensis",
    # "atroparvus",
    "funestus",
    "gambiae",
    # "labranchiae",
    "melas",
    "merus",
    # "messeae",
    "moucheti",
    "nili"#,
    # "sacharovi",
    # "sergentii",
    # "superpictus",
  )
)

# make aggregated mask because the distance calculation takes effing ages
maskagg <- aggregate(project_mask, fact = 10)

# get species
spp <- expert_maps$species

# calculate the distance from the expert map layers across the
aggdist <- sapply(
  X = spp,
  FUN = function(spp, expert_maps, maskagg){
    distance(maskagg, expert_maps |> filter(species == spp), rasterize = TRUE)
  },
  expert_maps,
  maskagg,
  simplify = FALSE
)

z <- 1 - aggdist$arabiensis/1000000
z[which(values(z)<0)] <- 0

aggoffset <- sapply(
  aggdist,
  FUN = function(aggdist){
    z <- 1 - aggdist/500000
    z[which(values(z)<0)] <- 0
    z
  }
)

writeRaster(
  rast(aggoffset),
  "outputs/rasters/va_plots_20250718/expert_offset_aggregated_500.tif"
)

expert_offset_preds_mspp <- c(
  terra::subset(pred_multisp_pp, "arabiensis") * aggoffset$arabiensis,
  subset(pred_multisp_pp, "coluzzii") * aggoffset$gambiae,
  subset(pred_multisp_pp, "funestus") * aggoffset$funestus,
  subset(pred_multisp_pp, "funestus_complex") * aggoffset$funestus,
  subset(pred_multisp_pp, "gambiae") * aggoffset$gambiae,
  subset(pred_multisp_pp, "gambiae_coluzzii") * aggoffset$gambiae,
  subset(pred_multisp_pp, "gambiae_complex") * aggoffset$gambiae,
  subset(pred_multisp_pp, "melas") * aggoffset$melas,
  subset(pred_multisp_pp, "merus") * aggoffset$merus,
  subset(pred_multisp_pp, "moucheti") * aggoffset$moucheti,
  subset(pred_multisp_pp, "nili") * aggoffset$nili
)

writeRaster(
  expert_offset_preds_mspp,
  "outputs/rasters/va_plots_20250718/expert_offset_preds_mspp.tif"
)
expert_offset_preds_mspp <- rast("outputs/rasters/va_plots_20250718/expert_offset_preds_mspp.tif")

distplot_pts <- function(rst, sp, pts){
  spname <- paste0("Anopheles ", sp)

  r <- subset(rst, sp)

  ggplot() +
    geom_spatraster(
      data = r
    ) +
    geom_point(
      data = pts |> filter(species == sp),
      aes(
        x = lon,
        y = lat,
        col = type
      ),
      alpha = 0.7
    ) +
    scale_colour_viridis_d() +
    theme_void() +
    labs(
      title = bquote(italic(.(spname))),
      subtitle = "Occurrence data",
      col = "Data type"
    ) +
    guides(
      fill = "none",
      alpha = "none"
    )

}

distplot <- function(rst, sp){
  spname <- paste0("Anopheles ", sp)

  r <- subset(rst, sp)

  ggplot() +
    geom_spatraster(
      data = r
    ) +
    scale_fill_viridis_c(
      option = "G",
      direction = -1,
      na.value = "transparent"
    ) +
    theme_void() +
    labs(
      title = bquote(italic(.(spname)))
    ) +
    guides(
      fill = "none"
    )

}

eo_plots <- sapply(
  X = names(expert_offset_preds_mspp),
  FUN = function(x, expert_offset_preds_mspp){
    distplot(expert_offset_preds_mspp, x)
  },
  expert_offset_preds_mspp
)

ts_plots <- eo_plots[target_species]

ts_plots_points <- sapply(
  X = target_species,
  FUN = function(
    x,
    ts_plots,
    model_data_spatial
  ){
    p <- ts_plots[[x]]

    d <- model_data_spatial |>
      filter(
        species == x,
        data_type != presence
      )

    p2 <- p +
      geom_point(
        data = d |>
          mutate(
            detected = case_when(
              presence == 1 ~ "Detected",
              presence == 0 ~ "Undetected"
            )
          ),
        aes(
          x = longitude,
          y = latitude,
          #shape = data_type,
          col = detected
        ),
        alpha = 0.7,
        size = 0.4
      ) +
      scale_colour_manual(
        values = c("deeppink", "grey70")
      ) +
      theme(legend.position = "none")

    p2

  },
  ts_plots,
  model_data_spatial
)

library(patchwork)

# 4-species / 4-panel distribution plot with points

ts_points_plot_all <- (ts_plots_points$coluzzii + ts_plots_points$gambiae) /
  (ts_plots_points$arabiensis + ts_plots_points$funestus)

ts_points_plot_all

ggsave(
  filename = "outputs/figures/distribution_plots/plots_for_va_mtg_20250718/mspp_dist_4spp_points.png",
  plot = ts_points_plot_all,
  width = 3200,
  height = 3200,
  dpi = 300,
  units = "px"
)

# without points

ts_plot_all <- (ts_plots$coluzzii + ts_plots$gambiae) /
  (ts_plots$arabiensis + ts_plots$funestus)

ts_plot_all

ggsave(
  filename = "outputs/figures/distribution_plots/plots_for_va_mtg_20250718/mspp_dist_4spp.png",
  plot = ts_plot_all,
  width = 3200,
  height = 3200,
  dpi = 300,
  units = "px"
)


#### alt colours

eo_plots_rb <- sapply(
  X = names(expert_offset_preds_mspp),
  # FUN = function(x, expert_offset_preds_mspp){
  #   distplot(expert_offset_preds_mspp, x)
  # },
  FUN = function(sp, rst){
    spname <- paste0("Anopheles ", sp)

    r <- subset(rst, sp)

    ggplot() +
      geom_spatraster(
        data = r
      ) +
      # scale_fill_viridis_c(
      #   option = "G",
      #   direction = -1,
      #   na.value = "transparent"
      # ) +
      scale_colour_distiller(
        palette = "RdBu",
        type = "div",
        aesthetics = c("fill"),
        na.value = "transparent"
      ) +
      theme_void() +
      labs(
        title = bquote(italic(.(spname)))
      ) +
      guides(
        fill = "none"
      )

  },
  rst = expert_offset_preds_mspp
)


eo_plots_rb$arabiensis

ts_plots_rb <- eo_plots_rb[target_species]

ts_plots_points_rb <- sapply(
  X = target_species,
  FUN = function(
    x,
    ts_plots_rb,
    model_data_spatial
  ){
    p <- ts_plots_rb[[x]]

    d <- model_data_spatial |>
      filter(
        species == x,
        data_type != presence
      )

    p2 <- p +
      geom_point(
        data = d |>
          mutate(
            detected = case_when(
              presence == 1 ~ "Detected",
              presence == 0 ~ "Undetected"
            )
          ),
        aes(
          x = longitude,
          y = latitude,
          #shape = data_type,
          col = detected
        ),
        alpha = 0.7,
        size = 0.4
      ) +
      scale_colour_manual(
        values = c("yellow", "grey70")
      ) +
      theme(legend.position = "none")

    p2

  },
  ts_plots_rb,
  model_data_spatial
)

library(patchwork)

# 4-species / 4-panel distribution plot with points

ts_points_plot_all_rb <- (ts_plots_points_rb$coluzzii + ts_plots_points_rb$gambiae) /
  (ts_plots_points_rb$arabiensis + ts_plots_points_rb$funestus)

ts_points_plot_all_rb

ggsave(
  filename = "outputs/figures/distribution_plots/plots_for_va_mtg_20250718/mspp_dist_4spp_points_rb.png",
  plot = ts_points_plot_all_rb,
  width = 3200,
  height = 3200,
  dpi = 300,
  units = "px"
)

# without points

ts_plot_all_rb <- (ts_plots_rb$coluzzii + ts_plots_rb$gambiae) /
  (ts_plots_rb$arabiensis + ts_plots_rb$funestus)

ts_plot_all_rb

ggsave(
  filename = "outputs/figures/distribution_plots/plots_for_va_mtg_20250718/mspp_dist_4spp_rb.png",
  plot = ts_plot_all_rb,
  width = 3200,
  height = 3200,
  dpi = 300,
  units = "px"
)

