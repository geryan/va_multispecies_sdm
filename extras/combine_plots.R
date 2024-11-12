library(tidyverse)
library(png)
library(ggplot2)
library(patchwork)

setwd("~/Desktop/")
sinka_files <- list.files("sinka_maps",
                          full.names = TRUE,
                          pattern = ".png")
# data_plots <- list.files("records",
#                          full.names = TRUE,
#                          pattern = ".png")
# offset_plots <- list.files("distributions/expert_offset_preds_mspp",
#                            full.names = TRUE,
#                            pattern = ".png")
# no_offset_plots <- list.files("distributions/multispecies_pp",
#                               full.names = TRUE,
#                               pattern = ".png")

species <- basename(sinka_files) %>%
  str_remove("^sinka_") %>%
  str_remove(".png$")

data_files <- file.path("records", sprintf("points_%s.png", species))
offset_files <- file.path("distributions/expert_offset_preds_mspp", sprintf("distribution_expert_offset_preds_mspp_%s.png", species))
no_offset_files <- file.path("distributions/multispecies_pp", sprintf("distribution_multispecies_pp_%s.png", species))

for (i in seq_along(species)) {
  
  sinka_gg <- sinka_files[i] %>%
    readPNG() %>%
    grid::rasterGrob(width = ggplot2::unit(0.6, "npc"),
                     height = ggplot2::unit(1, "npc")) %>%
    ggplot2::annotation_custom(-Inf, Inf, -Inf, Inf)
  sinka_plot <- ggplot2::ggplot() +
    sinka_gg +
    ggtitle("Sinka 2010")
  
  data_gg <- data_files[i] %>%
    readPNG() %>%
    grid::rasterGrob(width = ggplot2::unit(1.1, "npc"),
                     height = ggplot2::unit(1, "npc")) %>%
    ggplot2::annotation_custom(-Inf, Inf, -Inf, Inf)
  data_plot <- ggplot2::ggplot() +
    data_gg +
    ggtitle("New data")
  
  no_offset_gg <- no_offset_files[i] %>%
    readPNG() %>%
    grid::rasterGrob(width = ggplot2::unit(1, "npc"),
                     height = ggplot2::unit(1, "npc")) %>%
    ggplot2::annotation_custom(-Inf, Inf, -Inf, Inf)
  no_offset_plot <- ggplot2::ggplot() +
    no_offset_gg +
    ggtitle("Without expert opinion")
  
  offset_gg <- offset_files[i] %>%
    readPNG() %>%
    grid::rasterGrob(width = ggplot2::unit(1, "npc"),
                     height = ggplot2::unit(1, "npc")) %>%
    ggplot2::annotation_custom(-Inf, Inf, -Inf, Inf)
  offset_plot <- ggplot2::ggplot() +
    offset_gg +
    ggtitle("With expert opinion")
  
  species_name_plot <- species[i] %>%
    str_replace("_", "/") %>%
    sprintf("Anopheles %s", .)
  
  # combine into multi-panel plot
  multipanel <- (sinka_plot + data_plot) /
    (no_offset_plot + offset_plot) +
    plot_annotation(title = species_name_plot)
  
  dir.create("combined", showWarnings = FALSE)
  filename <- file.path("combined",
                        sprintf("%s.png", species[i]))
  ggsave(filename,
         plot = multipanel,
         width = 9,
         height = 7)
  
}

