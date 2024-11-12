# va_plots_20241112

library(targets)
tar_load_globals()
tar_load_everything()


pred_multisp_pp

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

distplot(pred_multisp_pp, "arabiensis")
distplot(pred_multisp_pp_with_offset, "arabiensis")

sapply(
  X = target_species,
  FUN = function(x, pred_multisp_pp){
    distplot(pred_multisp_pp, x)
    ggsave(
      filename = sprintf(
        "outputs/figures/distribution_plots/%s/distribution_%s_%s.png",
        "multispecies_pp",
        "multispecies_pp",
        x
      ),
      width = 3200,
      height = 2000,
      units = "px"
    )
  },
  pred_multisp_pp
)

sapply(
  X = target_species,
  FUN = function(x, pred_multisp_pp_with_offset){
    distplot(pred_multisp_pp_with_offset, x)
    ggsave(
      filename = sprintf(
        "outputs/figures/distribution_plots/%s/distribution_%s_%s.png",
        "multispecies_pp_with_offset",
        "multispecies_pp_with_offset",
        x
      ),
      width = 3200,
      height = 2000,
      units = "px"
    )
  },
  pred_multisp_pp_with_offset
)

# version with points
# version with buffered mask-y-offsety-thing





expert_maps

z <- distance(new_mask, expert_maps |> filter(species == "arabiensis"))


maskagg <- aggregate(new_mask, fact = 10)

z <- distance(maskagg, expert_maps |> filter(species == "arabiensis"))


spp <- expert_maps$species

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
    z <- 1 - aggdist/1000000
    z[which(values(z)<0)] <- 0
    z
  }
)

expert_offset_preds_mspp <- c(
  subset(pred_multisp_pp, "arabiensis") * aggoffset$arabiensis,
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

expert_offset_preds_mspp_with_offset <- c(
  subset(pred_multisp_pp_with_offset, "arabiensis") * aggoffset$arabiensis,
  subset(pred_multisp_pp_with_offset, "coluzzii") * aggoffset$gambiae,
  subset(pred_multisp_pp_with_offset, "funestus") * aggoffset$funestus,
  subset(pred_multisp_pp_with_offset, "funestus_complex") * aggoffset$funestus,
  subset(pred_multisp_pp_with_offset, "gambiae") * aggoffset$gambiae,
  subset(pred_multisp_pp_with_offset, "gambiae_coluzzii") * aggoffset$gambiae,
  subset(pred_multisp_pp_with_offset, "gambiae_complex") * aggoffset$gambiae,
  subset(pred_multisp_pp_with_offset, "melas") * aggoffset$melas,
  subset(pred_multisp_pp_with_offset, "merus") * aggoffset$merus,
  subset(pred_multisp_pp_with_offset, "moucheti") * aggoffset$moucheti,
  subset(pred_multisp_pp_with_offset, "nili") * aggoffset$nili
)


sapply(
  X = names(expert_offset_preds_mspp),
  FUN = function(x, expert_offset_preds_mspp){
    distplot(expert_offset_preds_mspp, x)
    ggsave(
      filename = sprintf(
        "outputs/figures/distribution_plots/%s/distribution_%s_%s.png",
        "expert_offset_preds_mspp",
        "expert_offset_preds_mspp",
        x
      ),
      width = 3200,
      height = 2000,
      units = "px"
    )
  },
  expert_offset_preds_mspp
)
