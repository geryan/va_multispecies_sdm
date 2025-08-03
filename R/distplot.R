distplot <- function(
    rst,
    sp,
    colscheme = c("mako", "rb")
){

  colscheme <- match.arg(colscheme)

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
