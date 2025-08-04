distplot <- function(
    rst,
    sp,
    colscheme = c(
      "mako",
      "rb",
      "magma",
      "rocket"
    ),
    guide = c(
      "none",
      "prob"
    )
){

  colscheme <- match.arg(colscheme)

  guide <- match.arg(guide)

  spname <- paste0("Anopheles ", sp)

  r <- subset(rst, sp)

  p <- ggplot() +
    geom_spatraster(
      data = r
    ) +
    theme_void() +
    labs(
      title = bquote(italic(.(spname)))
    )

  p <- switch(
    colscheme,
    mako = p  +
      scale_fill_viridis_c(
        option = "G",
        direction = -1,
        na.value = "transparent"
      ),
    rb = p +
      scale_colour_distiller(
        palette = "RdBu",
        type = "div",
        aesthetics = c("fill"),
        na.value = "transparent"
      ),
    magma = p +
      scale_fill_viridis_c(
        option = "A",
        direction = -1,
        na.value = "transparent"
      ),
    rocket = p +
      scale_fill_viridis_c(
        option = "F",
        direction = -1,
        na.value = "transparent"
      )
  )

  p <- switch(
    guide,
    none = p +
      guides(
        fill = "none"
      ),
    prob = p +
      labs(
        fill = "Prob. of\noccurrence"
      )
  )

  p

}
