distplot <- function(
    rst,
    sp,
    colscheme = c(
      "mako",
      "rb",
      "magma",
      "rocket",
      "mono",
      "orchid",
      "brick"
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
      ),
    mono = p +
      scale_id_continuous(
        cols = c("grey97", "black")
      ),
    orchid = p +
      scale_id_continuous(
        cols = c("grey97", "darkorchid")
      ),
    brick = p +
      scale_id_continuous(
        cols = c("grey97", "firebrick")
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
