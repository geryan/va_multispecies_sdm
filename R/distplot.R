distplot <- function(
    rst,
    sp,
    colscheme = c(
      "va",
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

  limits <- switch(guide,
                   prob = c(0, 1),
                   none = NULL)

  p <- switch(
    colscheme,
    va = p  +
      scale_fill_gradient(
        low = grey(0.9),
        high = "navy",
        na.value = "transparent",
        limits = limits
      ),
    mako = p  +
      scale_fill_viridis_c(
        option = "G",
        direction = -1,
        na.value = "transparent",
        limits = limits
      ),
    rb = p +
      scale_colour_distiller(
        palette = "RdBu",
        type = "div",
        aesthetics = c("fill"),
        na.value = "transparent",
        limits = limits
      ),
    magma = p +
      scale_fill_viridis_c(
        option = "A",
        direction = -1,
        na.value = "transparent",
        limits = limits
      ),
    rocket = p +
      scale_fill_viridis_c(
        option = "F",
        direction = -1,
        na.value = "transparent",
        limits = limits
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
