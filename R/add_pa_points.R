add_pa_points <- function(
    p,
    point_data,
    colp = "deeppink",
    cola = "grey80",
    guide = c(
      "none",
      "detected"
    )
){

  guide <- match.arg(guide)

  p <- p +
    geom_point(
      data = point_data,
      aes(
        x = longitude,
        y = latitude,
        #shape = data_type,
        col = detected
      ),
      alpha = 0.7,
      size = 0.4
    )

  p <- switch(
    guide,
    none = p +
      scale_colour_manual(
        values = c(colp, cola),
        guide = "none"
      ),
    detected = p +
      scale_colour_manual(
        values = c(colp, cola),
        guide = guide_legend(title = "Was species\ndetected?")
      )
  )


  p

}
