make_rel_abund_rgb_plot <- function(
    rel_abund_rgb,
    project_mask,
    filename,
    width = 3200,
    height = 3200,
    res = 300
) {


  prgb <- RGB(rel_abund_rgb, value = 1:3)

  colz <- colorize(prgb, to = "col")

  png(
    filename = filename,
    width = width,
    height = height,
    res = res,
    units = "px"
  )

  plot(
    x = project_mask,
    col = "grey80",
    legend = FALSE,
    axes = FALSE,
    box = FALSE
  )

  plot(
    colz,
    add = TRUE
  )

  dev.off()

  NULL

}
