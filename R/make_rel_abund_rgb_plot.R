make_rel_abund_rgb_plot <- function(
    rel_abund_rgb,
    project_mask,
    filename,
    width = 3200,
    height = 3200,
    res = 300
) {

  colz <- rel_abund_rgb |>
    RGB(value = 1:3) |>
    colorize(
    to = "col"
  )

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
