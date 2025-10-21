ppcsave <- function(x, y, z){

  ggsave(
    filename = sprintf(
      "%s_%s_%s.png",
      x,
      y,
      z
    ),
    width = 1500,
    height = 1000,
    dpi = 200,
    units = "px"
  )

}
