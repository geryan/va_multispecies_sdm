make_rel_abund_rgb <- function(
  x,
  threshold = NULL
){

  # subset to target species
  rgb_target_species <- c("arabiensis", "gambiae_coluzzii", "funestus")
  target_x <- x[[match(rgb_target_species, names(x))]]

  # convert to abundance by inverting cloglog link, then exponentiating
  # p = 1 - exp(-exp(x))
  # lambda = exp(x)
  # p = 1 - exp(-lambda)
  # exp(-lambda) = 1 - p
  # lambda = -log(1 - p)

  target_x_pa <- app(target_x, function(x) pmin(x, 0.999))
  target_x_abund <- -log(1 - target_x_pa)

  totals <- app(target_x_abund, sum)
  props <- target_x_abund / totals

  most <- global(totals, "max", na.rm = TRUE)$max
  totals_norm <- totals / most
  names(totals_norm) <- "transparency"
  plot(totals_norm)

  relabund <- c(props, trans = 1 - totals_norm) * 255

  if(!is.null(threshold)){
     allsp_mask <- app(target_x_pa, function(x) {
      all(x > threshold)
    }) |>
       app(
         fun = function(x){ ifelse(x == 0, NA, 1)}
       )

     relabund <- mask(
       x = relabund,
       mask = allsp_mask
     )
  }

  # geotargets doesn't currently facilitate saving color tables
  # so do this later when plotting
  #terra::RGB(relabund, value = 1:3)

  relabund

}
