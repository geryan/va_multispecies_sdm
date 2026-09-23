# 3 x 6 panel of the 18 species lambda surfaces for Uganda, one shared fill scale.

library(terra)
library(ggplot2)
library(tidyterra)

lambda_uganda <- rast("outputs/raster/lambda_uganda_20260911.tif")

  geom_spatraster(
    data = lambda_uganda
  ) +
  facet_wrap(
    ~lyr,
    ncol = 3
  ) +
  scale_fill_gradient(
    low = "grey95",
    high = "black",
    na.value = "transparent",
    #trans = "sqrt",
    name = expression(lambda)
  ) +
  theme_void() +
  theme(
    plot.background = element_rect(
      fill = "white",
      colour = NA
    ),
    strip.text = element_text(
      face = "italic",
      size = 9,
      margin = margin(b = 2)
    ),
    legend.position = "right"
  )

lambda_uganda_plot


ggsave(
  filename = "outputs/figures/lambda_uganda_20260911.png",
  plot = lambda_uganda_plot,
  width = 7,
  height = 14,
  dpi = 300,
  bg = "white"
)
